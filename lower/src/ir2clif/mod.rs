mod expr;
mod stmt;

use self::stmt::lower_block_stmt;
use anyhow::Result;
use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::{AbiParam, Configurable, Signature},
};
use cranelift_codegen::{
    binemit::{NullStackMapSink, NullTrapSink},
    ir::types as clif_ty,
    isa::{self, CallConv},
    settings, Context as ClifContext,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use long_ir::{decl::FuncDef, name::Name, ty as ir_ty};

pub struct LowerCtx {
    pub clif_ctx: ClifContext,
    pub module: ObjectModule,
}

impl LowerCtx {
    /// Creates a new `LowerCtx`.
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();
        let isa_builder = isa::lookup_by_name("x86_64-unknown-unknown-elf").unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));

        let builder = ObjectBuilder::new(
            isa,
            "".to_owned(), // TODO: This will be embedded in the object file.
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let module = ObjectModule::new(builder);

        Self {
            clif_ctx: module.make_context(),
            module,
        }
    }
}

pub fn lower_funcdef(ctx: &mut LowerCtx, funcdef: &FuncDef<'_>) -> Result<()> {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns
        .push(AbiParam::new(convert_type(ctx, funcdef.sig.ret)));
    ctx.clif_ctx.func.signature = sig;

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.clif_ctx.func, &mut fn_builder_ctx);
    {
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        lower_block_stmt(&mut builder, &funcdef.body)?;
    }
    builder.finalize();

    let name = mangle_name(funcdef.name);
    let func_id = ctx
        .module
        .declare_function(name.as_str(), Linkage::Export, &ctx.clif_ctx.func.signature)
        .unwrap();
    ctx.module
        .define_function(
            func_id,
            &mut ctx.clif_ctx,
            &mut NullTrapSink {},
            &mut NullStackMapSink {},
        )
        .unwrap();
    ctx.module.clear_context(&mut ctx.clif_ctx);

    Ok(())
}

fn convert_type(_ctx: &LowerCtx, from: &ir_ty::Type) -> clif_ty::Type {
    match from {
        ir_ty::Type::Void => clif_ty::I8,
        ir_ty::Type::Int(_) => clif_ty::I32,
        _ => todo!(),
    }
}

fn mangle_name(name: &Name) -> String {
    // TODO: Names not mangled yet.
    format!("{}", name).trim_start_matches("::").to_owned()
}

#[test]
fn parse_and_lower_to_clif() {
    use crate::ast2ir;
    use long_ast::node::Located;
    use long_ir::decl::Decl;
    use long_ir::Context as IrContext;
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int main() { return 42; }"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let ctx = IrContext::new();
    let mut ctx = ast2ir::LowerCtx::new(&ctx);
    let decl = ast2ir::lower_decl(&mut ctx, &inner).unwrap();
    let mut ctx = LowerCtx::new();
    let _clif_func = lower_funcdef(
        &mut ctx,
        match decl {
            Decl::FuncDef(funcdef) => funcdef,
        },
    );
    let product = ctx.module.finish();
    let obj = product.emit().unwrap();
    insta::assert_debug_snapshot!(obj);
}
