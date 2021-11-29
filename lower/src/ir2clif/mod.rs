mod decl;
mod expr;
mod name;
mod stmt;
mod ty;

use self::{decl::lower_simple_decl, name::mangle_name, stmt::lower_block_stmt};
use crate::ir2clif::ty::convert_type;
use anyhow::Result;
use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::{AbiParam, Configurable, Signature, StackSlotData, StackSlotKind},
};
use cranelift_codegen::{
    binemit::{NullStackMapSink, NullTrapSink},
    ir::StackSlot,
    isa::{self, CallConv},
    settings, Context as ClifContext,
};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use id_arena::Id;
use long_ir::decl::{Decl, FuncDef, Local};
use rustc_hash::FxHashMap as HashMap;

pub struct LowerCtx {
    pub clif_ctx: ClifContext,
    pub data_ctx: DataContext,
    pub module: ObjectModule,
}

pub struct FuncLowerCtx<'a, 'b: 'a> {
    pub builder: &'a mut FunctionBuilder<'b>,
    pub locals: HashMap<Id<Local<'a>>, StackSlot>,
    pub func: &'a FuncDef<'a>,
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
            data_ctx: DataContext::new(),
            module,
        }
    }
}

pub fn lower_decl(ctx: &mut LowerCtx, decl: &Decl) -> Result<()> {
    match decl {
        Decl::FuncDef(funcdef) => lower_funcdef(ctx, funcdef),
        Decl::SimpleDecl(decls) => {
            for decl in decls {
                lower_simple_decl(ctx, decl)?;
            }
            Ok(())
        }
    }
}

pub fn lower_funcdef(ctx: &mut LowerCtx, funcdef: &FuncDef<'_>) -> Result<()> {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns
        .push(AbiParam::new(convert_type(funcdef.sig.ret)));
    ctx.clif_ctx.func.signature = sig;

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.clif_ctx.func, &mut fn_builder_ctx);
    {
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let mut locals = HashMap::default();
        for (id, local) in funcdef.locals.iter() {
            let ty = convert_type(local.ty);
            let slot = builder
                .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ty.bytes()));
            locals.insert(id, slot);
        }
        lower_block_stmt(
            &mut FuncLowerCtx {
                builder: &mut builder,
                locals,
                func: funcdef,
            },
            &funcdef.body,
        )?;
    }
    builder.finalize();

    let name = mangle_name(funcdef.name);
    let func_id = ctx.module.declare_function(
        name.as_str(),
        Linkage::Export,
        &ctx.clif_ctx.func.signature,
    )?;
    ctx.module.define_function(
        func_id,
        &mut ctx.clif_ctx,
        &mut NullTrapSink {},
        &mut NullStackMapSink {},
    )?;
    ctx.module.clear_context(&mut ctx.clif_ctx);

    Ok(())
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
            _ => todo!(),
        },
    );
    let product = ctx.module.finish();
    let obj = product.emit().unwrap();
    insta::assert_debug_snapshot!(obj);
}

#[test]
fn parse_and_lower_to_clif2() {
    use crate::ast2ir;
    use long_ast::node::Located;
    use long_ir::Context as IrContext;
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int i;"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let ctx = IrContext::new();
    let mut ctx = ast2ir::LowerCtx::new(&ctx);
    let decl = ast2ir::lower_decl(&mut ctx, &inner).unwrap();
    let mut ctx = LowerCtx::new();
    let _clif_func = lower_decl(&mut ctx, decl);
    let product = ctx.module.finish();
    let obj = product.emit().unwrap();
    insta::assert_debug_snapshot!(obj);
}
