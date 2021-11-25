use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::{AbiParam, Configurable, InstBuilder, Signature},
};
use cranelift_codegen::{
    binemit::{NullStackMapSink, NullTrapSink},
    ir::types as clif_ty,
    isa::{self, CallConv},
    settings, Context as ClifContext,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use long_ir::{func::Function as IrFunction, name::Name, ty as ir_ty, Module as IrModule};

/// A context used in the lowering process from IR to Cranelift IR.
pub struct Context {
    pub ir_ctx: IrModule,
    pub clif_ctx: ClifContext,
    pub module: ObjectModule,
}

impl Context {
    /// Creates a new `Context`.
    pub fn new(ir_ctx: IrModule) -> Self {
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
            ir_ctx,
            clif_ctx: module.make_context(),
            module,
        }
    }
}

/// Converts an IR function into a Cranelift function.
pub fn lower_function(ctx: &mut Context, f: IrFunction) {
    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns.push(AbiParam::new(conv_ty(f.sig.ret)));
    ctx.clif_ctx.func.signature = sig;

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.clif_ctx.func, &mut fn_builder_ctx);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let ret = builder.ins().iconst(clif_ty::I32, 0);
        builder.ins().return_(&[ret]);
        builder.finalize();
    }

    let name = mangle_name(ctx.ir_ctx.name_arena.get(f.name).unwrap());
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
}

fn conv_ty(from: ir_ty::Type) -> clif_ty::Type {
    match from {
        ir_ty::primitive::VOID => clif_ty::I8,
        ir_ty::primitive::SIGNED_INT | ir_ty::primitive::UNSIGNED_INT => clif_ty::I32,
        _ => todo!(),
    }
}

fn mangle_name(name: &Name) -> String {
    // TODO: Names not mangled yet.
    match name {
        Name::Global(names) => names.join("."),
        Name::Local(name) => name.to_owned(),
    }
}

#[test]
fn parse_and_lower_to_clif() {
    use crate::ast2ir;
    use long_ast::node::{decl::Decl, Located};
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int main() {}"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let mut module = IrModule::new();
    let mut ctx = ast2ir::Context::new(&mut module);
    let func_ir = ast2ir::lower_function(
        &mut ctx,
        match inner {
            Decl::FuncDef(funcdef) => funcdef,
        },
    )
    .unwrap();
    let mut ctx = Context::new(module);
    let _clif_func = lower_function(&mut ctx, func_ir);
    let product = ctx.module.finish();
    let obj = product.emit().unwrap();
    insta::assert_debug_snapshot!(obj);
}
