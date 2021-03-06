use super::stmt::lower_block_stmt;
use super::{name::mangle_name, LowerCtx};
use crate::ir2clif::ty::convert_type;
use anyhow::Result;
use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::{AbiParam, Signature, StackSlotData, StackSlotKind},
};
use cranelift_codegen::ir::Function;
use cranelift_codegen::{
    binemit::{NullStackMapSink, NullTrapSink},
    ir::StackSlot,
};
use cranelift_module::{Linkage, Module};
use id_arena::Id;
use long_ir::decl::{Decl, FuncDef, Local, SimpleDecl};
use rustc_hash::FxHashMap as HashMap;

pub struct FuncLowerCtx<'a, 'b: 'a> {
    pub builder: &'a mut FunctionBuilder<'b>,
    pub locals: HashMap<Id<Local<'a>>, StackSlot>,
    pub func: &'a FuncDef<'a>,
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

pub fn lower_simple_decl(ctx: &mut LowerCtx, decl: &SimpleDecl) -> Result<()> {
    ctx.data_ctx.define_zeroinit(4); // TODO: Type size?
    let id = ctx.module.declare_data(
        mangle_name(decl.name).as_str(),
        Linkage::Export,
        true,
        false,
    )?;
    ctx.module.define_data(id, &ctx.data_ctx)?;
    ctx.data_ctx.clear();
    Ok(())
}

pub fn lower_funcdef(ctx: &mut LowerCtx, funcdef: &FuncDef<'_>) -> Result<()> {
    lower_funcdef_sub(ctx, funcdef)?;

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

pub fn lower_funcdef_sub<'a>(
    ctx: &'a mut LowerCtx,
    funcdef: &FuncDef<'_>,
) -> Result<&'a mut Function> {
    let mut sig = Signature::new(ctx.module.isa().default_call_conv());
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

    Ok(&mut ctx.clif_ctx.func)
}

macro_rules! lower_func {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            use crate::ast2ir;
            use cranelift_codegen::isa::CallConv;
            use long_ast::node::Located;
            use long_ir::decl::Decl;
            use long_ir::Context as IrContext;
            use long_parser::lexer::Lexer;
            use long_parser::Parser;
            let Located { inner, .. } = Parser::new(&mut Lexer::new($src))
                .parse_program()
                .unwrap()
                .into_iter()
                .next()
                .unwrap();
            let ctx = IrContext::new();
            let mut ctx = ast2ir::LowerCtx::new(&ctx);
            let decl = ast2ir::lower_decl(&mut ctx, &inner).unwrap();
            #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
            let mut ctx = LowerCtx::new("x86_64-unknown-unknown-elf");
            #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
            let mut ctx = LowerCtx::new("aarch64-apple-darwin");
            let clif_func = lower_funcdef_sub(
                &mut ctx,
                match decl {
                    Decl::FuncDef(funcdef) => funcdef,
                    _ => todo!(),
                },
            )
            .unwrap();
            // TODO: FIXME: Depending on OS and ISA, the calling convention may vary.
            // This makes it difficult to do testing using insta because the text representation
            // of a cranelift function contains the name of calling convention.
            // So we have to reset the calling convention here.
            clif_func.signature.call_conv = CallConv::Fast;
            insta::assert_display_snapshot!(clif_func.display());
        }
    };
}

lower_func!(arith, "int f() { return 1 + 2 - 3 * 4 / 5; }");
lower_func!(
    arith2,
    "int f() { int i; i = 1; i = i + 2; i = i - 3; i = i * 4; i = i / 5; return i; }"
);
