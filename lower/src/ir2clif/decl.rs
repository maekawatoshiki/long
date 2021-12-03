use super::stmt::lower_block_stmt;
use super::{name::mangle_name, LowerCtx};
use crate::ir2clif::ty::convert_type;
use anyhow::Result;
use cranelift::{
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::{AbiParam, Signature, StackSlotData, StackSlotKind},
};
use cranelift_codegen::{
    binemit::{NullStackMapSink, NullTrapSink},
    ir::StackSlot,
    isa::CallConv,
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
