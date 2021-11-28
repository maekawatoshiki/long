use super::{name::mangle_name, LowerCtx};
use anyhow::Result;
use cranelift_module::{Linkage, Module};
use long_ir::decl::SimpleDecl;

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
