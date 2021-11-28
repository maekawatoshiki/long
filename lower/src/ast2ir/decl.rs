use super::{expr::lower_expr, name::resolve_declarator_id, ty::resolve_type, LowerCtx};
use anyhow::Result;
use long_ast::node::decl::SimpleDecl as AstSimpleDecl;
use long_ir::decl::SimpleDecl as IrSimpleDecl;

pub fn lower_simple_decl<'a>(
    ctx: &mut LowerCtx<'a>,
    decl: &AstSimpleDecl,
) -> Result<IrSimpleDecl<'a>> {
    Ok(IrSimpleDecl {
        name: resolve_declarator_id(ctx, &decl.name)?,
        ty: resolve_type(ctx, &decl.ty)?,
        init: if let Some(init) = &decl.init {
            Some(lower_expr(ctx, init)?)
        } else {
            None
        },
    })
}
