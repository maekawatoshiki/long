use super::LowerCtx;
use anyhow::Result;
use long_ast::node::expr::Expr as AstExpr;
use long_ir::expr::Expr as IrExpr;

pub fn lower_expr<'a>(ctx: &mut LowerCtx<'a>, expr: &AstExpr) -> Result<&'a IrExpr<'a>> {
    match expr {
        AstExpr::Literal(lit) => Ok(ctx.ir_ctx.expr_arena.alloc(IrExpr::Literal(lit.to_owned()))),
        _ => todo!(),
    }
}
