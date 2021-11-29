use super::LowerCtx;
use anyhow::Result;
use long_ast::{
    node::{
        expr::{AssignOp, Expr as AstExpr},
        lit::Literal as AstLiteral,
        Located,
    },
    token::kind::IntKind,
};
use long_ir::{
    expr::{Expr as IrExpr, Literal as IrLiteral},
    name::Name,
};

pub fn lower_expr<'a>(ctx: &mut LowerCtx<'a>, expr: &AstExpr) -> Result<&'a IrExpr<'a>> {
    match expr {
        AstExpr::Literal(AstLiteral::Int(IntKind::Int(i))) => Ok(ctx
            .ir_ctx
            .expr_arena
            .alloc(IrExpr::Literal(IrLiteral::Int(IntKind::Int(*i))))),
        AstExpr::Literal(AstLiteral::Ident(ident)) => {
            let name = ctx.ir_ctx.name_arena.alloc(Name::local(ident));
            let var = ctx.envs.lookup_local(name).expect("TODO");
            Ok(ctx
                .ir_ctx
                .expr_arena
                .alloc(IrExpr::Literal(IrLiteral::Local(var))))
        }
        AstExpr::Assign(AssignOp::None, lhs, rhs) => {
            let lhs = Located::new(lower_expr(ctx, &lhs.inner)?, lhs.loc());
            let rhs = Located::new(lower_expr(ctx, &rhs.inner)?, lhs.loc());
            Ok(ctx
                .ir_ctx
                .expr_arena
                .alloc(IrExpr::Assign(AssignOp::None, lhs, rhs)))
        }
        _ => todo!(),
    }
}
