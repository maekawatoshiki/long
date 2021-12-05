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
use long_ir::expr::{Expr as IrExpr, Literal as IrLiteral};

pub fn lower_expr<'a>(ctx: &mut LowerCtx<'a>, expr: &AstExpr) -> Result<&'a IrExpr<'a>> {
    match expr {
        AstExpr::Literal(AstLiteral::Int(IntKind::Int(i))) => Ok(ctx
            .ir_ctx
            .expr_arena
            .alloc(IrExpr::Literal(IrLiteral::Int(IntKind::Int(*i))))),
        AstExpr::Literal(AstLiteral::Ident(ident)) => {
            if let Some(local) = ctx.envs.lookup_local(ident) {
                Ok(ctx
                    .ir_ctx
                    .expr_arena
                    .alloc(IrExpr::Literal(IrLiteral::Local(local))))
            } else {
                ctx.envs.lookup(ident).map_or_else(
                    || {
                        todo!()
                        // Err(Error::Message(
                        //     format!("Undefined identifier: {}", ident),
                        //     loc,
                        // ))
                    },
                    |_ty| {
                        Ok(&*ctx
                            .ir_ctx
                            .expr_arena
                            .alloc(IrExpr::Literal(IrLiteral::Global(ident.to_owned()))))
                    },
                )
            }
        }
        AstExpr::Binary(op, lhs, rhs) => {
            let lhs = Located::new(lower_expr(ctx, &lhs.inner)?, lhs.loc());
            let rhs = Located::new(lower_expr(ctx, &rhs.inner)?, lhs.loc());
            Ok(ctx.ir_ctx.expr_arena.alloc(IrExpr::Binary(*op, lhs, rhs)))
        }
        AstExpr::Assign(AssignOp::None, lhs, rhs) => {
            let lhs = Located::new(lower_expr(ctx, &lhs.inner)?, lhs.loc());
            let rhs = Located::new(lower_expr(ctx, &rhs.inner)?, lhs.loc());
            Ok(ctx
                .ir_ctx
                .expr_arena
                .alloc(IrExpr::Assign(AssignOp::None, lhs, rhs)))
        }
        AstExpr::Call(callee, args) => {
            let callee = Located::new(lower_expr(ctx, &callee.inner)?, callee.loc());
            let mut new_args = vec![];
            for arg in args {
                new_args.push(Located::new(lower_expr(ctx, &arg.inner)?, arg.loc()));
            }
            Ok(ctx.ir_ctx.expr_arena.alloc(IrExpr::Call(callee, new_args)))
        }
        _ => todo!(),
    }
}
