use crate::ast2ir::expr::lower_expr;

use super::LowerCtx;
use anyhow::Result;
use long_ast::node::{
    stmt::{BlockStmt as AstBlockStmt, Stmt as AstStmt},
    Located,
};
use long_ir::stmt::{BlockStmt as IrBlockStmt, Stmt as IrStmt};

pub fn lower_stmt<'a>(ctx: &mut LowerCtx<'a>, stmt: &AstStmt) -> Result<&'a IrStmt<'a>> {
    match stmt {
        AstStmt::Expr(_) => todo!(),
        AstStmt::Block(_) => todo!(),
        AstStmt::Return(None) => Ok(ctx.ir_ctx.stmt_arena.alloc(IrStmt::Return(None))),
        AstStmt::Return(Some(Located { inner: e, loc })) => {
            let e = lower_expr(ctx, e)?;
            Ok(ctx
                .ir_ctx
                .stmt_arena
                .alloc(IrStmt::Return(Some(Located::new(e, *loc)))))
        }
    }
}

pub fn lower_block_stmt<'a>(
    ctx: &mut LowerCtx<'a>,
    stmt: &AstBlockStmt,
) -> Result<IrBlockStmt<'a>> {
    let mut stmts = Vec::new();
    for Located { inner, loc } in stmt.0.iter() {
        stmts.push(Located::new(lower_stmt(ctx, inner)?, *loc));
    }
    Ok(IrBlockStmt(stmts))
}
