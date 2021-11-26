use crate::ast2ir::{expr::lower_expr, Context};
use anyhow::Result;
use long_ast::node::{
    expr::Expr,
    stmt::{BlockStmt, Stmt},
    Located,
};
use long_ir::{
    block::{InstOrBlock, SimpleBlock},
    inst::{Inst, InstId},
};

pub(crate) fn lower_block(ctx: &mut Context, BlockStmt(stmts): BlockStmt) -> Result<SimpleBlock> {
    let mut seq = vec![];
    for Located {
        inner: stmt,
        loc: _, // TODO: We should not ignore this.
    } in stmts
    {
        match stmt {
            Stmt::Return(expr) => seq.push(InstOrBlock::Inst(lower_return(ctx, expr)?)),
            _ => todo!(),
        }
    }
    Ok(SimpleBlock(seq))
}

fn lower_return(ctx: &mut Context, expr: Option<Located<Expr>>) -> Result<InstId> {
    if expr.is_none() {
        return Ok(ctx.module.inst_arena.alloc(Inst::Return(None)));
    }

    let expr = expr.unwrap();
    let val = lower_expr(ctx, &expr.inner);
    Ok(ctx.module.inst_arena.alloc(Inst::Return(Some(val))))
}
