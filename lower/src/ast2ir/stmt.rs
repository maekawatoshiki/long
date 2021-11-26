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
            Stmt::Return(expr) => lower_return(ctx, &mut seq, expr)?,
            _ => todo!(),
        };
    }
    Ok(SimpleBlock(seq))
}

fn lower_return(
    ctx: &mut Context,
    seq: &mut Vec<InstOrBlock>,
    expr: Option<Located<Expr>>,
) -> Result<InstId> {
    if expr.is_none() {
        let id = ctx.module.inst_arena.alloc(Inst::Return(None));
        seq.push(InstOrBlock::Inst(id));
        return Ok(id);
    }

    let expr = expr.unwrap();
    let val = lower_expr(ctx, seq, &expr.inner);
    let id = ctx.module.inst_arena.alloc(Inst::Return(Some(val)));
    seq.push(InstOrBlock::Inst(id));
    Ok(id)
}
