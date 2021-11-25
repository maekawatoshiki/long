use crate::ast2ir::Context;
use anyhow::Result;
use long_ast::{
    node::{
        expr::Expr,
        lit::Literal,
        stmt::{BlockStmt, Stmt},
        Located,
    },
    token::kind::IntKind,
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

    // TODO: Implement lower_expr().
    assert!(matches!(
        expr.unwrap().inner,
        Expr::Literal(Literal::Int(IntKind::Int(0)))
    ));

    let val = ctx.module.val_arena.new_int(0);
    Ok(ctx.module.inst_arena.alloc(Inst::Return(Some(val))))
}
