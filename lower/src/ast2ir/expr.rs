use long_ast::{
    node::{
        expr::{BinOp, Expr},
        lit::Literal,
    },
    token::kind::IntKind,
};
use long_ir::{
    block::InstOrBlock,
    inst::{Inst, InstId},
    value::{Value, ValueId},
};

use super::Context;

pub(crate) fn lower_expr(ctx: &mut Context, seq: &mut Vec<InstOrBlock>, expr: &Expr) -> InstId {
    match expr {
        Expr::Binary(BinOp::Add, lhs, rhs) => {
            let lhs = lower_expr(ctx, seq, &lhs.inner);
            let rhs = lower_expr(ctx, seq, &rhs.inner);
            let inst = ctx.module.inst_arena.alloc(Inst::Add(lhs, rhs));
            seq.push(InstOrBlock::Inst(inst));
            inst
        }
        Expr::Literal(Literal::Int(IntKind::Int(i))) => ctx.module.inst_arena.alloc(Inst::Int(*i)),
        _ => todo!(),
    }
}
