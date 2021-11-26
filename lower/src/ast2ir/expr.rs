use long_ast::{
    node::{expr::Expr, lit::Literal},
    token::kind::IntKind,
};
use long_ir::value::ValueId;

use super::Context;

pub(crate) fn lower_expr(ctx: &mut Context, expr: &Expr) -> ValueId {
    match expr {
        Expr::Literal(Literal::Int(IntKind::Int(i))) => ctx.module.val_arena.new_int(*i),
        _ => todo!(),
    }
}
