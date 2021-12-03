use crate::ir2clif::{ty::convert_type, FuncLowerCtx};
use anyhow::Result;
use cranelift::prelude::{InstBuilder, Value};
use cranelift_codegen::ir::types as clif_ty;
use long_ast::{
    node::expr::{AssignOp, BinOp},
    token::kind::IntKind,
};
use long_ir::expr::{Expr, Literal};

pub fn lower_expr(ctx: &mut FuncLowerCtx, expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Literal(Literal::Int(IntKind::Int(i))) => {
            Ok(ctx.builder.ins().iconst(clif_ty::I32, *i as i64))
        }
        Expr::Literal(Literal::Local(id)) => {
            let ty = convert_type(ctx.func.locals.get(*id).unwrap().ty);
            let slot = *ctx.locals.get(id).unwrap();
            let val = ctx.builder.ins().stack_load(ty, slot, 0);
            Ok(val)
        }
        Expr::Binary(
            op @ BinOp::Add | op @ BinOp::Sub | op @ BinOp::Mul | op @ BinOp::Div,
            lhs,
            rhs,
        ) => {
            let lhs = lower_expr(ctx, &lhs.inner)?;
            let rhs = lower_expr(ctx, &rhs.inner)?;
            let val = match op {
                BinOp::Add => ctx.builder.ins().iadd(lhs, rhs),
                BinOp::Sub => ctx.builder.ins().isub(lhs, rhs),
                BinOp::Mul => ctx.builder.ins().imul(lhs, rhs),
                BinOp::Div => ctx.builder.ins().sdiv(lhs, rhs), // TODO
                _ => unreachable!(),
            };
            Ok(val)
        }
        Expr::Assign(AssignOp::None, lhs, rhs) => {
            let rhs = lower_expr(ctx, &rhs.inner)?;
            assign(ctx, &lhs.inner, rhs)
        }
        _ => todo!(),
    }
}

fn assign(ctx: &mut FuncLowerCtx<'_, '_>, lvalue: &Expr<'_>, rvalue: Value) -> Result<Value> {
    match lvalue {
        Expr::Literal(Literal::Local(id)) => {
            ctx.builder
                .ins()
                .stack_store(rvalue, *ctx.locals.get(id).unwrap(), 0);
            Ok(rvalue)
        }
        _ => todo!(),
    }
}
