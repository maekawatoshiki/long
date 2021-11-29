use crate::ir2clif::FuncLowerCtx;
use anyhow::Result;
use cranelift::prelude::{InstBuilder, Value};
use cranelift_codegen::ir::types as clif_ty;
use long_ast::{node::expr::AssignOp, token::kind::IntKind};
use long_ir::expr::{Expr, Literal};

pub fn lower_expr(ctx: &mut FuncLowerCtx, expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Literal(Literal::Int(IntKind::Int(i))) => {
            Ok(ctx.builder.ins().iconst(clif_ty::I32, *i as i64))
        }
        Expr::Assign(AssignOp::None, lhs, rhs) => {
            todo!()
        }
        _ => todo!(),
    }
}

fn lvalue(ctx: &mut FuncLowerCtx, expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Literal(Literal::Local(id)) => {
            todo!()
        }
        _ => todo!(),
    }
}
