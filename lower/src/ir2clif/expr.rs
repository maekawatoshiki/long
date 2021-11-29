use anyhow::Result;
use cranelift::{
    frontend::FunctionBuilder,
    prelude::{InstBuilder, Value},
};
use cranelift_codegen::ir::types as clif_ty;
use long_ast::token::kind::IntKind;
use long_ir::expr::{Expr, Literal};

pub fn lower_expr(builder: &mut FunctionBuilder, expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Literal(Literal::Int(IntKind::Int(i))) => {
            Ok(builder.ins().iconst(clif_ty::I32, *i as i64))
        }
        _ => todo!(),
    }
}
