use anyhow::Result;
use cranelift::{frontend::FunctionBuilder, prelude::InstBuilder};
use cranelift_codegen::ir::types as clif_ty;
use long_ast::node::Located;
use long_ir::stmt::{BlockStmt, Stmt};

use crate::ir2clif::expr::lower_expr;

pub fn lower_stmt(builder: &mut FunctionBuilder, stmt: &Stmt) -> Result<()> {
    match stmt {
        Stmt::Block(_) => todo!(),
        Stmt::Expr(e) => {
            let _expr = lower_expr(builder, &e.inner)?;
        }
        Stmt::Return(None) => {
            let ret = builder.ins().iconst(clif_ty::I32, 0);
            builder.ins().return_(&[ret]);
        }
        Stmt::Return(Some(Located {
            inner: expr,
            loc: _,
        })) => {
            let ret = lower_expr(builder, expr)?;
            builder.ins().return_(&[ret]);
        }
    };
    Ok(())
}

pub fn lower_block_stmt<'a>(builder: &mut FunctionBuilder, block: &BlockStmt<'a>) -> Result<()> {
    for stmt in block.0.iter() {
        lower_stmt(builder, stmt.inner)?;
    }
    Ok(())
}
