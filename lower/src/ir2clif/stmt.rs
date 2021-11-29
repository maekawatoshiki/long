use crate::ir2clif::{expr::lower_expr, FuncLowerCtx};
use anyhow::Result;
use cranelift::prelude::InstBuilder;
use cranelift_codegen::ir::types as clif_ty;
use long_ast::node::Located;
use long_ir::stmt::{BlockStmt, Stmt};

pub fn lower_stmt(ctx: &mut FuncLowerCtx, stmt: &Stmt) -> Result<()> {
    match stmt {
        Stmt::Block(_) => todo!(),
        Stmt::Expr(e) => {
            let _expr = lower_expr(ctx, &e.inner)?;
        }
        Stmt::Return(None) => {
            let ret = ctx.builder.ins().iconst(clif_ty::I32, 0);
            ctx.builder.ins().return_(&[ret]);
        }
        Stmt::Return(Some(Located {
            inner: expr,
            loc: _,
        })) => {
            let ret = lower_expr(ctx, expr)?;
            ctx.builder.ins().return_(&[ret]);
        }
    };
    Ok(())
}

pub fn lower_block_stmt<'a>(
    ctx: &'a mut FuncLowerCtx<'a, '_>,
    block: &'a BlockStmt<'a>,
) -> Result<()> {
    for stmt in block.0.iter() {
        lower_stmt(ctx, stmt.inner)?;
    }
    Ok(())
}
