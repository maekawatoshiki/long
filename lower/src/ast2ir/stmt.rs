use crate::ast2ir::{expr::lower_expr, ty::resolve_type};

use super::LowerCtx;
use anyhow::Result;
use long_ast::node::{
    decl::DeclaratorId,
    stmt::{BlockStmt as AstBlockStmt, Stmt as AstStmt},
    Located,
};
use long_ir::{
    decl::Local,
    stmt::{BlockStmt as IrBlockStmt, Stmt as IrStmt},
};

pub fn lower_stmt<'a>(ctx: &mut LowerCtx<'a>, stmt: &AstStmt) -> Result<Option<&'a IrStmt<'a>>> {
    match stmt {
        AstStmt::Expr(Located { inner: expr, loc }) => {
            let e = lower_expr(ctx, expr)?;
            let stmt = ctx
                .ir_ctx
                .stmt_arena
                .alloc(IrStmt::Expr(Located::new(e, *loc)));
            Ok(Some(stmt))
        }
        AstStmt::Block(_) => todo!(),
        AstStmt::SimpleDecl(decls) => {
            for decl in decls {
                let name = match &decl.name {
                    DeclaratorId::Ident(ident) => ident,
                };
                let ty = resolve_type(ctx, &decl.ty)?;
                let id = ctx.locals.alloc(Local {
                    name: name.into(),
                    ty,
                });
                ctx.envs.add_to_cur_env(name, ty);
                ctx.envs.add_local_to_cur_env(name, id);
            }
            Ok(None)
        }
        AstStmt::Return(None) => Ok(Some(ctx.ir_ctx.stmt_arena.alloc(IrStmt::Return(None)))),
        AstStmt::Return(Some(Located { inner: e, loc })) => {
            let e = lower_expr(ctx, e)?;
            Ok(Some(
                ctx.ir_ctx
                    .stmt_arena
                    .alloc(IrStmt::Return(Some(Located::new(e, *loc)))),
            ))
        }
    }
}

pub fn lower_block_stmt<'a>(
    ctx: &mut LowerCtx<'a>,
    stmt: &AstBlockStmt,
) -> Result<IrBlockStmt<'a>> {
    let mut stmts = Vec::new();
    for Located { inner, loc } in stmt.0.iter() {
        if let Some(stmt) = lower_stmt(ctx, inner)? {
            stmts.push(Located::new(stmt, *loc));
        }
    }
    Ok(IrBlockStmt(stmts))
}
