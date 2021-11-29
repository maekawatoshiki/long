use crate::expr::Expr;
use long_ast::node::Located;

/// A statement kind.
#[derive(Debug)]
pub enum Stmt<'a> {
    Expr(Located<&'a Expr<'a>>),
    Block(BlockStmt<'a>),
    Return(Option<Located<&'a Expr<'a>>>),
}

/// A block statement.
#[derive(Debug)]
pub struct BlockStmt<'a>(pub Vec<Located<&'a Stmt<'a>>>);
