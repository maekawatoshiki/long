use long_ast::node::Located;

use crate::expr::Expr;

/// A statement kind.
pub enum Stmt<'a> {
    Block(BlockStmt<'a>),
    Return(Option<Located<&'a Expr<'a>>>),
}

/// A block statement.
pub struct BlockStmt<'a>(pub Vec<Located<&'a Stmt<'a>>>);
