use super::{expr::Expr, Located};

/// A statement kind.
#[derive(Debug, Clone)]
pub enum Stmt {
    Block(BlockStmt),
    Return(Option<Located<Expr>>),
}

/// A block statement.
#[derive(Debug, Clone)]
pub struct BlockStmt(pub Vec<Located<Stmt>>);
