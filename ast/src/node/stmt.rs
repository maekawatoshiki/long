use super::{decl::SimpleDecl, expr::Expr, Located};

/// A statement kind.
#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Located<Expr>),
    Block(BlockStmt),
    SimpleDecl(Vec<SimpleDecl>),
    Return(Option<Located<Expr>>),
}

/// A block statement.
#[derive(Debug, Clone)]
pub struct BlockStmt(pub Vec<Located<Stmt>>);
