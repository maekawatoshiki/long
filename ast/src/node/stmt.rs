use super::Located;

/// A statement kind.
#[derive(Debug, Clone)]
pub enum Stmt {
    Block(BlockStmt),
}

/// A block statement.
#[derive(Debug, Clone)]
pub struct BlockStmt(pub Vec<Located<Stmt>>);
