use super::lit::Literal;

/// An expression node.
#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(BinOp),
    Literal(Literal),
}

/// A binary operator node.
#[derive(Debug, Clone)]
pub enum BinOp {
    Comma(Box<Expr>, Box<Expr>),
}
