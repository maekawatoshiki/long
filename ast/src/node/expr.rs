use super::lit::Literal;
use long_sourceloc::SourceLoc;

/// An expression node.
#[derive(Debug, Clone)]
pub struct Expr {
    /// The kind of the expression.
    pub(crate) kind: ExprKind,

    /// The source location of the expression.
    pub(crate) loc: SourceLoc,
}

impl Expr {
    /// Creates a new `Expr`.
    pub fn new(kind: ExprKind, loc: SourceLoc) -> Self {
        Self { kind, loc }
    }

    /// Returns the kind of the token.
    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    /// Returns the source location of the token.
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }
}

/// An expression kind.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A literal expression.
    Literal(Literal),

    /// A binary expression.
    Binary(BinOp, Box<Expr>, Box<Expr>),

    /// A ternary expression.
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>), // cond, then, else.
}

/// A binary operator node.
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Comma,
    LogicalOr,
    LogicalAnd,
}
