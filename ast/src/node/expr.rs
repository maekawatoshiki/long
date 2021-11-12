use super::lit::Literal;
use crate::token::kind::IntKind;
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

impl Expr {
    pub fn eval_constexpr(&self) -> Option<i64> {
        match self.kind {
            ExprKind::Literal(Literal::Int(IntKind::Int(i))) => Some(i as i64),
            ExprKind::Literal(Literal::Int(_)) => None,
            ExprKind::Binary(BinOp::Comma, _, ref rhs) => rhs.eval_constexpr(),
            ExprKind::Binary(BinOp::LogicalOr, ref lhs, ref rhs) => {
                if lhs.eval_constexpr() != Some(0) {
                    Some(1)
                } else {
                    Some((rhs.eval_constexpr() != Some(0)) as i64)
                }
            }
            ExprKind::Binary(BinOp::LogicalAnd, ref lhs, ref rhs) => {
                if lhs.eval_constexpr() == Some(0) {
                    Some(0)
                } else {
                    Some((rhs.eval_constexpr() != Some(0)) as i64)
                }
            }
            ExprKind::Ternary(ref cond, ref thn, ref els) => {
                if cond.eval_constexpr() != Some(0) {
                    thn.eval_constexpr()
                } else {
                    els.eval_constexpr()
                }
            }
        }
    }
}
