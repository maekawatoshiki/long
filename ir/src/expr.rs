use id_arena::Id;
use long_ast::{
    node::{
        expr::{AssignOp, BinOp, UnaryOp},
        Located,
    },
    token::kind::IntKind,
};

use crate::decl::Local;

/// An expression kind.
#[derive(Debug)]
pub enum Expr<'a> {
    /// A literal expression.
    Literal(Literal<'a>),

    /// A unary expression.
    Unary(UnaryOp, Located<&'a Expr<'a>>),

    /// A binary expression.
    Binary(BinOp, Located<&'a Expr<'a>>, Located<&'a Expr<'a>>),

    /// A ternary expression.
    Ternary(
        Located<&'a Expr<'a>>,
        Located<&'a Expr<'a>>,
        Located<&'a Expr<'a>>,
    ), // cond, then, else.

    /// An Assign expression.
    Assign(AssignOp, Located<&'a Expr<'a>>, Located<&'a Expr<'a>>),
}

/// A literal node.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    Int(IntKind),
    Local(Id<Local<'a>>),
}
