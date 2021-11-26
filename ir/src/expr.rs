use long_ast::node::{
    expr::{AssignOp, BinOp, UnaryOp},
    Located,
};

/// An expression kind.
pub enum Expr<'a> {
    // /// A literal expression.
    // Literal(Literal),
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
