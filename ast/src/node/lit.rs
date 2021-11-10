use long_lexer::token::kind::IntKind as Int;

/// A literal node.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(Int),
}
