extern crate long_lexer as lexer;

/// A parser for C++.
pub struct Parser<'a, L> {
    pub lexer: &'a mut L,
}
