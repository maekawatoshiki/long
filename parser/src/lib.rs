extern crate anyhow;
extern crate long_ast as ast;
extern crate long_lexer as lexer;

mod expr;

use anyhow::Result;
use ast::node::expr::Expr;
use lexer::traits::LexerLike;

/// A parser for C++.
pub struct Parser<'a, L: LexerLike> {
    pub lexer: &'a mut L,
}

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Create a new `Parser`.
    pub fn new(lexer: &'a mut L) -> Parser<'a, L> {
        Parser { lexer }
    }

    /// Parses an expression.
    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_comma()
    }
}

#[test]
#[should_panic]
fn parse() {
    use lexer::Lexer;
    let _ = Parser::new(&mut Lexer::new("1+2+3")).parse_expr();
}
