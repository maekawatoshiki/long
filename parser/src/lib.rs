extern crate anyhow;
extern crate long_ast as ast;
extern crate long_sourceloc as sourceloc;

mod decl;
mod expr;
pub mod lexer;
mod stmt;

use anyhow::Result;
use ast::{
    node::{decl::Decl, expr::Expr, Located},
    token::kind::TokenKind,
};
use lexer::{traits::LexerLike, Error};

/// A parser for C++.
pub struct Parser<'a, L: LexerLike> {
    pub lexer: &'a mut L,
}

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Create a new `Parser`.
    pub fn new(lexer: &'a mut L) -> Parser<'a, L> {
        Parser { lexer }
    }

    /// Parses a program.
    pub fn parse_program(&mut self) -> Result<Located<Decl>> {
        self.parse_decl_seq()
    }

    /// Parses an expression.
    pub fn parse_expr(&mut self) -> Result<Located<Expr>> {
        self.parse_comma()
    }

    fn expect(&mut self, kind: impl Into<TokenKind>) -> Result<()> {
        let loc = *self.lexer.peek()?.ok_or(Error::UnexpectedEof)?.loc();
        if self.lexer.skip(kind.into()) {
            Ok(())
        } else {
            Err(Error::Unexpected(loc).into())
        }
    }
}
