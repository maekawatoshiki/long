use crate::{
    lexer::{traits::LexerLike, Error},
    Parser,
};
use anyhow::Result;
use long_ast::{
    node::{
        stmt::{BlockStmt, Stmt},
        Located,
    },
    token::kind::{SymbolKind, TokenKind},
};

impl<'a, L: LexerLike> Parser<'a, L> {
    pub(crate) fn parse_function_body(&mut self) -> Result<BlockStmt> {
        self.parse_block()
    }

    /// Parses a statement.
    pub(crate) fn parse_stmt(&mut self) -> Result<Located<Stmt>> {
        let tok = self.lexer.next()?.ok_or_else(|| Error::UnexpectedEof)?;
        match tok.kind {
            TokenKind::Symbol(SymbolKind::OpeningBrace) => self
                .parse_block()
                .map(|x| Located::new(Stmt::Block(x), *tok.loc())),
            _ => todo!(),
        }
    }

    /// Parses a block statement. Assumes that '{' has already been skipped.
    fn parse_block(&mut self) -> Result<BlockStmt> {
        let mut stmts = vec![];
        loop {
            if self.lexer.skip(SymbolKind::ClosingBrace.into()) {
                break;
            }
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        Ok(BlockStmt(stmts))
    }
}

#[test]
fn parse_block() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("{}")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_block2() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("  { \n}      ")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}
