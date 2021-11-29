use crate::{
    lexer::{traits::LexerLike, Error},
    Parser,
};
use anyhow::Result;
use long_ast::{
    node::{
        decl::Decl,
        expr::Expr,
        stmt::{BlockStmt, Stmt},
        Located,
    },
    token::kind::{KeywordKind, SymbolKind, TokenKind},
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
            TokenKind::Keyword(KeywordKind::Return) => self
                .parse_return()
                .map(|x| Located::new(Stmt::Return(x), *tok.loc())),
            TokenKind::Keyword(kwd) if kwd.is_type() => match self.parse_decl(false)? {
                Located {
                    inner: Decl::SimpleDecl(decls),
                    loc,
                } => Ok(Located::new(Stmt::SimpleDecl(decls), loc)),
                _ => unreachable!(),
            },
            _ => {
                let loc = *tok.loc();
                self.lexer.unget(tok);
                let expr = self.parse_expr()?;
                self.expect(SymbolKind::Semicolon)?;
                Ok(Located::new(Stmt::Expr(expr), loc))
            }
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

    /// Parses a return statement. Assumes that 'return' has already been skipped.
    fn parse_return(&mut self) -> Result<Option<Located<Expr>>> {
        if self.lexer.skip(SymbolKind::Semicolon.into()) {
            return Ok(None);
        }
        let expr = self.parse_expr()?;
        self.expect(SymbolKind::Semicolon)?;
        Ok(Some(expr))
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

#[test]
fn parse_return() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("return 0;")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_return2() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("return;")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_return3() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("return")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_vardecl() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("int i;")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_vardecl2() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("int i, j;")).parse_stmt();
    insta::assert_debug_snapshot!(node);
}
