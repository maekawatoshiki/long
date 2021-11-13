use crate::lexer::{traits::LexerLike, Error};
use crate::Parser;
use anyhow::Result;
use long_ast::{
    node::{
        expr::{BinOp, Expr, ExprKind},
        lit::Literal,
    },
    token::kind::{SymbolKind, TokenKind},
};

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a comma expression.
    pub(crate) fn parse_comma(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_assign()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::Comma.into()) {
            let rhs = self.parse_assign()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::Comma, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses an assignment expression.
    pub(crate) fn parse_assign(&mut self) -> Result<Expr> {
        let lhs = self.parse_logor()?;

        if self.lexer.skip(SymbolKind::Question.into()) {
            return self.parse_ternary(lhs);
        }

        // TODO: Support assignments.

        Ok(lhs)
    }

    /// Parses a ternary expression.
    fn parse_ternary(&mut self, cond: Expr) -> Result<Expr> {
        let loc = *cond.loc();
        let thn = self.parse_assign()?;
        self.expect(SymbolKind::Colon)?;
        let els = self.parse_assign()?;
        Ok(Expr::new(
            ExprKind::Ternary(Box::new(cond), Box::new(thn), Box::new(els)),
            loc,
        ))
    }

    /// Parses a logical or expression.
    fn parse_logor(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_logand()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::LOr.into()) {
            let rhs = self.parse_logand()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::LogicalOr, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses a logical and expression.
    fn parse_logand(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_primary()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::LAnd.into()) {
            let rhs = self.parse_primary()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::LogicalAnd, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses a literal or parenthetical expression.
    pub(crate) fn parse_primary(&mut self) -> Result<Expr> {
        // TODO: Support other than int literals.
        let tok = self.lexer.next()?.ok_or_else(|| Error::UnexpectedEof)?;
        match tok.kind() {
            TokenKind::Int(i) => Ok(Expr::new(ExprKind::Literal(Literal::Int(*i)), *tok.loc())),
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                let expr = self.parse_expr()?;
                self.expect(SymbolKind::ClosingParen)?;
                Ok(expr)
            }
            e => todo!("parse_primary: {:?}", e),
        }
    }
}

#[test]
fn parse_comma() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("1, 2, 3")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_ternary() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("1 ? 2 : 3")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_logor_logand() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("1 && 2 || 3 && 4")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_paren() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 && (1 || 2)")).parse_expr();
    insta::assert_debug_snapshot!(node);
}
