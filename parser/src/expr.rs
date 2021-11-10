use crate::Parser;
use anyhow::Result;
use long_ast::node::{
    expr::{BinOp, Expr, ExprKind},
    lit::Literal,
};
use long_lexer::{
    token::kind::{SymbolKind, TokenKind},
    traits::LexerLike,
    Error,
};

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a comma expression.
    pub(crate) fn parse_comma(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_primary()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::Comma.into()) {
            let rhs = self.parse_primary()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::Comma, Box::new(lhs), Box::new(rhs)),
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
            _ => todo!(),
        }
    }
}

#[test]
fn parse_comma() {
    use lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("1, 2, 3")).parse_expr();
    insta::assert_debug_snapshot!(node);
}
