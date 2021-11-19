use crate::lexer::{traits::LexerLike, Error};
use crate::Parser;
use anyhow::Result;
use long_ast::node::expr::UnaryOp;
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
        let mut lhs = self.parse_or()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::LAnd.into()) {
            let rhs = self.parse_or()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::LogicalAnd, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses an or expression.
    fn parse_or(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_and()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::Or.into()) {
            let rhs = self.parse_and()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::Or, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses an and expression.
    fn parse_and(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_eq_ne()?;
        let loc = *lhs.loc();
        while self.lexer.skip(SymbolKind::And.into()) {
            let rhs = self.parse_eq_ne()?;
            lhs = Expr::new(
                ExprKind::Binary(BinOp::And, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses an euqal, not equal operator.
    fn parse_eq_ne(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_lt_le_gt_ge()?;
        let loc = *lhs.loc();
        loop {
            if self.lexer.skip(SymbolKind::Eq.into()) {
                let rhs = self.parse_lt_le_gt_ge()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Eq, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else if self.lexer.skip(SymbolKind::Ne.into()) {
                let rhs = self.parse_lt_le_gt_ge()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Ne, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// Parses a less than, less than or equal, greater than, greater than or equal expression.
    fn parse_lt_le_gt_ge(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_shl_shr()?;
        let loc = *lhs.loc();
        loop {
            if self.lexer.skip(SymbolKind::Lt.into()) {
                let rhs = self.parse_shl_shr()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Lt, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else if self.lexer.skip(SymbolKind::Le.into()) {
                let rhs = self.parse_shl_shr()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Le, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else if self.lexer.skip(SymbolKind::Gt.into()) {
                let rhs = self.parse_shl_shr()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Gt, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else if self.lexer.skip(SymbolKind::Ge.into()) {
                let rhs = self.parse_shl_shr()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Ge, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// Parses a left-shift or right-shift expression.
    fn parse_shl_shr(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_add_sub()?;
        let loc = *lhs.loc();
        loop {
            if self.lexer.skip(SymbolKind::Shl.into()) {
                let rhs = self.parse_add_sub()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Shl, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else if self.lexer.skip(SymbolKind::Shr.into()) {
                let rhs = self.parse_add_sub()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Shr, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// Parses an addition or subtraction expression.
    fn parse_add_sub(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_unary()?;
        let loc = *lhs.loc();
        loop {
            if self.lexer.skip(SymbolKind::Add.into()) {
                let rhs = self.parse_unary()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Add, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else if self.lexer.skip(SymbolKind::Sub.into()) {
                let rhs = self.parse_unary()?;
                lhs = Expr::new(
                    ExprKind::Binary(BinOp::Sub, Box::new(lhs), Box::new(rhs)),
                    loc,
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// Parses a unary expression.
    fn parse_unary(&mut self) -> Result<Expr> {
        let tok = self.lexer.peek()?.ok_or(Error::UnexpectedEof)?;
        match tok.kind() {
            TokenKind::Symbol(SymbolKind::Not) => {
                self.lexer.next()?;
                return Ok(Expr::new(
                    ExprKind::Unary(UnaryOp::Not, Box::new(self.parse_unary()?)),
                    *tok.loc(),
                ));
            }
            _ => self.parse_primary(),
        }
    }

    /// Parses a literal or parenthetical expression.
    pub(crate) fn parse_primary(&mut self) -> Result<Expr> {
        // TODO: Support other than int literals.
        let tok = self.lexer.next()?.ok_or(Error::UnexpectedEof)?;
        match tok.kind() {
            TokenKind::Int(i) => Ok(Expr::new(ExprKind::Literal(Literal::Int(*i)), *tok.loc())),
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                let expr = self.parse_expr()?;
                self.expect(SymbolKind::ClosingParen)?;
                Ok(expr)
            }
            e => Err(Error::Message(format!("Unimplemented: {:?}", e), *tok.loc()).into()),
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

#[test]
fn parse_unary() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("!0")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_unary_logand() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("!0 && !1")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_lt_le_gt_ge() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 < 1 && 0 <= 1 && 0 > 1 && 0 >= 1")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_add_sub() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 + 1 - 2")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_shl_shr() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 << 1 >> 2")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_and_or() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 & 1 | 2")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_eq_ne() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 == 0 && 0 != 1")).parse_expr();
    insta::assert_debug_snapshot!(node);
}
