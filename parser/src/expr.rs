use crate::lexer::{traits::LexerLike, Error};
use crate::Parser;
use anyhow::Result;
use long_ast::node::expr::{AssignOp, UnaryOp};
use long_ast::{
    node::{
        expr::{BinOp, Expr},
        lit::Literal,
        Located,
    },
    token::kind::{SymbolKind, TokenKind},
};

macro_rules! def_binop {
    ($name:ident < $child:ident, $($sym:ident -> $op:ident),+) => {
        fn $name(&mut self) -> Result<Located<Expr>> {
            let mut lhs = self.$child()?;
            let loc = lhs.loc();
            loop {
                $(
                    if self.lexer.skip(SymbolKind::$sym.into()) {
                        let rhs = self.$child()?;
                        lhs = Located::new(
                            Expr::Binary(BinOp::$op, Box::new(lhs), Box::new(rhs)),
                            loc,
                        );
                        continue;
                    }
                )+;
                break;
            }
            Ok(lhs)
        }
    };
}

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a comma expression.
    pub(crate) fn parse_comma(&mut self) -> Result<Located<Expr>> {
        let mut lhs = self.parse_assign()?;
        let loc = lhs.loc();
        while self.lexer.skip(SymbolKind::Comma.into()) {
            let rhs = self.parse_assign()?;
            lhs = Located::new(
                Expr::Binary(BinOp::Comma, Box::new(lhs), Box::new(rhs)),
                loc,
            );
        }
        Ok(lhs)
    }

    /// Parses an assignment expression.
    pub(crate) fn parse_assign(&mut self) -> Result<Located<Expr>> {
        let mut lhs = self.parse_logor()?;
        let loc = lhs.loc();

        if self.lexer.skip(SymbolKind::Question.into()) {
            return self.parse_ternary(lhs);
        }

        loop {
            if let Ok(Some(tok)) = self.lexer.peek() {
                let op = match tok.kind() {
                    TokenKind::Symbol(SymbolKind::Assign) => AssignOp::None,
                    TokenKind::Symbol(SymbolKind::AssignAdd) => AssignOp::Add,
                    TokenKind::Symbol(SymbolKind::AssignSub) => AssignOp::Sub,
                    TokenKind::Symbol(SymbolKind::AssignMul) => AssignOp::Mul,
                    TokenKind::Symbol(SymbolKind::AssignDiv) => AssignOp::Div,
                    TokenKind::Symbol(SymbolKind::AssignMod) => AssignOp::Rem,
                    TokenKind::Symbol(SymbolKind::AssignShl) => AssignOp::Shl,
                    TokenKind::Symbol(SymbolKind::AssignShr) => AssignOp::Shr,
                    TokenKind::Symbol(SymbolKind::AssignAnd) => AssignOp::And,
                    TokenKind::Symbol(SymbolKind::AssignOr) => AssignOp::Or,
                    TokenKind::Symbol(SymbolKind::AssignXor) => AssignOp::Xor,
                    _ => break,
                };
                self.lexer.next()?;
                let rhs = self.parse_assign()?;
                lhs = Located::new(Expr::Assign(op, Box::new(lhs), Box::new(rhs)), loc);
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    /// Parses a ternary expression.
    fn parse_ternary(&mut self, cond: Located<Expr>) -> Result<Located<Expr>> {
        let loc = cond.loc();
        let thn = self.parse_assign()?;
        self.expect(SymbolKind::Colon)?;
        let els = self.parse_assign()?;
        Ok(Located::new(
            Expr::Ternary(Box::new(cond), Box::new(thn), Box::new(els)),
            loc,
        ))
    }

    def_binop!(parse_logor < parse_logand, LOr -> LogicalOr);
    def_binop!(parse_logand < parse_or, LAnd -> LogicalAnd);
    def_binop!(parse_or < parse_and, Or -> Or);
    def_binop!(parse_and < parse_eq_ne, And -> And);
    def_binop!(parse_eq_ne < parse_lt_le_gt_ge, Eq -> Eq, Ne -> Ne);
    def_binop!(parse_lt_le_gt_ge < parse_shl_shr, Lt -> Lt, Le -> Le, Gt -> Gt, Ge -> Ge);
    def_binop!(parse_shl_shr < parse_add_sub, Shl -> Shl, Shr -> Shr);
    def_binop!(parse_add_sub < parse_mul_div_rem, Add -> Add, Sub -> Sub);
    def_binop!(parse_mul_div_rem < parse_unary, Asterisk -> Mul, Div -> Div, Mod -> Rem);

    /// Parses a unary expression.
    fn parse_unary(&mut self) -> Result<Located<Expr>> {
        let tok = self.lexer.peek()?.ok_or(Error::UnexpectedEof)?;
        match tok.kind() {
            TokenKind::Symbol(SymbolKind::Not) => {
                self.lexer.next()?;
                return Ok(Located::new(
                    Expr::Unary(UnaryOp::Not, Box::new(self.parse_unary()?)),
                    *tok.loc(),
                ));
            }
            _ => self.parse_postfix(),
        }
    }

    /// Parses a postfix expression.
    fn parse_postfix(&mut self) -> Result<Located<Expr>> {
        let mut base = self.parse_primary()?;
        loop {
            if self.lexer.skip(SymbolKind::OpeningParen.into()) {
                base = self.parse_func_call(base)?;
                continue;
            }
            break;
        }
        Ok(base)
    }

    /// Parses a function call expression.
    fn parse_func_call(
        &mut self,
        Located { inner: base, loc }: Located<Expr>,
    ) -> Result<Located<Expr>> {
        let mut args = vec![];
        while !self.lexer.skip(SymbolKind::ClosingParen.into()) {
            args.push(self.parse_assign()?);
            if self.lexer.skip(SymbolKind::ClosingParen.into()) {
                break;
            }
            self.expect(SymbolKind::Comma)?;
        }
        Ok(Located::new(
            Expr::Call(Box::new(Located::new(base, loc)), args),
            loc,
        ))
    }

    /// Parses a literal or parenthetical expression.
    pub(crate) fn parse_primary(&mut self) -> Result<Located<Expr>> {
        // TODO: Support other than int literals.
        let tok = self.lexer.next()?.ok_or(Error::UnexpectedEof)?;
        match tok.kind() {
            TokenKind::Int(i) => Ok(Located::new(Expr::Literal(Literal::Int(*i)), *tok.loc())),
            TokenKind::Symbol(SymbolKind::OpeningParen) => {
                let expr = self.parse_expr()?;
                self.expect(SymbolKind::ClosingParen)?;
                Ok(expr)
            }
            TokenKind::Ident(i) => Ok(Located::new(
                Expr::Literal(Literal::Ident(i.into())),
                *tok.loc(),
            )),
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

#[test]
fn parse_mul_div_rem() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 * 1 / 2 % 3")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_assign() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("0 = 1 = 2")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_assign_op() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new(
        "0 += 1 -= 2 *= 3 /= 4 %= 5 <<= 6 >>= 7 &= 8 |= 9 ^= 10",
    ))
    .parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_lit() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("ident, 1")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_call() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("f()")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_call2() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("f(1)")).parse_expr();
    insta::assert_debug_snapshot!(node);
}

#[test]
fn parse_call3() {
    use crate::lexer::Lexer;
    let node = Parser::new(&mut Lexer::new("f(1, 2)")).parse_expr();
    insta::assert_debug_snapshot!(node);
}
