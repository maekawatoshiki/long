use crate::Parser;
use anyhow::Result;
use long_ast::node::expr::{BinOp, Expr};
use long_lexer::{token::kind::SymbolKind, traits::LexerLike, Error};

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a comma expression.
    pub(crate) fn parse_comma(&mut self) -> Result<Expr> {
        let mut lhs = self.parse_primary()?;
        while self.lexer.skip(SymbolKind::Comma.into()) {
            let rhs = self.parse_primary()?;
            lhs = Expr::BinOp(BinOp::Comma(Box::new(lhs), Box::new(rhs)));
        }
        todo!()
        // let mut lhs = try!(self.read_assign());
        // while try!(self.lexer.skip_symbol(Symbol::Comma)) {
        //     let rhs = try!(self.read_assign());
        //     lhs = AST::new(
        //         ASTKind::BinaryOp(Box::new(lhs), Box::new(rhs), node::CBinOps::Comma),
        //         self.lexer.get_cur_pos(),
        //     )
        // }
        // Ok(lhs)
    }

    /// Parses a literal or parenthetical expression.
    pub(crate) fn parse_primary(&mut self) -> Result<Expr> {
        // TODO: Support other than int literals.
        let _tok = self.lexer.next()?.ok_or_else(|| Error::UnexpectedEof)?;
        todo!()

        // let tok = match self.lexer.get() {
        //     Ok(tok) => tok,
        //     Err(_) => {
        //         let peek = self.lexer.peek();
        //         self.show_error_token(
        //             &try!(peek),
        //             "expected primary(number, string...), but reach EOF",
        //         );
        //         return Err(Error::Something);
        //     }
        // };
        //
        // match tok.kind.clone() {
        //     TokenKind::IntNumber(n, bits) => {
        //         Ok(AST::new(ASTKind::Int(n, bits), self.lexer.get_cur_pos()))
        //     }
        //     TokenKind::FloatNumber(f) => Ok(AST::new(ASTKind::Float(f), self.lexer.get_cur_pos())),
        //     TokenKind::Identifier(ident) => {
        //         if let Some(ast) = self.env.get(ident.as_str()) {
        //             return match ast.kind {
        //                 ASTKind::Variable(_, _) => Ok(AST::new(
        //                     ASTKind::Load(Box::new((*ast).clone())),
        //                     self.lexer.get_cur_pos(),
        //                 )),
        //                 _ => Ok((*ast).clone()),
        //             };
        //         }
        //         self.show_error_token(
        //             &tok,
        //             format!("not found the variable or function '{}'", ident).as_str(),
        //         );
        //         Err(Error::Something)
        //     }
        //     TokenKind::String(s) => Ok(AST::new(ASTKind::String(s), self.lexer.get_cur_pos())),
        //     TokenKind::Char(ch) => Ok(AST::new(ASTKind::Char(ch as i32), self.lexer.get_cur_pos())),
        //     TokenKind::Symbol(sym) => match sym {
        //         Symbol::OpeningParen => {
        //             let expr = self.read_expr();
        //             if !try!(self.lexer.skip_symbol(Symbol::ClosingParen)) {
        //                 self.show_error_token(&tok, "expected ')'");
        //             }
        //             expr
        //         }
        //         _ => {
        //             self.show_error_token(
        //                 &tok,
        //                 format!("expected primary section, but got {:?}", tok.kind).as_str(),
        //             );
        //             Err(Error::Something)
        //         }
        //     },
        //     _ => {
        //         self.show_error_token(
        //             &tok,
        //             format!("read_primary unknown token {:?}", tok.kind).as_str(),
        //         );
        //         Err(Error::Something)
        //     }
        // }
    }
}
