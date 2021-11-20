use crate::lexer::{traits::LexerLike, Error};
use crate::Parser;
use anyhow::Result;
use long_ast::node::ty::{Sign, Type};
use long_ast::node::{decl::Decl, Located};
use long_ast::token::kind::{KeywordKind, TokenKind};

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a declaration sequence.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
    pub(crate) fn parse_decl_seq(&mut self) -> Result<Located<Decl>> {
        // TODO
        self.parse_decl()
    }

    /// Parses a declaration.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
    pub(crate) fn parse_decl(&mut self) -> Result<Located<Decl>> {
        // TODO
        self.parse_func_def()
    }

    /// Parses a function definition.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.fct.def.general#1>
    fn parse_func_def(&mut self) -> Result<Located<Decl>> {
        // function-definition:
        //     attribute-specifier-seq(opt) decl-specifier-seq(opt) declarator virt-specifier-seq(opt) function-body
        let _decl_specifier = self.parse_decl_specifier();
        todo!()
    }

    /// Parses declaration specifiers.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.spec#nt:decl-specifier-seq>
    // Add tests for this.
    fn parse_decl_specifier(&mut self) -> Result<Type> {
        #[derive(PartialEq, Debug, Clone)]
        enum Size {
            Short,
            Long,
            LongLong,
            Unspecified,
        }

        #[derive(PartialEq, Debug, Clone)]
        enum PrimitiveType {
            Void,
            Bool,
            Char,
            Char16,
            Char32,
            Int,
            Float,
            Double,
        }

        let mut ty = None;
        let mut sign = None;

        loop {
            let tok = self.lexer.peek()?.ok_or(Error::UnexpectedEof)?;
            match tok.kind() {
                TokenKind::Keyword(
                    KeywordKind::Void
                    | KeywordKind::Bool
                    | KeywordKind::Char
                    | KeywordKind::Char16T
                    | KeywordKind::Char32T
                    | KeywordKind::Int
                    | KeywordKind::Float
                    | KeywordKind::Double,
                ) if ty.is_some() => {
                    return Err(Error::Message(
                        format!("Cannot specify '{}'", tok.kind().to_string()),
                        *tok.loc(),
                    )
                    .into())
                }
                TokenKind::Keyword(KeywordKind::Void) => ty = Some(PrimitiveType::Void),
                TokenKind::Keyword(KeywordKind::Bool) => ty = Some(PrimitiveType::Bool),
                TokenKind::Keyword(KeywordKind::Char) => ty = Some(PrimitiveType::Char),
                TokenKind::Keyword(KeywordKind::Char16T) => ty = Some(PrimitiveType::Char16),
                TokenKind::Keyword(KeywordKind::Char32T) => ty = Some(PrimitiveType::Char32),
                TokenKind::Keyword(KeywordKind::Int) => ty = Some(PrimitiveType::Int),
                TokenKind::Keyword(KeywordKind::Float) => ty = Some(PrimitiveType::Float),
                TokenKind::Keyword(KeywordKind::Double) => ty = Some(PrimitiveType::Double),
                TokenKind::Keyword(KeywordKind::Signed) if sign == Some(Sign::Signed) => {
                    // TODO: Had better raise a warning message.
                }
                TokenKind::Keyword(KeywordKind::Signed) if sign == Some(Sign::Unsigned) => {
                    return Err(Error::Message(
                        format!("Cannot specify '{}'", tok.kind().to_string()),
                        *tok.loc(),
                    )
                    .into())
                }
                TokenKind::Keyword(KeywordKind::Unsigned) if sign == Some(Sign::Signed) => {
                    return Err(Error::Message(
                        format!("Cannot specify '{}'", tok.kind().to_string()),
                        *tok.loc(),
                    )
                    .into())
                }
                TokenKind::Keyword(KeywordKind::Signed) => sign = Some(Sign::Signed),
                TokenKind::Keyword(KeywordKind::Unsigned) => sign = Some(Sign::Unsigned),
                _ => break,
            }
            self.lexer.next()?;
        }

        let sign = sign.unwrap_or(Sign::Signed);

        match ty {
            Some(PrimitiveType::Void) => return Ok(Type::Void),
            Some(PrimitiveType::Bool) => return Ok(Type::Bool),
            Some(PrimitiveType::Char) => return Ok(Type::Char(sign)),
            Some(PrimitiveType::Char16) => return Ok(Type::Char16(sign)),
            Some(PrimitiveType::Char32) => return Ok(Type::Char32(sign)),
            Some(PrimitiveType::Int) => return Ok(Type::Int(sign)),
            Some(PrimitiveType::Float) => return Ok(Type::Float),
            Some(PrimitiveType::Double) => return Ok(Type::Double),
            None => {}
        }

        todo!()
    }
}
