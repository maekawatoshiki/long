use crate::lexer::{traits::LexerLike, Error};
use crate::Parser;
use anyhow::Result;
use long_ast::node::decl::{DeclaratorId, FuncDef, SimpleDecl};
use long_ast::node::ty::{FuncType, Sign, Type};
use long_ast::node::{decl::Decl, Located};
use long_ast::token::kind::{KeywordKind, SymbolKind, TokenKind};
use long_ast::token::Token;

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a declaration sequence.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
    pub(crate) fn parse_decl_seq(&mut self) -> Result<Vec<Located<Decl>>> {
        let mut decls = Vec::new();
        while let Some(_) = self.lexer.peek()? {
            decls.push(self.parse_decl(true)?);
        }
        Ok(decls)
    }

    /// Parses a declaration.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
    pub(crate) fn parse_decl(&mut self, accept_funcdef: bool) -> Result<Located<Decl>> {
        let Located {
            inner: decl_specifier,
            loc,
        } = self.parse_decl_specifier()?;

        let mut decls = vec![];

        loop {
            let mut declarator_id = None;
            let ty = self.parse_declarator(decl_specifier.clone(), &mut declarator_id)?;

            if accept_funcdef && self.lexer.skip(SymbolKind::OpeningBrace.into()) {
                return Ok(Located::new(
                    self.parse_func_def(
                        ty.into_func_type().expect("TODO"),
                        declarator_id.expect("TODO"),
                    )?,
                    loc,
                ));
            }

            let loc = self
                .lexer
                .peek()?
                .map_or_else(|| Err(Error::UnexpectedEof), |t| Ok(*t.loc()))?;

            let declarator_id = declarator_id.map_or_else(
                || Err(Error::Message("Expected variable name".to_string(), loc)),
                |x| Ok(x),
            )?;

            decls.push(SimpleDecl {
                name: declarator_id,
                ty,
                init: None, // TODO
            });

            if self.lexer.skip(SymbolKind::Comma.into()) {
                continue;
            }

            self.expect(SymbolKind::Semicolon)?;
            break;
        }

        Ok(Located::new(Decl::SimpleDecl(decls), loc))
    }

    /// Parses a function definition.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.fct.def.general#1>
    // function-definition:
    //     attribute-specifier-seq(opt) decl-specifier-seq(opt) declarator virt-specifier-seq(opt) function-body
    fn parse_func_def(&mut self, ty: FuncType, name: DeclaratorId) -> Result<Decl> {
        let body = self.parse_function_body()?;
        Ok(FuncDef { name, ty, body }.into())
    }

    /// Parses a declarator.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.decl#4>
    fn parse_declarator(
        &mut self,
        basety: Type,
        declarator_id: &mut Option<DeclaratorId>,
    ) -> Result<Type> {
        if self.lexer.skip(SymbolKind::Asterisk.into()) {
            // TODO: Parse attribute-specifier-seq(opt) cv-qualifier-seq(opt) here.
            return self.parse_declarator(Type::Pointer(Box::new(basety)), declarator_id);
        }

        if self.lexer.skip(SymbolKind::And.into()) {
            // TODO: Parse attribute-specifier-seq(opt) here.
            return self.parse_declarator(Type::LValueRef(Box::new(basety)), declarator_id);
        }

        if self.lexer.skip(SymbolKind::LAnd.into()) {
            // TODO: Parse attribute-specifier-seq(opt) here.
            return self.parse_declarator(Type::RValueRef(Box::new(basety)), declarator_id);
        }

        let tok = match self.lexer.next()? {
            Some(tok) => tok,
            None => return Ok(basety),
        };

        match tok.kind {
            // TODO: Support more complex declarator-id.
            TokenKind::Ident(id) => *declarator_id = Some(DeclaratorId::Ident(id)),
            _ => {
                self.lexer.unget(tok);
            }
        }

        let ty = self.parse_declarator_tail(basety)?;

        Ok(ty)
    }

    /// Parses the tail part of a declarator.
    fn parse_declarator_tail(&mut self, basety: Type) -> Result<Type> {
        // parameters-and-qualifiers
        if self.lexer.skip(SymbolKind::OpeningParen.into()) {
            // TODO
            assert!(self.lexer.skip(SymbolKind::ClosingParen.into()));
            return Ok(FuncType { ret: basety }.into());
        }

        Ok(basety)
    }

    /// Parses declaration specifiers.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.spec#nt:decl-specifier-seq>
    fn parse_decl_specifier(&mut self) -> Result<Located<Type>> {
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
            WChar,
            Int,
            Float,
            Double,
        }

        let mut ty = None;
        let mut sign = None;
        let mut loc = None;
        let mut size = Size::Unspecified;

        fn err(tok: Token) -> Result<()> {
            Err(Error::Message(
                format!("Cannot specify '{}'", tok.kind().to_string()),
                *tok.loc(),
            )
            .into())
        }

        loop {
            let tok = match self.lexer.next()? {
                Some(tok) => tok,
                None => break,
            };

            if loc.is_none() {
                loc = Some(*tok.loc());
            }

            // TODO: Support more types: struct, union, enum, class, user defined types...

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
                ) if ty.is_some() => err(tok)?,
                TokenKind::Keyword(KeywordKind::Void) => ty = Some(PrimitiveType::Void),
                TokenKind::Keyword(KeywordKind::Bool) => ty = Some(PrimitiveType::Bool),
                TokenKind::Keyword(KeywordKind::Char) => ty = Some(PrimitiveType::Char),
                TokenKind::Keyword(KeywordKind::Char16T) => ty = Some(PrimitiveType::Char16),
                TokenKind::Keyword(KeywordKind::Char32T) => ty = Some(PrimitiveType::Char32),
                TokenKind::Keyword(KeywordKind::WcharT) => ty = Some(PrimitiveType::WChar),
                TokenKind::Keyword(KeywordKind::Int) => ty = Some(PrimitiveType::Int),
                TokenKind::Keyword(KeywordKind::Float) => ty = Some(PrimitiveType::Float),
                TokenKind::Keyword(KeywordKind::Double) => ty = Some(PrimitiveType::Double),
                TokenKind::Keyword(KeywordKind::Short) if size == Size::Unspecified => {
                    size = Size::Short
                }
                TokenKind::Keyword(KeywordKind::Short) => err(tok)?,
                TokenKind::Keyword(KeywordKind::Long) if size == Size::Unspecified => {
                    size = Size::Long
                }
                TokenKind::Keyword(KeywordKind::Long) if size == Size::Long => {
                    size = Size::LongLong;
                }
                TokenKind::Keyword(KeywordKind::Long) if size == Size::LongLong => {
                    err(tok)?;
                }
                TokenKind::Keyword(KeywordKind::Signed) if sign == Some(Sign::Signed) => {
                    // TODO: Had better raise a warning message.
                }
                TokenKind::Keyword(KeywordKind::Signed) if sign == Some(Sign::Unsigned) => {
                    err(tok)?
                }
                TokenKind::Keyword(KeywordKind::Unsigned) if sign == Some(Sign::Signed) => {
                    err(tok)?
                }
                TokenKind::Keyword(KeywordKind::Signed) => sign = Some(Sign::Signed),
                TokenKind::Keyword(KeywordKind::Unsigned) => sign = Some(Sign::Unsigned),
                _ => {
                    self.lexer.unget(tok);
                    break;
                }
            }
        }

        let loc = match loc {
            Some(loc) => loc,
            None => return Err(Error::UnexpectedEof.into()),
        };
        let sign = sign.unwrap_or(Sign::Signed);

        let ty = match ty {
            Some(PrimitiveType::Void) => Type::Void,
            Some(PrimitiveType::Bool) => Type::Bool,
            Some(PrimitiveType::Char) => Type::Char(sign),
            Some(PrimitiveType::Char16) => Type::Char16(sign),
            Some(PrimitiveType::Char32) => Type::Char32(sign),
            Some(PrimitiveType::WChar) => Type::WChar(sign),
            Some(PrimitiveType::Int) if size == Size::Short => Type::Short(sign),
            Some(PrimitiveType::Int) if size == Size::Long => Type::Long(sign),
            Some(PrimitiveType::Int) if size == Size::LongLong => Type::LongLong(sign),
            Some(PrimitiveType::Int) => Type::Int(sign),
            Some(PrimitiveType::Float) => Type::Float,
            Some(PrimitiveType::Double) => Type::Double,
            None => match size {
                Size::Short => Type::Short(sign),
                Size::Long => Type::Long(sign),
                Size::LongLong => Type::LongLong(sign),
                Size::Unspecified => Type::Int(sign),
            },
        };

        Ok(Located::new(ty, loc))
    }
}

macro_rules! decl_specifier_test {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            use crate::lexer::Lexer;
            let mut lexer = Lexer::new($src);
            let mut parser = Parser::new(&mut lexer);
            insta::assert_debug_snapshot!(parser.parse_decl_specifier());
        }
    };
}

decl_specifier_test!(decl_spec_empty, "");
decl_specifier_test!(decl_spec_bool, "bool");
decl_specifier_test!(decl_spec_char, "char");
decl_specifier_test!(decl_spec_char16, "char16_t");
decl_specifier_test!(decl_spec_char32, "char32_t");
decl_specifier_test!(decl_spec_unsigned_char, "unsigned char");
decl_specifier_test!(decl_spec_unsigned_char_2, "char unsigned");
decl_specifier_test!(decl_spec_wchar, "wchar_t");
decl_specifier_test!(decl_spec_short, "short");
decl_specifier_test!(decl_spec_int, "int");
decl_specifier_test!(decl_spec_long, "long");
decl_specifier_test!(decl_spec_long_long, "long long");
decl_specifier_test!(decl_spec_long_long_long, "long long long");
decl_specifier_test!(decl_spec_int_long, "int long");
decl_specifier_test!(decl_spec_long_int, "long int");
decl_specifier_test!(decl_spec_unsigned_long_int, "unsigned long int");
decl_specifier_test!(decl_spec_float, "float");
decl_specifier_test!(decl_spec_double, "double");

macro_rules! declarator_test {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            use crate::lexer::Lexer;
            let mut lexer = Lexer::new($src);
            let mut parser = Parser::new(&mut lexer);
            #[derive(Debug)]
            #[allow(dead_code)]
            struct Result {
                ty: Type,
                declarator_id: Option<DeclaratorId>,
            }
            let mut declarator_id = None;
            let ty = parser
                .parse_declarator(Type::Void, &mut declarator_id)
                .unwrap();
            insta::assert_debug_snapshot!(Result { ty, declarator_id })
        }
    };
}

declarator_test!(declarator_ptr, "*");
declarator_test!(declarator_many_ptr, "****");
declarator_test!(declarator_lvalref, "&");
declarator_test!(declarator_rvalref, "&&");
declarator_test!(declarator_id, "var");
declarator_test!(declarator_func, "f()");

macro_rules! decl_test {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            use crate::lexer::Lexer;
            let mut lexer = Lexer::new($src);
            let mut parser = Parser::new(&mut lexer);
            insta::assert_debug_snapshot!(parser.parse_decl(true));
        }
    };
}

decl_test!(decl_func, "int f() {}");
decl_test!(decl_func2, "void ggg() {}");
decl_test!(decl_var, "int var;");
decl_test!(decl_var2, "int var, var2;");
decl_test!(decl_var3, "int *ptr, i;");
