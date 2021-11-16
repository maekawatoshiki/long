use super::cursor::Cursor;
use super::macros::{FuncMacroToken, Macro, Macros};
use crate::Parser;
use anyhow::Result;
use ast::token::kind::{FloatKind, IntKind, KeywordKind, SymbolKind, TokenKind};
use ast::token::{stringify, Token};
use long_sourceloc::SourceLoc;
use sourceloc::source::{Source, Sources};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::fs::read_to_string;
use std::path::PathBuf;

/// A lexical analyzer for either a single source file or a string.
pub(crate) struct SourceLexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,

    /// The cursor of the lexer.
    cursor: Cursor,

    /// The buffer for the tokens ungot.
    buf: VecDeque<Token>,

    /// The conditions of preprocessor directives.
    cond_stack: Vec<bool>,
}

#[derive(Debug)]
pub(crate) enum Error {
    Include(PathBuf, SourceLoc),
    Unexpected(SourceLoc),
    Message(String, SourceLoc),
    UnexpectedEof,
}

impl SourceLexer {
    /// Creates a new `SourceLexer`.
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            filepath: None,
            cursor: Cursor::new(source.into()),
            buf: VecDeque::new(),
            cond_stack: Vec::new(),
        }
    }

    /// Creates a new `SourceLexer` for a file.
    pub fn new_from_file<P>(filepath: P, sources: &mut Sources) -> Result<Self>
    where
        P: Into<PathBuf> + Clone,
    {
        Ok(Self {
            cursor: Cursor::new(read_to_string(filepath.clone().into())?)
                .with_source_id(sources.add(Source::File(filepath.clone().into()))),
            filepath: Some(filepath.into()),
            buf: VecDeque::new(),
            cond_stack: Vec::new(),
        })
    }

    /// Returns the file path.
    pub fn filepath(&self) -> Option<&PathBuf> {
        self.filepath.as_ref()
    }

    /// Reads a token and preprocess it if necessary.
    pub fn next_preprocessed(&mut self, macros: &mut Macros) -> Result<Option<Token>> {
        // Convert an identifier token to a keyword token if necessary.
        fn to_keyword_if_necessary(tok: Token) -> Token {
            match tok.kind() {
                TokenKind::Ident(ident) => match KeywordKind::from_str(ident.as_str()) {
                    Some(kind) => tok
                        .with_kind(TokenKind::Keyword(kind))
                        .with_hideset(HashSet::new()),
                    None => tok,
                },
                _ => tok,
            }
        }

        match self.next()? {
            Some(tok) if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::Hash)) => {
                self.read_cpp_directive(macros)?;
                return self.next_preprocessed(macros);
            }
            Some(tok) if tok.kind() == &TokenKind::NewLine => self.next_preprocessed(macros),
            Some(tok) => Ok(Some(to_keyword_if_necessary(self.expand(macros, tok)?))),
            None => return Ok(None),
        }
    }

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        if !self.buf.is_empty() {
            return Ok(self.buf.pop_front());
        }

        match self.cursor.peek_char() {
            Some(c) if c.is_ascii_alphabetic() || c == '_' => Ok(Some(self.read_identifier())),
            Some(c) if c == ' ' || c == '\t' => {
                self.read_whitespaces();
                self.next().map(|t| t.map(|t| t.set_leading_space(true)))
            }
            Some(c) if c == '\n' => {
                self.read_newlines();
                Ok(Some(Token::new(TokenKind::NewLine, self.cursor.loc)))
            }
            Some(c) if c == '\"' => Ok(Some(self.read_string()?)),
            Some(c)
                if c.is_ascii_digit()
                    || (c == '.'
                        && self
                            .cursor
                            .peek_char2()
                            .map_or(false, |c| c.is_ascii_digit())) =>
            {
                Ok(Some(self.read_number()?))
            }
            Some(c) if c == '/' && matches!(self.cursor.peek_char2(), Some('/' | '*')) => {
                self.read_comment();
                self.next()
            }
            Some(c) if c == '\\' => {
                self.cursor.take_chars_while(|&c| c != '\n');
                assert_eq!(self.cursor.next_char().unwrap(), '\n');
                self.next()
            }
            Some(c) if SymbolKind::at_least_starts_with(c) => Ok(Some(self.read_symbol())),
            Some(c) => Err(Error::Message(
                format!("Unsupported character: {:?}", c),
                self.cursor.loc,
            )
            .into()),
            None => return Ok(None),
        }
    }

    /// Pushes `tok` back to the buffer so that it can be read again.
    pub fn unget(&mut self, tok: Token) {
        self.buf.push_front(tok);
    }

    /// Reads an identifier. Assumes the result of `self.cursor.peek_char()` is alphabetic.
    fn read_identifier(&mut self) -> Token {
        let loc = self.cursor.loc;
        let ident = self
            .cursor
            .take_chars_while(|&c| c.is_ascii_alphanumeric() || c == '_');
        Token::new(TokenKind::Ident(ident), loc)
    }

    /// Reads a string literal.
    fn read_string(&mut self) -> Result<Token> {
        let loc = self.cursor.loc;
        assert_eq!(self.cursor.next_char(), Some('\"'));
        let mut string = "".to_string();
        while let Some(c) = self.cursor.next_char() {
            if c == '\"' {
                return Ok(Token::new(TokenKind::String(string), loc));
            }
            // TODO: Support escape sequences.
            string.push(c);
        }
        Err(Error::Unexpected(loc).into())
    }

    /// Reads a symbol.
    fn read_symbol(&mut self) -> Token {
        let loc = self.cursor.loc;
        let x = self.cursor.peek_char();
        let y = self.cursor.peek_char2();
        match (x, y) {
            (Some('.'), Some('.')) if self.cursor.peek_char3() == Some('.') => {
                self.cursor.next_char();
                self.cursor.next_char();
                self.cursor.next_char();
                Token::new(TokenKind::Symbol(SymbolKind::Vararg), loc)
            }
            (Some('<'), Some('<'))
            | (Some('>'), Some('>'))
            | (Some('&'), Some('&'))
            | (Some('|'), Some('|'))
                if self.cursor.peek_char3() == Some('=') =>
            {
                self.cursor.next_char();
                self.cursor.next_char();
                self.cursor.next_char();
                Token::new(
                    TokenKind::Symbol(match x {
                        Some('<') => SymbolKind::AssignShl,
                        Some('>') => SymbolKind::AssignShr,
                        Some('&') => SymbolKind::AssignLAnd,
                        Some('|') => SymbolKind::AssignLOr,
                        _ => unreachable!(),
                    }),
                    loc,
                )
            }
            (Some(x), Some(y)) => match SymbolKind::from_two_chars(x, y) {
                Some(kind) => {
                    self.cursor.next_char();
                    self.cursor.next_char();
                    Token::new(TokenKind::Symbol(kind), loc)
                }
                None => self.read_symbol1(),
            },
            _ => self.read_symbol1(),
        }
    }

    /// Reads a single-character symbol.
    fn read_symbol1(&mut self) -> Token {
        let loc = self.cursor.loc;
        let c = self.cursor.next_char().unwrap();
        Token::new(
            TokenKind::Symbol(SymbolKind::from_char(c).expect("unreachable")),
            loc,
        )
    }

    /// Reads a number literal, including integer literals and floating-point literals.
    fn read_number(&mut self) -> Result<Token> {
        let loc = self.cursor.loc;
        let mut is_float = false;
        let mut last = '\0';
        let lit = self.cursor.take_chars_while(|&c| {
            let is_f = "eEpP".contains(last) && "+-".contains(c);
            is_float = is_float || c == '.' || is_f;
            last = c;
            c.is_alphanumeric() || c == '.' || is_f
        });
        if is_float {
            return Ok(
                if let Some((lit, after_suffix)) = lit.split_once(|c| matches!(c, 'f' | 'F')) {
                    if !after_suffix.is_empty() {
                        return Err(Error::Unexpected(loc).into());
                    }
                    let f: f32 = lit.parse().unwrap();
                    Token::new(TokenKind::Float(FloatKind::Float(f)), loc)
                } else if let Some((lit, after_suffix)) = lit.split_once(|c| matches!(c, 'l' | 'L'))
                {
                    if !after_suffix.is_empty() {
                        return Err(Error::Unexpected(loc).into());
                    }
                    let f: f64 = lit.parse().unwrap();
                    Token::new(TokenKind::Float(FloatKind::LongDouble(f)), loc)
                } else if lit.ends_with(|c: char| c.is_ascii_digit() || c == '.') {
                    let f: f64 = lit.parse().unwrap();
                    Token::new(TokenKind::Float(FloatKind::Double(f)), loc)
                } else {
                    return Err(Error::Unexpected(loc).into());
                },
            );
        }
        let (lit, radix) = if lit.starts_with("0x") || lit.starts_with("0X") {
            (&lit[2..], 16)
        } else if lit.starts_with("0") {
            (&lit[..], 8)
        } else {
            (lit.as_str(), 10)
        };
        // TODO: Support suffix.
        let lit = lit.trim_end_matches(|c| matches!(c, 'u' | 'U' | 'l' | 'L'));
        if let Ok(i) = i32::from_str_radix(lit, radix) {
            Ok(Token::new(TokenKind::Int(IntKind::Int(i)), loc))
        } else if let Ok(i) = u32::from_str_radix(lit, radix) {
            Ok(Token::new(TokenKind::Int(IntKind::UInt(i)), loc))
        } else if let Ok(i) = i64::from_str_radix(lit, radix) {
            Ok(Token::new(TokenKind::Int(IntKind::LongLongInt(i)), loc))
        } else if let Ok(i) = u64::from_str_radix(lit, radix) {
            Ok(Token::new(TokenKind::Int(IntKind::ULongLongInt(i)), loc))
        } else {
            // Too big integer.
            // TODO: We should have a generous error message.
            Err(Error::Unexpected(loc).into())
        }
    }

    /// Reads a comment. Returns if the comment is a line comment.
    fn read_comment(&mut self) {
        assert_eq!(self.cursor.next_char(), Some('/'));
        let is_line_comment = self.cursor.next_char() == Some('/');
        if is_line_comment {
            self.cursor.take_chars_while(|&c| c != '\n');
        } else {
            let mut last = '\0';
            self.cursor.take_chars_while(|&c| {
                let end = last == '*' && c == '/';
                last = c;
                !end
            });
            assert_eq!(self.cursor.next_char(), Some('/'));
        }
    }

    /// Reads whitespaces (i.e. ' ' '\t').
    fn read_whitespaces(&mut self) -> String {
        self.cursor.take_chars_while(|&c| c == ' ' || c == '\t')
    }

    /// Reads whitespaces (i.e. '\n').
    fn read_newlines(&mut self) -> String {
        self.cursor.take_chars_while(|&c| c == '\n')
    }

    // Functions for preprocess.

    /// Reads a preprocessor directive.
    fn read_cpp_directive(&mut self, macros: &mut Macros) -> Result<()> {
        let tok = match self.next()? {
            Some(tok) => tok,
            None => return Ok(()),
        };
        match tok.kind() {
            TokenKind::Ident(i) => match i.as_str() {
                "include" => {
                    let loc = self.cursor.loc;
                    let name = self.read_header_name()?;
                    Err(Error::Include(name.into(), loc).into())
                }
                "define" => self.read_define(macros),
                "undef" => self.read_undef(macros),
                "if" => self.read_if(macros),
                "ifdef" => self.read_ifdef(macros),
                "ifndef" => self.read_ifndef(macros),
                "elif" => self.read_elif(macros),
                "else" => self.read_else(),
                "error" => self.read_error(*tok.loc()),
                "endif" => {
                    self.cond_stack.pop().unwrap();
                    Ok(())
                }
                "pragma" => self.read_pragma(),
                e => todo!("Directive {}", e),
            },
            _ => Ok(()),
        }
    }

    /// Reads a header name of #include directive.
    fn read_header_name(&mut self) -> Result<String> {
        let loc = self.cursor.loc;
        match self.next()? {
            Some(tok) if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::Lt)) => {
                let name = self.cursor.take_chars_while(|&c| c != '>');
                assert_eq!(self.cursor.next_char(), Some('>'));
                Ok(name)
            }
            _ => Err(Error::Unexpected(loc).into()),
        }
    }

    /// Reads a #define directive.
    fn read_define(&mut self, macros: &mut Macros) -> Result<()> {
        let loc = self.cursor.loc;
        let tok = self.next()?.ok_or(Error::UnexpectedEof)?;
        let name = match tok.kind() {
            TokenKind::Ident(name) => name,
            _ => return Err(Error::Unexpected(loc).into()),
        };
        let t = self.next()?.ok_or(Error::UnexpectedEof)?;
        if !t.leading_space() && matches!(t.kind(), TokenKind::Symbol(SymbolKind::OpeningParen)) {
            let body = self.read_define_func_macro()?;
            macros.add_func_macro(name, body);
        } else {
            self.unget(t);
            let body = self.read_define_obj_macro()?;
            macros.add_obj_macro(name, body);
        }
        Ok(())
    }

    fn read_define_func_macro(&mut self) -> Result<Vec<FuncMacroToken>> {
        let mut params = HashMap::new();
        while let Some(arg) = self.next()? {
            let arg = match arg.kind() {
                TokenKind::Ident(name) => name.to_owned(),
                TokenKind::Symbol(SymbolKind::ClosingParen) => break,
                TokenKind::Symbol(SymbolKind::Comma) if !params.is_empty() => continue,
                _ => return Err(Error::Unexpected(*arg.loc()).into()),
            };
            params.insert(arg, params.len());
        }
        let mut body = vec![];
        while let Some(t) = self.next()? {
            match t.kind() {
                TokenKind::NewLine => break,
                TokenKind::Ident(ref i) if params.contains_key(i) => {
                    body.push(FuncMacroToken::Param(params[i]));
                }
                _ => body.push(FuncMacroToken::Token(t)),
            }
        }
        Ok(body)
    }

    fn read_define_obj_macro(&mut self) -> Result<Vec<Token>> {
        let mut body = vec![];
        while let Some(t) = self.next()? {
            if matches!(t.kind(), TokenKind::NewLine) {
                break;
            }
            body.push(t);
        }
        Ok(body)
    }

    /// If `token` is defined as a macro, expands it and returns the first token of the macro.
    /// The remaining macro tokens are pushed back to the buffer.
    fn expand(&mut self, macros: &Macros, token: Token) -> Result<Token> {
        match token.kind() {
            TokenKind::Ident(ref name) if token.hideset().contains(name) => return Ok(token),
            TokenKind::Ident(ref name) => match macros.find(name) {
                Some(Macro::Obj(ref body)) => self
                    .expand_obj_macro(macros, name, body, *token.loc())
                    .map(|t| t.set_leading_space(token.leading_space())),
                Some(Macro::Func(ref body)) => self
                    .expand_func_macro(macros, name, body, *token.loc())
                    .map(|t| t.set_leading_space(token.leading_space())),
                None => Ok(token),
            },
            TokenKind::Keyword(_) => unreachable!(),
            _ => return Ok(token),
        }
    }

    /// Expands an object-like macro named `name` (whose body is `body`).
    fn expand_obj_macro(
        &mut self,
        macros: &Macros,
        name: &str,
        body: &[Token],
        loc: SourceLoc,
    ) -> Result<Token> {
        for t in body.into_iter().rev() {
            self.unget(
                t.clone()
                    .with_hideset_modified(|s| {
                        s.insert(name.into());
                    })
                    .with_loc(loc),
            );
        }
        let tok = self.next()?.unwrap();
        self.expand(macros, tok)
    }

    /// Expands an function-like macro named `name` (whose body is `body`).
    fn expand_func_macro(
        &mut self,
        macros: &Macros,
        name: &str,
        body: &[FuncMacroToken],
        loc: SourceLoc,
    ) -> Result<Token> {
        if !matches!(
            self.next()?,
            Some(tok) if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::OpeningParen))
        ) {
            return Err(Error::Unexpected(loc).into());
        }

        let mut args = vec![];
        let mut end = false;
        while !end {
            args.push(self.read_func_macro_arg(&mut end)?);
        }

        #[derive(Copy, Clone)]
        enum Substitution {
            Stringify,
            Concat,
            None,
        }

        let mut expanded = vec![];
        let mut subst = Substitution::None;

        fn append_expanded_tokens(
            expanded: &mut Vec<Token>,
            tokens: &[Token],
            loc: SourceLoc,
            s: &mut Substitution,
        ) {
            match s {
                Substitution::Stringify => {
                    expanded.push(Token::new(TokenKind::String(stringify(tokens, false)), loc));
                }
                Substitution::Concat => {
                    let mut toks = vec![expanded.pop().unwrap()];
                    toks.extend(tokens.iter().cloned());
                    expanded.push(Token::new(TokenKind::Ident(stringify(&toks, true)), loc));
                }
                Substitution::None => {
                    expanded.extend(tokens.iter().cloned());
                }
            }
            *s = Substitution::None;
        }

        for tok in body {
            match tok {
                FuncMacroToken::Token(tok)
                    if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::Hash)) =>
                {
                    match subst {
                        Substitution::None => subst = Substitution::Stringify,
                        Substitution::Stringify => subst = Substitution::Concat,
                        Substitution::Concat => subst = Substitution::None,
                    }
                    continue;
                }
                FuncMacroToken::Token(tok) => {
                    append_expanded_tokens(
                        &mut expanded,
                        std::slice::from_ref(tok),
                        loc,
                        &mut subst,
                    );
                }
                FuncMacroToken::Param(n) => {
                    if let Some(arg) = args.get(*n) {
                        append_expanded_tokens(&mut expanded, arg, loc, &mut subst);
                    } else {
                        return Err(Error::Unexpected(loc).into());
                    }
                }
            }
        }

        for tok in expanded.into_iter().rev() {
            let tok = tok
                .with_hideset_modified(|s| {
                    s.insert(name.into());
                })
                .with_loc(loc);
            self.unget(tok);
        }

        let tok = self.next()?.unwrap();
        self.expand(macros, tok)
    }

    fn read_func_macro_arg(&mut self, end: &mut bool) -> Result<Vec<Token>> {
        let mut nest = 0;
        let mut arg = vec![];
        while let Some(t) = self.next()? {
            match t.kind() {
                TokenKind::Symbol(SymbolKind::OpeningParen) => nest += 1,
                TokenKind::Symbol(SymbolKind::Comma) if nest == 0 => {
                    return Ok(arg);
                }
                TokenKind::Symbol(SymbolKind::ClosingParen) if nest == 0 => {
                    *end = true;
                    return Ok(arg);
                }
                TokenKind::Symbol(SymbolKind::ClosingParen) => nest -= 1,
                _ => {
                    arg.push(t);
                }
            }
        }
        Err(Error::UnexpectedEof.into())
    }

    /// Reads a #undef directive.
    fn read_undef(&mut self, macros: &mut Macros) -> Result<()> {
        let tok = self.next()?.ok_or(Error::UnexpectedEof)?;
        let name = tok.kind().as_ident().ok_or(Error::Unexpected(*tok.loc()))?;
        macros.remove(name);
        Ok(())
    }

    /// Reads a #if directive.
    fn read_if(&mut self, macros: &mut Macros) -> Result<()> {
        let cond = self.read_and_eval_constexpr(macros)?;
        self.do_if(cond)
    }

    /// Reads a #ifdef directive.
    fn read_ifdef(&mut self, macros: &mut Macros) -> Result<()> {
        let name = self.next()?.ok_or(Error::UnexpectedEof)?;
        let is_defined = name
            .kind()
            .as_ident()
            .map_or(false, |name| macros.find(name).is_some());
        self.do_if(is_defined)
    }

    /// Reads a #ifndef directive.
    fn read_ifndef(&mut self, macros: &mut Macros) -> Result<()> {
        let name = self.next()?.ok_or(Error::UnexpectedEof)?;
        let is_defined = name
            .kind()
            .as_ident()
            .map_or(false, |name| macros.find(name).is_some());
        self.do_if(!is_defined)
    }

    /// Reads a #elif directive.
    fn read_elif(&mut self, macros: &mut Macros) -> Result<()> {
        if *self.cond_stack.last().unwrap() || !self.read_and_eval_constexpr(macros)? {
            self.skip_group()?;
        } else {
            self.cond_stack.pop();
            self.cond_stack.push(true);
        }
        Ok(())
    }

    /// Reads a #else directive.
    fn read_else(&mut self) -> Result<()> {
        if *self.cond_stack.last().unwrap() {
            self.skip_group()?;
        }
        Ok(())
    }

    /// Skips the group if `cond` is false.
    fn do_if(&mut self, cond: bool) -> Result<()> {
        self.cond_stack.push(cond);
        if !cond {
            self.skip_group()?;
        }
        Ok(())
    }

    /// Skips the group.
    fn skip_group(&mut self) -> Result<()> {
        let mut nest = 0;
        while let Some(tok) = self.next()? {
            if tok.kind() != &SymbolKind::Hash.into() {
                continue;
            }
            let directive = self.next()?.ok_or(Error::UnexpectedEof)?;
            if nest == 0 {
                if matches!(
                    directive.kind().as_ident().map(String::as_str),
                    Some("else" | "elif" | "endif")
                ) {
                    self.unget(directive);
                    self.unget(tok);
                    return Ok(());
                }
            }
            match directive.kind().as_ident().map(String::as_str) {
                Some("if" | "ifdef" | "ifndef") => nest += 1,
                Some("endif") => nest -= 1,
                _ => {
                    assert!(self.buf.is_empty());
                    self.cursor.take_chars_while(|&c| c != '\n');
                }
            }
        }
        Err(Error::UnexpectedEof.into())
    }

    /// Reads an integer expression line and returns its evaluated value.
    fn read_and_eval_constexpr(&mut self, macros: &mut Macros) -> Result<bool> {
        let expr_line = self.read_intexpr_line(macros)?;
        let loc = *expr_line[0].loc();
        let expr = Parser::new(&mut expr_line.into_iter()).parse_expr()?;
        if let Some(e) = expr.eval_constexpr() {
            Ok(e != 0)
        } else {
            Err(Error::Message("The expression is invalid".into(), loc).into())
        }
    }

    /// Reads an integer expression line.
    fn read_intexpr_line(&mut self, macros: &mut Macros) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        while let Some(tok) = self.next()? {
            if tok.kind() == &TokenKind::NewLine {
                break;
            }
            let tok = self.expand(macros, tok)?;
            match tok.kind() {
                TokenKind::Ident(ident) if ident == "defined" => {
                    tokens.push(self.read_defined_op(macros)?)
                }
                TokenKind::Ident(_) => {
                    tokens.push(Token::new(IntKind::Int(0).into(), *tok.loc()));
                }
                _ => tokens.push(tok),
            }
        }
        Ok(tokens)
    }

    /// Reads a "defined" operator in an integer expression.
    fn read_defined_op(&mut self, macros: &mut Macros) -> Result<Token> {
        let mut tok = self.next()?.ok_or(Error::UnexpectedEof)?;
        if tok.kind() == &SymbolKind::OpeningParen.into() {
            tok = self.next()?.ok_or(Error::UnexpectedEof)?;
            let maybe_closing_paren = self.next()?.ok_or(Error::UnexpectedEof)?;
            if maybe_closing_paren.kind() != &SymbolKind::ClosingParen.into() {
                return Err(Error::Unexpected(*maybe_closing_paren.loc()).into());
            }
        }
        let name = tok.kind().as_ident().ok_or(Error::Unexpected(*tok.loc()))?;
        Ok(Token::new(
            IntKind::Int(macros.find(name).is_some().into()).into(),
            *tok.loc(),
        ))
    }

    /// Reads a #error directive.
    fn read_error(&mut self, loc: SourceLoc) -> Result<()> {
        let mut tokens = Vec::new();
        while let Some(tok) = self.next()? {
            if tok.kind() == &TokenKind::NewLine {
                break;
            }
            tokens.push(tok);
        }
        let msg = tokens.into_iter().fold("#error: ".to_string(), |acc, tok| {
            acc + if tok.leading_space() { " " } else { "" } + tok.kind().to_string().as_str()
        });
        Err(Error::Message(msg, loc).into())
    }

    /// Reads a #pragma directive.
    fn read_pragma(&mut self) -> Result<()> {
        // TODO: Don't just ignore the directive.
        let mut tokens = Vec::new();
        while let Some(tok) = self.next()? {
            if tok.kind() == &TokenKind::NewLine {
                break;
            }
            tokens.push(tok);
        }
        Ok(())
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[test]
fn test() {
    let _ = SourceLexer::new("int main() {}");
}

#[test]
#[should_panic]
fn cannot_open_file() {
    use sourceloc::source::*;
    let _ = SourceLexer::new_from_file("", &mut Sources::new()).unwrap();
}

#[test]
fn just_read_one_token() {
    let mut l = SourceLexer::new("int main() {}");
    assert!(matches!(
        l.next().unwrap().unwrap().kind(),
        TokenKind::Ident(i) if i == "int"
    ));
    assert!(l.cursor.pos == 3);
    let mut l = SourceLexer::new("__num");
    assert!(matches!(l.next().unwrap().unwrap().kind(), TokenKind::Ident(i) if i == "__num"));
    assert!(l.cursor.pos == 5)
}

#[test]
fn read_tokens() {
    let mut l = SourceLexer::new("int main\nlong short");
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_tokens2() {
    let mut l = SourceLexer::new("int main()");
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_tokens3() {
    let mut l = SourceLexer::new("int main() {\n return;\n}");
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_tokens4() {
    let mut l = SourceLexer::new("int main() {\n puts(\"hello\");\n}");
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_symbols() {
    let mut l = SourceLexer::new(
        "(){}[],;:.+-*/%!~&<>^|?=#++--::-><<>><=>===!=&&||+=-=*=/=%=&=^=|=<<=>>=&&=||=sizeof",
    );
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_ints() {
    let mut l =
        SourceLexer::new(".123 1e+10 34567890 123 4300000000 3.4l 2.71f 1.1 2. 0xfff 01727 33llu");
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_comments() {
    let mut l = SourceLexer::new("int // hello!\nmain(/*hello*/)");
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_macro() {
    let mut l = SourceLexer::new(
        r#"
#define ONE 1
int f(int x) { return x + ONE; }"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro2() {
    let mut l = SourceLexer::new(
        r#"
#define F(x, y) (x) + (y)
int f(int x, int y) { return F(x, y); }"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro3() {
    let mut l = SourceLexer::new(
        r#"
#define PUTS(x) puts(#x)
void main() { PUTS(1 *2+ 3); }"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro4() {
    let mut l = SourceLexer::new(
        r#"
#define CAT(x, y) x##y##123
void main() { int CAT(foo, bar); }"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro5() {
    let mut l = SourceLexer::new(
        r#"
#define HELLO "hello"
#define TEN 10
HELLO; TEN;
#undef TEN
HELLO; TEN;
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro6() {
    let mut l = SourceLexer::new(
        r#"
#define ONE 1
#if defined ONE
int f;
#endif
#undef ONE
#if defined ONE
int g;
#endif
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro7() {
    let mut l = SourceLexer::new(
        r#"
#define ONE 1
#define TWO 2
#ifdef ONE
int f;
#endif
#ifndef ONE
int g;
#elif TWO
int h;
#endif
#undef ONE
#ifndef ONE
int k;
#endif
#ifdef TWO
#endif
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro8() {
    let mut l = SourceLexer::new(
        r#"
#define ONE 1
#define one ONE
one
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_include() {
    let mut l = SourceLexer::new("#include <stdio.h>");
    assert!(matches!(
        l.next_preprocessed(&mut Macros::new()).unwrap_err().downcast_ref::<Error>().unwrap(),
        Error::Include(p, _) if p.to_str() == Some("stdio.h")
    ));
}

#[cfg(test)]
fn read_all_tokens(l: &mut SourceLexer) -> Vec<Token> {
    let mut tokens = vec![];
    while let Some(tok) = l.next().unwrap() {
        tokens.push(tok)
    }
    tokens
}

#[cfg(test)]
fn read_all_tokens_expanded(l: &mut SourceLexer) -> Vec<Token> {
    let mut tokens = vec![];
    let mut macros = Macros::new();
    while let Some(tok) = l.next_preprocessed(&mut macros).unwrap() {
        tokens.push(tok)
    }
    assert!(l.cond_stack.is_empty());
    tokens
}
