use super::cursor::Cursor;
use super::macros::{FuncMacroToken, Macro, Macros};
use super::traits::TokenStream;
use crate::Parser;
use anyhow::Result;
use ast::token::kind::{FloatKind, IntKind, KeywordKind, SymbolKind, TokenKind};
use ast::token::{stringify, Token};
use long_sourceloc::SourceLoc;
use sourceloc::source::{Source, Sources};
use std::collections::HashMap;
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
    buf: Vec<Token>,

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
            buf: Vec::new(),
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
            buf: Vec::new(),
            cond_stack: Vec::new(),
        })
    }

    /// Creates a new `SourceLexer` from tokens.
    pub fn new_from_tokens(buf: impl Into<Vec<Token>>) -> Self {
        Self {
            cursor: Cursor::new("".to_string()),
            filepath: None,
            buf: buf.into().into_iter().rev().collect(),
            cond_stack: Vec::new(),
        }
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
                    Some(kind) => tok.with_kind(TokenKind::Keyword(kind)).with_empty_hideset(),
                    None => tok,
                },
                _ => tok,
            }
        }

        match self.next()? {
            Some(tok) if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::Hash)) => {
                self.read_cpp_directive(macros)?;
                self.next_preprocessed(macros)
            }
            Some(tok) if tok.kind() == &TokenKind::NewLine => self.next_preprocessed(macros),
            Some(tok) => Ok(self.expand(macros, tok)?.map(to_keyword_if_necessary)),
            None => Ok(None),
        }
    }

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        if !self.buf.is_empty() {
            return Ok(self.buf.pop());
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
            Some(c) if c == '\'' || (c == 'L' && self.cursor.peek_char2() == Some('\'')) => {
                Ok(Some(self.read_char()?))
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
            None => Ok(None),
        }
    }

    /// Reads a token (new lines are skipped).
    pub fn next_skipping_newline(&mut self) -> Result<Option<Token>> {
        match self.next()? {
            Some(tok) if tok.kind() == &TokenKind::NewLine => self.next_skipping_newline(),
            tok => Ok(tok),
        }
    }

    /// Pushes `tok` back to the buffer so that it can be read again.
    pub fn unget(&mut self, tok: Token) {
        self.buf.push(tok);
    }

    /// Pushes `tokens` back to the buffer so that they can be read again.
    pub fn unget_tokens<I>(&mut self, tokens: I)
    where
        I: IntoIterator<Item = Token>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        self.buf.extend(tokens.into_iter().rev());
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
            if c != '\\' {
                string.push(c);
                continue;
            }
            string.push(self.escape_char(c)?);
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
        } else if lit.starts_with('0') {
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
            Err(Error::Message("Too big integer".into(), loc).into())
        }
    }

    /// Reads a character literal.
    fn read_char(&mut self) -> Result<Token> {
        let loc = self.cursor.loc;
        // Just ignore the prefix.
        // TODO: Support prefix.
        if self.cursor.peek_char().map_or(false, char::is_alphabetic) {
            self.cursor.next_char();
        }
        assert_eq!(self.cursor.next_char(), Some('\''));
        let c = self.cursor.next_char().ok_or(Error::Unexpected(loc))?;
        let c = self.escape_char(c)?;
        if self.cursor.next_char().ok_or(Error::Unexpected(loc))? != '\'' {
            return Err(Error::Unexpected(loc).into());
        }
        Ok(Token::new(TokenKind::Char(c), loc))
    }

    /// If the given `c` is `\`, reads one or more characters to process the escape sequence.
    /// Otherwise, just returns the given `c`.
    fn escape_char(&mut self, c: char) -> Result<char> {
        if c != '\\' {
            return Ok(c);
        }

        let loc = self.cursor.loc;
        let c = self.cursor.next_char().ok_or(Error::Unexpected(loc))?;
        match c {
            '\'' | '"' | '?' | '\\' => Ok(c),
            'a' => Ok('\x07'),
            'b' => Ok('\x08'),
            'f' => Ok('\x0c'),
            'n' => Ok('\x0a'),
            'r' => Ok('\x0d'),
            't' => Ok('\x09'),
            'v' => Ok('\x0b'),
            'x' => {
                let hex = self
                    .cursor
                    .take_chars_while(|c| matches!(c, '0'..='9' | 'a'..='f'|'A'..='F'));
                Ok(
                    char::from_u32(u32::from_str_radix(&hex, 16).expect("Can't parse as a hex"))
                        .expect("Can't treat as a char"),
                )
            }
            '0'..='7' => {
                let mut oct = self.cursor.take_chars_while(|c| matches!(c, '0'..='7'));
                oct.insert(0, c);
                Ok(
                    char::from_u32(u32::from_str_radix(&oct, 8).expect("Can't parse as a oct"))
                        .expect("Can't treat as a char"),
                )
            }
            _ => Ok(c),
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
                "include" | "include_next" => {
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
                e => Err(Error::Message(
                    format!("Unsupported preprocessor directive {}", e),
                    *tok.loc(),
                )
                .into()),
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
                TokenKind::Symbol(SymbolKind::Vararg) => "__VA_ARGS__".into(),
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
                    let idx = params[i];
                    body.push(if i == "__VA_ARGS__" {
                        FuncMacroToken::Vararg(t, idx)
                    } else {
                        FuncMacroToken::Param(t, idx)
                    })
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
    fn expand(&mut self, macros: &Macros, token: Token) -> Result<Option<Token>> {
        match token.kind() {
            TokenKind::Ident(ref name) if token.hideset().contains(name) => Ok(Some(token)),
            TokenKind::Ident(ref name) => match macros.find(name) {
                Some(Macro::Obj(ref body)) => self
                    .expand_obj_macro(macros, name, body, *token.loc())
                    .map(|t| t.map(|t| t.set_leading_space(token.leading_space()))),
                Some(Macro::Func(ref body)) => self
                    .expand_func_macro(macros, name, body, *token.loc())
                    .map(|t| t.map(|t| t.set_leading_space(token.leading_space()))),
                None => Ok(Some(token)),
            },
            // TokenKind::Keyword(_) => unreachable!(),
            _ => Ok(Some(token)),
        }
    }

    /// Expands an object-like macro named `name` (whose body is `body`).
    fn expand_obj_macro(
        &mut self,
        macros: &Macros,
        name: &str,
        body: &[Token],
        loc: SourceLoc,
    ) -> Result<Option<Token>> {
        self.unget_tokens(body.iter().cloned().map(|t| {
            t.with_hideset_modified(|s| {
                s.insert(name.into());
            })
            .with_loc(loc)
        }));
        self.next()?.map_or(Ok(None), |t| self.expand(macros, t))
    }

    /// Expands an function-like macro named `name` (whose body is `body`).
    fn expand_func_macro(
        &mut self,
        macros: &Macros,
        name: &str,
        body: &[FuncMacroToken],
        loc: SourceLoc,
    ) -> Result<Option<Token>> {
        if !matches!(
            self.next_skipping_newline()?,
            Some(tok) if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::OpeningParen))
        ) {
            return Ok(Some(Token::new(TokenKind::Ident(name.into()), loc)));
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

        fn expand(tokens: Vec<Token>, macros: &Macros) -> Result<Vec<Token>> {
            let mut lex = SourceLexer::new_from_tokens(tokens);
            let mut new_tokens = vec![];
            while let Some(t) = lex.next()? {
                if let Some(t) = lex.expand(macros, t)? {
                    new_tokens.push(t)
                }
            }
            Ok(new_tokens)
        }

        fn append_expanded_tokens(
            expanded: &mut Vec<Token>,
            tmpl: &Token,
            tokens: &[Token],
            loc: SourceLoc,
            s: &mut Substitution,
        ) {
            match s {
                Substitution::Stringify => {
                    expanded.push(
                        Token::new(TokenKind::String(stringify(tokens, false)), loc)
                            .set_leading_space(tmpl.leading_space()),
                    );
                }
                Substitution::Concat => {
                    let mut toks = vec![];
                    if let Some(last_tok) = expanded.pop() {
                        toks.push(last_tok)
                    }
                    toks.extend(tokens.iter().cloned());
                    if !toks.is_empty() {
                        expanded.push(
                            Token::new(TokenKind::Ident(stringify(&toks, true)), loc)
                                .set_leading_space(toks[0].leading_space()),
                        );
                    }
                }
                Substitution::None => {
                    expanded.extend(tokens.iter().cloned().enumerate().map(|(i, t)| {
                        if i == 0 {
                            t.set_leading_space(tmpl.leading_space())
                        } else {
                            t
                        }
                    }))
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
                        tok,
                        std::slice::from_ref(tok),
                        loc,
                        &mut subst,
                    );
                }
                FuncMacroToken::Param(tok, n) => {
                    if let Some(arg) = args.get(*n) {
                        // Expand the arg in advance.
                        append_expanded_tokens(
                            &mut expanded,
                            tok,
                            &expand(arg.clone(), macros)?,
                            loc,
                            &mut subst,
                        );
                    } else {
                        return Err(Error::Unexpected(loc).into());
                    }
                }
                FuncMacroToken::Vararg(tok, mut n) => {
                    while let Some(arg) = args.get(n) {
                        append_expanded_tokens(&mut expanded, tok, arg, loc, &mut subst);
                        if n < args.len() - 1 {
                            expanded.push(Token::new(SymbolKind::Comma.into(), loc))
                        }
                        n += 1
                    }
                }
            }
        }

        self.unget_tokens(expanded.iter().cloned().map(|t| {
            t.with_hideset_modified(|s| {
                s.insert(name.into());
            })
            .with_loc(loc)
        }));

        self.next()?.map_or(Ok(None), |t| self.expand(macros, t))
    }

    fn read_func_macro_arg(&mut self, end: &mut bool) -> Result<Vec<Token>> {
        let mut nest = 0;
        let mut arg = vec![];
        while let Some(t) = self.next_skipping_newline()? {
            match t.kind() {
                TokenKind::Symbol(SymbolKind::OpeningParen) => {
                    arg.push(t);
                    nest += 1
                }
                TokenKind::Symbol(SymbolKind::Comma) if nest == 0 => {
                    return Ok(arg);
                }
                TokenKind::Symbol(SymbolKind::ClosingParen) if nest == 0 => {
                    *end = true;
                    return Ok(arg);
                }
                TokenKind::Symbol(SymbolKind::ClosingParen) => {
                    arg.push(t);
                    nest -= 1
                }
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
        let name = tok
            .kind()
            .as_ident()
            .ok_or_else(|| Error::Unexpected(*tok.loc()))?;
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
            if nest == 0
                && matches!(
                    directive.kind().as_ident().map(String::as_str),
                    Some("else" | "elif" | "endif")
                )
            {
                self.unget(directive);
                self.unget(tok);
                return Ok(());
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
        let expr = Parser::new(&mut TokenStream::from(expr_line)).parse_expr()?;
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
            if let Some(tok) = self.expand(macros, tok)? {
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
        }
        // let msg = tokens.iter().fold("#error: ".to_string(), |acc, tok| {
        //     acc + if tok.leading_space() { " " } else { "" } + tok.kind().to_string().as_str()
        // });
        // println!(">> {}", msg);
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
        let name = tok
            .kind()
            .as_ident()
            .ok_or_else(|| Error::Unexpected(*tok.loc()))?;
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
fn read_chars() {
    let mut l = SourceLexer::new(
        r#"'a' 'b''c' '\\' '\a' '\'' '\"' '\?' '\b' '\b' '\f' '\n' '\r' '\t' '\v'
        '\xfff' '\x0' '\x1' '\x1Fc' '\0' '\321'"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens(&mut l));
}

#[test]
fn read_string() {
    let mut l = SourceLexer::new(
        r#"
    "hello world!";
    "new\nline"
        "#,
    );
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
fn read_macro9() {
    let mut l = SourceLexer::new(
        r#"
#define F(x, ...) f(x, __VA_ARGS__)
F(1, 2, 3)
#define G(...) g(__VA_ARGS__)
G(1,2,3)
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro10() {
    let mut l = SourceLexer::new(
        r#"
#define F(ty, name, ...) ty name (__VA_ARGS__)
F(int, f, int x, int y)
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_macro11() {
    let mut l = SourceLexer::new(
        r#"
#define x       3
#define f(a)    f(x * (a))
#undef  x
#define x       2
#define g       f
#define z       z[0]
#define h       g(~
#define m(a)    a(w)
#define w       0,1
#define t(a)    a
#define p()     int
#define q(x)    x
#define r(x,y)  x ## y
#define str(x)  # x

f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);
g(x+(3,4)-w) | h 5) & m
    (f)^m(m);
p() i[q()] = { q(1), r(2,3), r(4,), r(,5), r(,) };
char c[2][6] = { str(hello), str() };
"#,
    );
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l)
        .into_iter()
        .map(|t| t.with_empty_hideset())
        .collect::<Vec<_>>());
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
