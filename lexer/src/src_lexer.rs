use crate::cursor::Cursor;
use crate::token::kind::{FloatKind, IntKind, KeywordKind, SymbolKind, TokenKind};
use crate::token::Token;
use anyhow::Result;
use long_sourceloc::SourceLoc;
use std::fmt;
use std::fs::read_to_string;
use std::path::PathBuf;

/// A lexical analyzer for either a single source file or a string.
pub(crate) struct SourceLexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    #[allow(dead_code)]
    filepath: Option<PathBuf>,

    /// The cursor of the lexer.
    cursor: Cursor,
}

#[derive(Debug)]
pub(crate) enum Error {
    Include(PathBuf),
    Unexpected(SourceLoc),
}

impl SourceLexer {
    /// Creates a new `SourceLexer`.
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            filepath: None,
            cursor: Cursor::new(source.into()),
        }
    }

    /// Creates a new `SourceLexer` for a file.
    pub fn new_from_file<P>(filepath: P) -> Result<Self>
    where
        P: Into<PathBuf> + Clone,
    {
        Ok(Self {
            cursor: Cursor::new(read_to_string(filepath.clone().into())?),
            filepath: Some(filepath.into()),
        })
    }

    /// Reads a token and preprocess it if necessary.
    pub fn next_preprocessed(&mut self) -> Result<Option<Token>> {
        let tok = match self.next()? {
            Some(tok) if matches!(tok.kind(), TokenKind::Symbol(SymbolKind::Hash)) => {
                self.read_cpp_directive()?;
                return self.next_preprocessed();
            }
            Some(tok) => tok,
            None => return Ok(None),
        };
        // TODO: Macro-expand `tok` here.
        Ok(Some(tok))
    }

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        match self.cursor.peek_char() {
            Some(c) if c.is_ascii_alphabetic() || c == '_' => Ok(Some(self.read_identifier())),
            Some(c) if c == ' ' || c == '\t' || c == '\n' => {
                let leading_space = !self.read_whitespaces().ends_with('\n');
                self.next()
                    .map(|t| t.map(|t| t.set_leading_space(leading_space)))
            }
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
            Some(c) if SymbolKind::at_least_starts_with(c) => Ok(Some(self.read_symbol())),
            None => return Ok(None),
            _ => todo!(),
        }
    }

    /// Reads an identifier. Assumes the result of `self.cursor.peek_char()` is alphabetic.
    fn read_identifier(&mut self) -> Token {
        let loc = self.cursor.loc;
        let ident = self
            .cursor
            .take_chars_while(|&c| c.is_ascii_alphanumeric() || c == '_');
        KeywordKind::from_str(ident.as_str()).map_or_else(
            || Token::new(TokenKind::Ident(ident), loc),
            |kind| Token::new(TokenKind::Keyword(kind), loc),
        )
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
            (&lit[1..], 8)
        } else {
            (lit.as_str(), 10)
        };
        // TODO: Support suffix.
        let lit = lit.trim_end_matches(|c| matches!(c, 'u' | 'U' | 'l' | 'L'));
        if let Ok(i) = i32::from_str_radix(lit, radix) {
            Ok(Token::new(TokenKind::Int(IntKind::Int(i)), loc))
        } else if let Ok(i) = i64::from_str_radix(lit, radix) {
            Ok(Token::new(TokenKind::Int(IntKind::LongLongInt(i)), loc))
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

    /// Reads whitespaces (i.e. ' ' '\t' '\n').
    fn read_whitespaces(&mut self) -> String {
        self.cursor
            .take_chars_while(|&c| c == ' ' || c == '\t' || c == '\n')
    }

    // Functions for preprocess.

    /// Reads a preprocessor directive.
    fn read_cpp_directive(&mut self) -> Result<()> {
        let tok = match self.next()? {
            Some(tok) => tok,
            None => return Ok(()),
        };
        match tok.kind() {
            TokenKind::Ident(i) => match i.as_str() {
                "include" => {
                    let name = self.read_header_name()?;
                    Err(Error::Include(name.into()).into())
                }
                _ => Ok(()),
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
                Ok(name)
            }
            _ => Err(Error::Unexpected(loc).into()),
        }
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
    let _ = SourceLexer::new_from_file("").unwrap();
}

#[test]
fn just_read_one_token() {
    let mut l = SourceLexer::new("int main() {}");
    assert!(matches!(
        l.next().unwrap().unwrap().kind(),
        TokenKind::Keyword(KeywordKind::Int)
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
    // TODO: We must support #define.
    let mut l = SourceLexer::new("int f(int x) { return x + 1; }");
    insta::assert_debug_snapshot!(read_all_tokens_expanded(&mut l));
}

#[test]
fn read_include() {
    let mut l = SourceLexer::new("#include <stdio.h>");
    assert!(matches!(
        l.next_preprocessed().unwrap_err().downcast_ref::<Error>().unwrap(),
        Error::Include(p) if p.to_str() == Some("stdio.h")
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
    while let Some(tok) = l.next_preprocessed().unwrap() {
        tokens.push(tok)
    }
    tokens
}
