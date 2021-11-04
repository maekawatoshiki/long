use crate::cursor::Cursor;
use crate::token::kind::TokenKind;
use crate::token::Token;
use anyhow::Result;
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

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        match self.cursor.peek_char() {
            Some(c) if c.is_ascii_alphabetic() || c == '_' => Ok(Some(self.read_identifier())),
            Some(c) if c == ' ' || c == '\t' || c == '\n' => {
                let leading_space = !self.read_whitespaces().ends_with('\n');
                self.next()
                    .map(|t| t.map(|t| t.set_leading_space(leading_space)))
            }
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
        Token::new(TokenKind::Ident(ident), loc)
    }

    /// Reads whitespaces (i.e. ' ' '\t' '\n').
    fn read_whitespaces(&mut self) -> String {
        self.cursor
            .take_chars_while(|&c| c == ' ' || c == '\t' || c == '\n')
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
    assert!(matches!(l.next().unwrap().unwrap().kind(), TokenKind::Ident(i) if i == "int"));
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

#[cfg(test)]
fn read_all_tokens(l: &mut SourceLexer) -> Vec<Token> {
    let mut tokens = vec![];
    while let Some(tok) = l.next().unwrap() {
        tokens.push(tok)
    }
    tokens
}
