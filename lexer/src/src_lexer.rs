use crate::token::kind::TokenKind;
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

    /// The source code the lexer reads.
    source: String,

    /// The current position in `source`.
    pos: usize,

    /// The current source location.
    loc: SourceLoc,
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
            source: source.into(),
            pos: 0,
            loc: SourceLoc::new(0, 1),
        }
    }

    /// Creates a new `SourceLexer` for a file.
    pub fn new_from_file<P>(filepath: P) -> Result<Self>
    where
        P: Into<PathBuf> + Clone,
    {
        Ok(Self {
            source: read_to_string(filepath.clone().into())?,
            filepath: Some(filepath.into()),
            pos: 0,
            loc: SourceLoc::new(0, 1),
        })
    }

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        self.read_identifier()
    }

    /// Reads an identifier. Assumes the character at `self.pos` is alphabetic.
    pub fn read_identifier(&mut self) -> Result<Option<Token>> {
        let loc = self.loc;
        let source = &self.source[self.pos..];
        if source.is_empty() {
            return Ok(None);
        }

        let ident: String = source
            .chars()
            .take_while(|c| c.is_ascii_alphabetic())
            .collect();
        self.pos += ident.len();
        *self.loc.column_mut() += ident.len() as u32;

        Ok(Some(Token::new(TokenKind::Ident(ident), loc)))
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
    assert!(l.pos == 3)
}
