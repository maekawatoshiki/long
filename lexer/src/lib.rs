pub mod token;

extern crate anyhow;
extern crate long_sourceloc as sourceloc;

use anyhow::Result;
use std::{fs::read_to_string, path::PathBuf};
use token::Token;

/// A lexical analyzer for a translation unit.
pub struct Lexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,

    /// The source lexers.
    src_lexers: Vec<SourceLexer>,
}

/// A lexical analyzer for either a single source file or a string.
struct SourceLexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,

    /// The source code the lexer reads.
    source: String,
}

impl Lexer {
    /// Creates a new `Lexer`.
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            filepath: None,
            src_lexers: vec![SourceLexer::new(source.into())],
        }
    }

    /// Creates a new `Lexer` for a file.
    pub fn new_from_file<P>(filepath: P) -> Self
    where
        P: Into<PathBuf> + Clone,
    {
        Self {
            filepath: Some(filepath.into()),
            src_lexers: vec![],
        }
    }

    /// Returns the file path.
    pub fn filepath(&self) -> Option<&PathBuf> {
        self.filepath.as_ref()
    }

    /// Reads a token.
    pub fn next(&mut self) -> Result<Token> {
        todo!()
    }
}

impl SourceLexer {
    /// Creates a new `SourceLexer`.
    pub fn new(source: String) -> Self {
        Self {
            filepath: None,
            source,
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
        })
    }
}

#[test]
fn test() {
    let l = Lexer::new_from_file("hello.c");
    assert_eq!(l.filepath().unwrap().to_str().unwrap(), "hello.c");
    let l = Lexer::new("int main(){}");
    assert!(l.filepath().is_none());
}
