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
    pub fn new_from_file<P>(filepath: P) -> Result<Self>
    where
        P: Into<PathBuf> + Clone,
    {
        Ok(Self {
            src_lexers: vec![SourceLexer::new_from_file(filepath.clone().into())?],
            filepath: Some(filepath.into()),
        })
    }

    /// Returns the file path.
    pub fn filepath(&self) -> Option<&PathBuf> {
        self.filepath.as_ref()
    }

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        if self.src_lexers.is_empty() {
            todo!()
        }

        self.src_lexers.last_mut().unwrap().next()
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

    pub fn next(&mut self) -> Result<Option<Token>> {
        todo!()
    }
}

#[test]
fn test() {
    let l = Lexer::new("int main(){}");
    assert!(l.filepath().is_none());
}

#[test]
#[should_panic]
fn test2() {
    let _ = Lexer::new_from_file("").unwrap();
}
