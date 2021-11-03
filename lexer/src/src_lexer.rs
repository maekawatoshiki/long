use crate::token::Token;
use anyhow::Result;
use std::fmt;
use std::fs::read_to_string;
use std::path::PathBuf;

/// A lexical analyzer for either a single source file or a string.
pub(crate) struct SourceLexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,

    /// The source code the lexer reads.
    source: String,
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

    /// Reads a token.
    pub fn next(&mut self) -> Result<Option<Token>> {
        todo!()
    }
}

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
#[should_panic]
fn not_implemented() {
    let mut l = SourceLexer::new("int main() {}");
    l.next().unwrap();
}
