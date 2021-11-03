use std::path::PathBuf;

pub mod token;

extern crate long_sourceloc as sourceloc;

/// A lexical analyzer for a translation unit.
pub struct Lexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,
}

impl Lexer {
    /// Creates a new `Lexer`.
    pub fn new(_source: String) -> Self {
        Self { filepath: None }
    }

    /// Creates a new `Lexer` for a file.
    pub fn new_from_file(filepath: impl Into<PathBuf>) -> Self {
        Self {
            filepath: Some(filepath.into()),
        }
    }

    /// Returns the file path.
    pub fn filepath(&self) -> Option<&PathBuf> {
        self.filepath.as_ref()
    }
}

#[test]
fn test() {
    let l = Lexer::new_from_file("hello.c");
    assert_eq!(l.filepath().unwrap().to_str().unwrap(), "hello.c")
}
