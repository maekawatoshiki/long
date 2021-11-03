use std::path::PathBuf;

pub mod token;

extern crate long_sourceloc as sourceloc;

/// A lexical analyzer for a translation unit.
pub struct Lexer {
    /// The file path of the source file the lexer originally reads.
    filepath: PathBuf,
}

impl Lexer {
    /// Creates a new `Lexer`.
    pub fn new(filepath: impl Into<PathBuf>) -> Self {
        Self {
            filepath: filepath.into(),
        }
    }

    /// Returns the file path.
    pub fn filepath(&self) -> &PathBuf {
        &self.filepath
    }
}

#[test]
fn test() {
    let l = Lexer::new("hello.c");
    assert_eq!(l.filepath().to_str().unwrap(), "hello.c")
}
