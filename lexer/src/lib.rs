pub mod src_lexer;
pub mod token;

extern crate anyhow;
extern crate long_sourceloc as sourceloc;

use anyhow::Result;
use src_lexer::SourceLexer;
use std::path::PathBuf;
use token::Token;

/// A lexical analyzer for a translation unit.
pub struct Lexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,

    /// The source lexers.
    src_lexers: Vec<SourceLexer>,
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

        // TODO: We had better not use recursions here...
        match self.src_lexers.last_mut().unwrap().next() {
            // End of the translation unit.
            Ok(None) if self.src_lexers.len() == 1 => Ok(None),
            // End of the current source lexer. Go back to the previous one.
            Ok(None) => {
                self.src_lexers.pop().unwrap();
                self.next()
            }
            Ok(tok) => Ok(tok),
            Err(e) => {
                use src_lexer::Error;
                match e.downcast_ref::<src_lexer::Error>() {
                    Some(e) => match e {
                        // When the source lexer reads #include directive, we create a new source
                        // lexer for that.
                        Error::Include(filepath) => {
                            self.src_lexers.push(SourceLexer::new_from_file(filepath)?);
                            self.next()
                        }
                    },
                    None => Err(e),
                }
            }
        }
    }
}

#[test]
fn test() {
    let l = Lexer::new("int main(){}");
    assert!(l.filepath().is_none());
}

#[test]
#[should_panic]
fn cannot_open_file() {
    let _ = Lexer::new_from_file("").unwrap();
}
