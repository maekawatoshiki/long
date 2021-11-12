pub mod cursor;
pub mod macros;
pub mod src_lexer;
pub mod traits;

extern crate anyhow;
extern crate long_ast as ast;
extern crate long_sourceloc as sourceloc;

use anyhow::Result;
use ast::token::kind::TokenKind;
use ast::token::Token;
use macros::Macros;
use sourceloc::SourceLoc;
use src_lexer::SourceLexer;
use std::fmt;
use std::path::{Path, PathBuf};

/// A lexical analyzer for a translation unit.
pub struct Lexer {
    /// The file path of the source file the lexer originally reads.
    /// Set to `None` if the lexer does not read a file but a string.
    filepath: Option<PathBuf>,

    /// The source lexers.
    src_lexers: Vec<SourceLexer>,

    /// The macros defined in the translation unit.
    macros: Macros,
}

#[derive(Debug)]
pub enum Error {
    FileNotFound(PathBuf, SourceLoc),
    Unexpected(SourceLoc),
    UnexpectedEof,
}

impl Lexer {
    /// Creates a new `Lexer`.
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            filepath: None,
            src_lexers: vec![SourceLexer::new(source.into())],
            macros: Macros::new(),
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
            macros: Macros::new(),
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
        match self
            .src_lexers
            .last_mut()
            .unwrap()
            .next_preprocessed(&mut self.macros)
        {
            // End of the translation unit.
            Ok(None) if self.src_lexers.len() == 1 => Ok(None),
            // End of the current source lexer. Go back to the previous one.
            Ok(None) => {
                self.src_lexers.pop().unwrap();
                self.next()
            }
            Ok(tok) => Ok(tok),
            Err(e) => {
                use src_lexer::Error as SError;
                match e.downcast_ref::<src_lexer::Error>() {
                    Some(e) => match e {
                        // When the source lexer reads #include directive, we create a new source
                        // lexer for that.
                        SError::Include(filepath, loc) => {
                            self.src_lexers.push(SourceLexer::new_from_file(
                                match try_include(filepath) {
                                    Some(path) => path,
                                    None => {
                                        return Err(
                                            Error::FileNotFound(filepath.clone(), *loc).into()
                                        )
                                    }
                                },
                            )?);
                            self.next()
                        }
                        SError::Unexpected(loc) => Err(Error::Unexpected(*loc).into()),
                        SError::UnexpectedEof => Err(Error::UnexpectedEof.into()),
                    },
                    None => Err(e),
                }
            }
        }
    }
}

impl traits::LexerLike for Lexer {
    fn next(&mut self) -> Result<Option<Token>> {
        self.next()
    }

    fn skip(&mut self, kind: TokenKind) -> bool {
        if let Ok(Some(tok)) = Lexer::next(self) {
            if tok.kind() == &kind {
                return true;
            }
        }
        false
    }

    fn filepath(&self) -> Option<&PathBuf> {
        self.filepath()
    }
}

fn try_include(filepath: &PathBuf) -> Option<PathBuf> {
    let header_paths = vec![
        "./include/",
        "/include/",
        "/usr/include/",
        "/usr/include/linux/",
        "/usr/include/c++/7/",
        "/usr/include/x86_64-linux-gnu/",
        "./include/",
        "",
    ];
    header_paths.iter().find_map(|header_path| {
        let mut path = Path::new(header_path).to_path_buf();
        path.push(filepath);
        if path.exists() {
            Some(path)
        } else {
            None
        }
    })
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
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
