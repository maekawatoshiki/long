pub mod kind;

use kind::TokenKind;
use long_sourceloc::SourceLoc;

#[derive(Debug, Clone)]
pub struct Token {
    /// The kind of the token.
    pub(crate) kind: TokenKind,

    /// `Some(...)` if any leading spaces occurs before the token.
    /// Line terminator characters are not leading spaces.
    pub(crate) leading_space: bool,

    /// The source location of the token.
    pub(crate) loc: SourceLoc,
}

impl Token {
    /// Creates a new `Token`.
    pub fn new(kind: TokenKind, loc: SourceLoc) -> Self {
        Self {
            kind,
            leading_space: false,
            loc,
        }
    }

    pub fn set_leading_space(mut self, leading_space: bool) -> Self {
        self.leading_space = leading_space;
        self
    }

    /// Returns the kind of the token.
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    /// Returns the source location of the token.
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }
}
