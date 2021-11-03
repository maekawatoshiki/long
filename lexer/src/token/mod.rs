pub mod kind;

use kind::TokenKind;
use long_sourceloc::SourceLoc;

#[derive(Debug, Clone)]
pub struct Token {
    /// The kind of the token.
    kind: TokenKind,

    /// The source location of the token.
    loc: SourceLoc,
}

impl Token {
    /// Creates a new `Token`.
    pub fn new(kind: TokenKind, loc: SourceLoc) -> Self {
        Self { kind, loc }
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
