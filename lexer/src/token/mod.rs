pub mod kind;

use std::collections::HashSet;

use kind::TokenKind;
use long_sourceloc::SourceLoc;

#[derive(Debug, Clone)]
pub struct Token {
    /// The kind of the token.
    pub(crate) kind: TokenKind,

    /// `Some(...)` if any leading spaces occurs before the token.
    /// Line terminator characters are not leading spaces.
    pub(crate) leading_space: bool,

    /// The hideset of the token.
    // TODO: We should create `PPToken` (with a hideset) to simplify code.
    pub(crate) hideset: HashSet<String>,

    /// The source location of the token.
    pub(crate) loc: SourceLoc,
}

impl Token {
    /// Creates a new `Token`.
    pub fn new(kind: TokenKind, loc: SourceLoc) -> Self {
        Self {
            kind,
            leading_space: false,
            hideset: HashSet::new(),
            loc,
        }
    }

    /// Sets a leading space flag and returns the token.
    pub fn set_leading_space(mut self, leading_space: bool) -> Self {
        self.leading_space = leading_space;
        self
    }

    /// Sets the hideset and returns the token.
    pub fn with_hideset(mut self, hideset: HashSet<String>) -> Self {
        self.hideset = hideset;
        self
    }

    /// Modifies the hideset by `f` and returns the token.
    pub fn with_hideset_modified<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut HashSet<String>),
    {
        f(&mut self.hideset);
        self
    }

    /// Sets the source location and returns the token.
    pub fn with_loc(mut self, loc: SourceLoc) -> Self {
        self.loc = loc;
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
