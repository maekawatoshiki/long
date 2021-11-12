pub mod kind;

use std::collections::HashSet;

use kind::TokenKind;
use long_sourceloc::SourceLoc;

#[derive(Debug, Clone)]
pub struct Token {
    /// The kind of the token.
    kind: TokenKind,

    /// `Some(...)` if any leading spaces occurs before the token.
    /// Line terminator characters are not leading spaces.
    leading_space: bool,

    /// The hideset of the token.
    // TODO: We should create `PPToken` (with a hideset) to simplify code.
    hideset: HashSet<String>,

    /// The source location of the token.
    loc: SourceLoc,
}

impl Token {
    /// Creates a new `Token`.
    #[inline]
    pub fn new(kind: TokenKind, loc: SourceLoc) -> Self {
        Self {
            kind,
            leading_space: false,
            hideset: HashSet::new(),
            loc,
        }
    }

    /// Sets the kind of the token and returns the token.
    #[inline]
    pub fn with_kind(mut self, kind: TokenKind) -> Self {
        self.kind = kind;
        self
    }

    /// Sets a leading space flag and returns the token.
    #[inline]
    pub fn set_leading_space(mut self, leading_space: bool) -> Self {
        self.leading_space = leading_space;
        self
    }

    /// Sets the hideset and returns the token.
    #[inline]
    pub fn with_hideset(mut self, hideset: HashSet<String>) -> Self {
        self.hideset = hideset;
        self
    }

    /// Modifies the hideset by `f` and returns the token.
    #[inline]
    pub fn with_hideset_modified<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut HashSet<String>),
    {
        f(&mut self.hideset);
        self
    }

    /// Sets the source location and returns the token.
    #[inline]
    pub fn with_loc(mut self, loc: SourceLoc) -> Self {
        self.loc = loc;
        self
    }

    /// Returns the kind of the token.
    #[inline]
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    /// Returns the source location of the token.
    #[inline]
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }

    /// Returns the leading space of the token.
    #[inline]
    pub fn leading_space(&self) -> bool {
        self.leading_space
    }

    /// Returns the hideset of the token.
    #[inline]
    pub fn hideset(&self) -> &HashSet<String> {
        &self.hideset
    }

    /// Converts the token to a string.
    #[inline]
    pub fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

/// Converts the token stream to a string.
pub fn stringify(tokens: &[Token], ignore_leading_space: bool) -> String {
    tokens
        .iter()
        .fold("".to_string(), |acc, tok| {
            format!(
                "{}{}{}",
                acc,
                if !ignore_leading_space && tok.leading_space {
                    " "
                } else {
                    ""
                },
                tok.to_string()
            )
        })
        .trim_start() // Remove a leading space.
        .to_string()
}
