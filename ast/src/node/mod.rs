use long_sourceloc::SourceLoc;

pub mod decl;
pub mod expr;
pub mod lit;
pub mod stmt;
pub mod ty;

/// Any kind of node with source location.
#[derive(Debug, Clone)]
pub struct Located<T> {
    /// Any kind of node.
    pub inner: T,

    /// The source location.
    pub loc: SourceLoc,
}

impl<T> Located<T> {
    /// Creates a new `Located`.
    pub fn new(inner: T, loc: SourceLoc) -> Self {
        Located { inner, loc }
    }

    /// Returns the inner node.
    pub fn inner(self) -> T {
        self.inner
    }

    /// Returns the reference to the inner node.
    pub fn inner_ref(&self) -> &T {
        &self.inner
    }

    /// Returns the source location.
    pub fn loc(&self) -> SourceLoc {
        self.loc
    }
}
