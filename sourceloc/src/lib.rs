use std::fmt;

/// A source location of each token, AST node, ...etc.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceLoc {
    /// The line number, starting at 0 for the first line.
    line: u32,

    /// The column number within a line, starting at 1 for the first character of the line.
    column: u32,
}

impl SourceLoc {
    pub fn new(line: u32, column: u32) -> Self {
        assert!(column > 0);
        Self { line, column }
    }
}

impl fmt::Debug for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[test]
fn test() {
    let loc = SourceLoc::new(0, 1);
    assert_eq!(loc, SourceLoc::new(0, 1));
    let loc = SourceLoc::new(0xffffffff, 0xffffffff);
    assert_eq!(loc, SourceLoc::new(0xffffffff, 0xffffffff));
}

#[test]
#[should_panic]
fn column_must_be_greater_than_0() {
    let _ = SourceLoc::new(0, 0);
}