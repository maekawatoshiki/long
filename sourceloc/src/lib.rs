pub mod source;

use source::SourceId;
use std::fmt;

/// A source location of each token, AST node, ...etc.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceLoc {
    /// The source id.
    source_id: Option<SourceId>,

    /// The line number, starting at 0 for the first line.
    line: u32,

    /// The column number within a line, starting at 1 for the first character of the line.
    column: u32,
}

impl SourceLoc {
    /// Creates a new `SourceLoc`. `column` must be greater than 0.
    pub fn new(line: u32, column: u32) -> Self {
        assert!(column > 0);
        Self {
            line,
            column,
            source_id: None,
        }
    }

    /// Sets the source id.
    pub fn with_source_id(mut self, source_id: Option<SourceId>) -> Self {
        self.source_id = source_id;
        self
    }

    /// Returns the source id.
    pub fn source_id(&self) -> Option<SourceId> {
        self.source_id
    }

    /// Returns the line number.
    pub fn line(&self) -> u32 {
        self.line
    }

    /// Returns the mutable reference to the line number.
    pub fn line_mut(&mut self) -> &mut u32 {
        &mut self.line
    }

    /// Returns the column number.
    pub fn column(&self) -> u32 {
        self.column
    }

    /// Returns the mutable reference to the column number.
    pub fn column_mut(&mut self) -> &mut u32 {
        &mut self.column
    }
}

impl fmt::Debug for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}:{}", self.source_id, self.line + 1, self.column)
    }
}

#[test]
fn test() {
    let loc = SourceLoc::new(0, 1);
    assert_eq!(loc, SourceLoc::new(0, 1));
    let loc = SourceLoc::new(0xffffffff, 0xffffffff);
    assert_eq!(loc, SourceLoc::new(0xffffffff, 0xffffffff));
    let loc = SourceLoc::new(10, 17);
    assert_eq!(loc.line(), 10);
    assert_eq!(loc.column(), 17);
}

#[test]
fn sources() {
    use source::*;
    use std::path::PathBuf;
    let mut sources = Sources::new();
    let id = sources.add(Source::File(PathBuf::new()));
    let loc = SourceLoc::new(0, 1).with_source_id(Some(id));
    assert!(loc.source_id() == Some(id));
}

#[test]
#[should_panic]
fn column_must_be_greater_than_0() {
    let _ = SourceLoc::new(0, 0);
}
