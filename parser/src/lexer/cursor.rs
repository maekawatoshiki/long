use sourceloc::{source::SourceId, SourceLoc};

/// A cursor for a string.
pub(crate) struct Cursor {
    /// The source code the lexer reads.
    source: String,

    /// The current position in `source`.
    pub(crate) pos: usize,

    /// The current source location.
    pub(crate) loc: SourceLoc,
}

impl Cursor {
    /// Creates a new `Cursor`.
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            pos: 0,
            loc: SourceLoc::new(0, 1),
        }
    }

    /// Sets the source id.
    pub fn with_source_id(mut self, source_id: SourceId) -> Self {
        self.loc = self.loc.with_source_id(Some(source_id));
        self
    }

    /// Peeks the character at `self.pos`.
    pub fn peek_char(&self) -> Option<char> {
        self.source.get(self.pos..)?.chars().next()
    }

    /// Peeks the character at `self.pos + 1`.
    pub fn peek_char2(&self) -> Option<char> {
        let mut chars = self.source.get(self.pos..)?.chars();
        chars.next();
        chars.next()
    }

    /// Peeks the character at `self.pos + 2`.
    pub fn peek_char3(&self) -> Option<char> {
        let mut chars = self.source.get(self.pos..)?.chars();
        chars.next();
        chars.next();
        chars.next()
    }

    /// Peeks the character at `self.pos` and advances `self.pos`.
    pub fn next_char(&mut self) -> Option<char> {
        let c = self.source[self.pos..].chars().next();
        if c == Some('\n') {
            *self.loc.line_mut() += 1;
            *self.loc.column_mut() = 1;
        } else {
            *self.loc.column_mut() += 1;
        }
        self.pos += 1;
        c
    }

    /// Takes a sequence of characters while `preicate()` returns true.
    pub fn take_chars_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(&char) -> bool,
    {
        let mut buf = String::new();
        loop {
            match self.peek_char() {
                Some(c) if predicate(&c) => {
                    buf.push(self.next_char().unwrap());
                }
                _ => break,
            }
        }
        buf
    }
}
