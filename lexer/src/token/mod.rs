use long_sourceloc::SourceLoc;

#[derive(Debug, Clone)]
pub struct Token {
    loc: SourceLoc,
}

impl Token {
    pub fn new(loc: SourceLoc) -> Self {
        Self { loc }
    }

    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }
}
