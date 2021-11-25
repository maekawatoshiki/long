use crate::inst::Inst;

/// The kind of a block. A block can be if-block, for-block, etc.
#[derive(Debug, Clone)]
pub enum BlockKind {
    Block(Block),
    // if, while, etc.
}

/// A simple block.
#[derive(Debug, Clone)]
pub struct Block(pub Vec<InstOrBlock>);

#[derive(Debug, Clone)]
pub enum InstOrBlock {
    Inst(Inst),
    Block(Block),
}
