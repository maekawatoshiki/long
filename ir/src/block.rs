use crate::{inst::InstId, Module};
use id_arena::Id;
use std::fmt;

pub type BlockId = Id<Block>;

/// The kind of a block. A block can be if-block, for-block, etc.
#[derive(Debug, Clone)]
pub enum Block {
    Block(SimpleBlock),
    // if, while, etc.
}

/// A simple block.
#[derive(Debug, Clone)]
pub struct SimpleBlock(pub Vec<InstOrBlock>);

#[derive(Debug, Clone)]
pub enum InstOrBlock {
    Inst(InstId),
    Block(BlockId),
}

impl Block {
    pub fn debug(&self, module: &Module, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        match self {
            Block::Block(block) => block.debug(module, f, indent),
        }
    }
}

impl SimpleBlock {
    pub fn debug(&self, module: &Module, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        writeln!(f, "{:indent$}Block:", "", indent = indent)?;
        for elem in &self.0 {
            match elem {
                InstOrBlock::Inst(id) => {
                    write!(f, "{:indent$}", "", indent = indent + 2)?;
                    module.inst_arena[*id].debug(module, f)?;
                }
                InstOrBlock::Block(id) => {
                    module.block_arena[*id].debug(module, f, indent + 2)?;
                }
            }
        }
        Ok(())
    }
}
