use crate::{inst::Inst, Module};
use std::fmt;

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
    Inst(Inst),
    Block(Block),
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
                InstOrBlock::Inst(inst) => {
                    inst.debug(module, f, indent + 2)?;
                }
                InstOrBlock::Block(block) => {
                    block.debug(module, f, indent + 2)?;
                }
            }
        }
        Ok(())
    }
}
