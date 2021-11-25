use std::fmt;

use crate::{value::ValueId, Module};

/// An instruction in the program.
#[derive(Debug, Clone)]
pub enum Inst {
    Return(Option<ValueId>),
}

impl Inst {
    pub fn debug(&self, module: &Module, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        write!(f, "{:indent$}", "", indent = indent)?;
        match self {
            Inst::Return(Some(id)) => {
                let val = module.val_arena.get(*id).unwrap();
                writeln!(f, "return {:?}", val)
            }
            Inst::Return(None) => writeln!(f, "return"),
        }
    }
}
