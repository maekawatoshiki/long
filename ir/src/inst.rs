use id_arena::Id;
use std::fmt;

use crate::{value::ValueId, Module};

pub type InstId = Id<Inst>;

/// An instruction in the program.
#[derive(Debug, Clone)]
pub enum Inst {
    Return(Option<ValueId>),
}

impl Inst {
    pub fn debug(&self, module: &Module, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::Return(Some(id)) => {
                let val = module.val_arena.get(*id).unwrap();
                writeln!(f, "return {:?}", val)
            }
            Inst::Return(None) => writeln!(f, "return"),
        }
    }
}
