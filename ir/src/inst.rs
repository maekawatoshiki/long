use id_arena::Id;
use std::fmt;

use crate::Module;

pub type InstId = Id<Inst>;

/// An instruction in the program.
#[derive(Debug, Clone)]
pub enum Inst {
    Int(i32),
    Add(InstId, InstId),
    Return(Option<InstId>),
}

impl Inst {
    pub fn debug(&self, _module: &Module, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::Int(i) => writeln!(f, "int {}", i),
            Inst::Add(lhs, rhs) => {
                writeln!(f, "add {:?}, {:?}", lhs, rhs)
            }
            Inst::Return(Some(id)) => {
                writeln!(f, "return {:?}", id)
            }
            Inst::Return(None) => writeln!(f, "return"),
        }
    }
}
