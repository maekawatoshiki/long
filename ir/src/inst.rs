use crate::value::ValueId;

/// An instruction in the program.
#[derive(Debug, Clone)]
pub enum Inst {
    Return(Option<ValueId>),
}
