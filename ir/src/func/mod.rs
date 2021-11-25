use id_arena::Id;

use crate::{block::SimpleBlock, name::NameId, ty::Type, Module};
use std::fmt;

pub type FunctionId = Id<Function>;

/// A function.
#[derive(Clone)]
pub struct Function {
    pub name: NameId,
    pub sig: FunctionSignature,
    pub body: SimpleBlock,
}

/// A function signature (i.e. the return type and parameters of a function).
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub ret: Type,
    pub params: Vec<Type>,
}

/// A parameter of a function.
#[derive(Debug, Clone)]
pub struct Param {
    pub name: NameId,
    pub ty: Type,
}

impl Function {
    pub fn debug(&self, module: &Module, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        writeln!(
            f,
            "{:indent$}Function ({}) {{",
            "",
            module.name_arena.get(self.name).unwrap(),
            indent = indent
        )?;
        writeln!(f, "{:indent$}sig: {:?}", "", self.sig, indent = indent + 2)?;
        writeln!(f, "{:indent$}body:", "", indent = indent + 2)?;
        self.body.debug(module, f, indent + 4)?;
        writeln!(f, "{:indent$}}}", "", indent = indent)?;
        Ok(())
    }
}
