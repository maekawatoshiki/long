use crate::{block::Block, name::NameId, ty::Type};

/// A function.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: NameId,
    pub sig: FunctionSignature,
    pub body: Block,
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
