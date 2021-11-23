use crate::{name::NameId, ty::Type};

/// A function.
pub struct Function {
    pub name: NameId,
    pub ret: Type,
    pub params: Vec<Param>,
}

/// A parameter of a function.
pub struct Param {
    pub name: NameId,
    pub ty: Type,
}
