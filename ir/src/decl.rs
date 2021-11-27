use crate::{name::Name, stmt::BlockStmt, ty::Type};

pub enum Decl<'a> {
    FuncDef(FuncDef<'a>),
}

pub struct FuncDef<'a> {
    pub name: &'a Name,
    pub sig: FuncSignature<'a>,
    pub body: BlockStmt<'a>,
}

/// A function signature (i.e. the return type and parameters of a function).
pub struct FuncSignature<'a> {
    pub ret: &'a Type<'a>,
    pub params: Vec<Param<'a>>,
}

/// A parameter of a function.
pub struct Param<'a> {
    pub name: &'a Name,
    pub ty: &'a Type<'a>,
}
