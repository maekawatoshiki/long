use crate::{name::Name, stmt::BlockStmt, ty::Type};

#[derive(Debug)]
pub enum Decl<'a> {
    FuncDef(FuncDef<'a>),
}

#[derive(Debug)]
pub struct FuncDef<'a> {
    pub name: &'a Name,
    pub sig: FuncSignature<'a>,
    pub body: BlockStmt<'a>,
}

/// A function signature (i.e. the return type and parameters of a function).
#[derive(Debug)]
pub struct FuncSignature<'a> {
    pub ret: &'a Type<'a>,
    pub params: Vec<Param<'a>>,
}

/// A parameter of a function.
#[derive(Debug)]
pub struct Param<'a> {
    pub name: &'a Name,
    pub ty: &'a Type<'a>,
}
