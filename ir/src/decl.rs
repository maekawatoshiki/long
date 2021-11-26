use crate::{name::Name, ty::Type};

pub enum Decl<'a> {
    FuncDef(FuncDef<'a>),
}

pub struct FuncDef<'a> {
    pub name: &'a Name,
    pub ty: &'a Type<'a>,
    pub body: (),
}
