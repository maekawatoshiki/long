use std::fmt;

use crate::{expr::Expr, name::Name, stmt::BlockStmt, ty::Type};
use id_arena::{Arena, Id};

#[derive(Debug)]
pub enum Decl<'a> {
    FuncDef(FuncDef<'a>),
    SimpleDecl(Vec<SimpleDecl<'a>>),
}

#[derive(Debug)]
pub struct FuncDef<'a> {
    pub name: &'a Name,
    pub sig: FuncSignature<'a>,
    pub body: BlockStmt<'a>,
    pub locals: Locals<'a>,
}

pub struct Locals<'a>(Arena<Local<'a>>);

#[derive(Debug)]
pub struct Local<'a> {
    pub name: &'a Name,
    pub ty: &'a Type<'a>,
}

#[derive(Debug)]
pub struct SimpleDecl<'a> {
    pub name: &'a Name,
    pub ty: &'a Type<'a>,
    pub init: Option<&'a Expr<'a>>,
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

impl<'a> Locals<'a> {
    pub fn new() -> Self {
        Locals(Arena::new())
    }

    pub fn alloc(&mut self, name: &'a Name, ty: &'a Type<'a>) -> Id<Local<'a>> {
        self.0.alloc(Local { name, ty })
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<Local<'a>>, &Local<'a>)> {
        self.0.iter()
    }
}

impl fmt::Debug for Locals<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Locals {{ ")?;
        for (i, local) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", local)?;
        }
        write!(f, " }}")
    }
}
