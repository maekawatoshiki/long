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

pub struct Variables<T: fmt::Debug>(Arena<T>);

pub type Locals<'a> = Variables<Local<'a>>;

pub type Globals<'a> = Variables<Global<'a>>;

#[derive(Debug)]
pub struct Local<'a> {
    pub name: String,
    pub ty: &'a Type<'a>,
}

#[derive(Debug)]
pub struct Global<'a> {
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

impl<T: fmt::Debug> Variables<T> {
    pub fn new() -> Self {
        Self(Arena::new())
    }

    pub fn alloc(&mut self, t: T) -> Id<T> {
        self.0.alloc(t)
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.0.get(id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> {
        self.0.iter()
    }
}

impl<T: fmt::Debug> fmt::Debug for Variables<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Variables {{ ")?;
        for (i, v) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", v)?;
        }
        write!(f, " }}")
    }
}
