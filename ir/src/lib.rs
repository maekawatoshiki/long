use decl::Decl;
use name::Name;
use ty::Type;
// use std::fmt;
//
// use block::Block;
// use func::Function;
// use id_arena::Arena;
// use inst::Inst;
// use name::NameArena;
use typed_arena::Arena as TypedArena;
// use value::ValueArena;

pub mod decl;
// pub mod func;
// pub mod inst;
pub mod name;
pub mod ty;
// pub mod value;

/// A translation unit represented in the IR.
pub struct Module<'a> {
    pub decl_arena: TypedArena<Decl<'a>>,
    pub type_arena: TypedArena<Type<'a>>,
    pub name_arena: TypedArena<Name>,
}

impl<'a> Module<'a> {
    /// Creates a new `Module`.
    pub fn new() -> Self {
        Self {
            decl_arena: TypedArena::new(),
            type_arena: TypedArena::new(),
            name_arena: TypedArena::new(),
        }
    }
}
