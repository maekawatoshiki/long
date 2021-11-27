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
pub mod expr;
pub mod name;
pub mod stmt;
pub mod ty;
// pub mod value;

/// A translation unit represented in the IR.
pub struct Context<'a> {
    pub decl_arena: TypedArena<Decl<'a>>,
    pub type_arena: TypedArena<Type<'a>>,
    pub name_arena: TypedArena<Name>,
}

impl<'a> Context<'a> {
    /// Creates a new `Context`.
    pub fn new() -> Self {
        Self {
            decl_arena: TypedArena::new(),
            type_arena: TypedArena::new(),
            name_arena: TypedArena::new(),
        }
    }
}

#[test]
fn test() {
    use decl::FuncDef;
    use stmt::BlockStmt;
    use ty::Sign;
    let module = Context::new();
    let name = module.name_arena.alloc(Name::global(vec!["x", "y"]));
    let ty = module.type_arena.alloc(Type::Int(Sign::Signed));
    let body = BlockStmt(vec![]);
    let f = module
        .decl_arena
        .alloc(Decl::FuncDef(FuncDef { name, ty, body }));
    let _decl_seq = vec![f];
}
