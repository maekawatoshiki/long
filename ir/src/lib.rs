use func::Function;
use id_arena::Arena;
use name::NameArena;

pub mod func;
pub mod name;
pub mod ty;

/// A translation unit represented in the IR.
#[derive(Debug, Clone)]
pub struct Context {
    pub name_arena: NameArena,
    pub func_arena: Arena<Function>,
}

impl Context {
    /// Creates a new `Context`.
    pub fn new() -> Self {
        Context {
            name_arena: NameArena::new(),
            func_arena: Arena::new(),
        }
    }
}
