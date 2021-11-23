use func::Function;
use id_arena::Arena;
use name::NameArena;

pub mod func;
pub mod name;
pub mod ty;

/// A translation unit represented in the IR.
#[derive(Debug, Clone)]
pub struct Module {
    pub name_arena: NameArena,
    pub func_arena: Arena<Function>,
}
