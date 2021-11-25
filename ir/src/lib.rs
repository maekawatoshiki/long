use std::fmt;

use func::Function;
use id_arena::Arena;
use name::NameArena;
use value::ValueArena;

pub mod block;
pub mod func;
pub mod inst;
pub mod name;
pub mod ty;
pub mod value;

/// A translation unit represented in the IR.
#[derive(Clone)]
pub struct Module {
    pub name_arena: NameArena,
    pub val_arena: ValueArena,
    pub func_arena: Arena<Function>,
}

impl Module {
    /// Creates a new `Module`.
    pub fn new() -> Self {
        Self {
            name_arena: NameArena::new(),
            val_arena: ValueArena::new(),
            func_arena: Arena::new(),
        }
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Module {{\n")?;
        write!(f, "  name_arena: {:?}\n", self.name_arena)?;
        write!(f, "  val_arena: {:?}\n", self.val_arena)?;
        for (_, func) in &self.func_arena {
            func.debug(self, f, 2)?;
        }
        write!(f, "}}")
    }
}
