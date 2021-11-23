use std::{
    fmt,
    sync::atomic::{self, AtomicU32},
};

/// A type.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Type(u32, u32);

/// User-defined types.
#[derive(Debug, Clone)]
pub enum CompoundType {
    Dummy,
    // TODO
}

/// An arena of compound types.
pub struct CompoundTypes {
    id: u32,
    arena: Vec<CompoundType>,
}

impl CompoundTypes {
    /// Create a new arena of types.
    pub fn new() -> Self {
        static ID_COUNTER: AtomicU32 = AtomicU32::new(1);
        Self {
            id: ID_COUNTER.fetch_add(1, atomic::Ordering::SeqCst),
            arena: Vec::new(),
        }
    }

    /// Creates a new `Type` for the given `ty`.
    pub fn add(&mut self, ty: CompoundType) -> Type {
        let id = Type(self.id, self.arena.len() as u32);
        self.arena.push(ty);
        id
    }

    /// Returns the `CompoundType` for the given `id`.
    pub fn get(&self, id: Type) -> Option<&CompoundType> {
        if id.0 != self.id {
            return None;
        }
        self.arena.get(id.1 as usize)
    }
}

impl Type {
    #[inline]
    pub fn void() -> Self {
        primitive::VOID
    }

    #[inline]
    pub fn signed_int() -> Self {
        primitive::SIGNED_INT
    }

    #[inline]
    pub fn unsigned_int() -> Self {
        primitive::UNSIGNED_INT
    }

    pub fn is_void(&self) -> bool {
        self == &primitive::VOID
    }

    pub fn is_signed_int(&self) -> bool {
        self == &primitive::SIGNED_INT
    }

    pub fn is_unsigned_int(&self) -> bool {
        self == &primitive::UNSIGNED_INT
    }
}

pub mod primitive {
    use super::Type;

    // TODO: Add more types.
    pub const VOID: Type = Type(0, 0);
    pub const SIGNED_INT: Type = Type(0, 1);
    pub const UNSIGNED_INT: Type = Type(0, 2);
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &primitive::VOID => write!(f, "Type(void)"),
            &primitive::SIGNED_INT => write!(f, "Type(signed int)"),
            &primitive::UNSIGNED_INT => write!(f, "Type(unsigned int)"),
            &Type(0, _) => todo!(),
            _ => write!(f, "Type({}, {})", self.0, self.1),
        }
    }
}

#[test]
fn primitive_types() {
    use primitive::*;
    assert!(VOID.is_void());
    assert!(SIGNED_INT.is_signed_int());
    assert!(UNSIGNED_INT.is_unsigned_int());
}

#[test]
fn compound_types() {
    let mut arena = CompoundTypes::new();
    let ty = arena.add(CompoundType::Dummy);
    assert!(matches!(arena.get(ty), Some(&CompoundType::Dummy)));
}
