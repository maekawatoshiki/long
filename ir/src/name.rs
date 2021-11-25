use std::fmt;

use id_arena::{Arena, Id};

pub type NameId = Id<Name>;

/// A name used in the IR.
#[derive(Debug, Clone, PartialEq)]
pub enum Name {
    /// A fully-qualified name for global variables, functions, namespaces, etc.
    Global(Vec<String>),

    /// A name for local variables.
    Local(String),
}

/// An arena for `Name`.
#[derive(Debug, Clone)]
pub struct NameArena(Arena<Name>);

impl Name {
    /// Creates a new global name.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::name::Name;
    /// Name::global(vec!["std", "cout"]);
    /// ```
    pub fn global<T>(name: Vec<T>) -> Self
    where
        T: Into<String>,
    {
        Name::Global(name.into_iter().map(Into::into).collect())
    }

    /// Creates a new local name.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::name::Name;
    /// Name::local("x");
    /// ```
    pub fn local<T>(name: T) -> Self
    where
        T: Into<String>,
    {
        Name::Local(name.into())
    }
}

impl NameArena {
    /// Creates a new `NameArena`.
    pub fn new() -> Self {
        NameArena(Arena::new())
    }

    /// Returns the `NameId` for `name` if exists.
    /// Otherwise, creates a new `NameId` for `name`.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::name::{Name, NameArena};
    /// let mut arena = NameArena::new();
    /// let id = arena.find_or_create_global(&["std", "cout"]);
    /// let id2 = arena.find_or_create_global(&["std", "cout"]);
    /// assert_eq!(id, id2);
    /// let id3 = arena.find_or_create_global(&["std", "cin"]);
    /// assert_ne!(id, id3);
    /// ```
    pub fn find_or_create_global<T>(&mut self, name: &[T]) -> NameId
    where
        T: AsRef<str> + Into<String> + Clone,
        String: PartialEq<T>,
    {
        if let Some(id) = self.0.iter().find_map(|(id, n)| match n {
            Name::Global(n) if n == name => Some(id),
            _ => None,
        }) {
            return id;
        }
        self.0.alloc(Name::global(name.to_vec()))
    }

    /// Returns the `NameId` for `name` if exists.
    /// Otherwise, creates a new `NameId` for `name`.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::name::{Name, NameArena};
    /// let mut arena = NameArena::new();
    /// let id = arena.find_or_create_local("x");
    /// let id2 = arena.find_or_create_local("x");
    /// assert_eq!(id, id2);
    /// let id3 = arena.find_or_create_local("y");
    /// assert_ne!(id, id3);
    /// ```
    pub fn find_or_create_local(&mut self, name: &str) -> NameId {
        if let Some(id) = self.0.iter().find_map(|(id, n)| match n {
            Name::Local(n) if n == name => Some(id),
            _ => None,
        }) {
            return id;
        }
        self.0.alloc(Name::Local(name.into()))
    }

    /// Returns the `Name` for `id`.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::name::{Name, NameArena};
    /// let mut arena = NameArena::new();
    /// let id = arena.find_or_create_local("x");
    /// let name = arena.get(id);
    /// assert_eq!(name, Some(&Name::local("x")));
    /// ```
    pub fn get(&self, id: NameId) -> Option<&Name> {
        self.0.get(id)
    }

    /// Returns the mutable reference to `Name` for `id`.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::name::{Name, NameArena};
    /// let mut arena = NameArena::new();
    /// let id = arena.find_or_create_local("x");
    /// let name = arena.get_mut(id);
    /// assert_eq!(name, Some(&mut Name::local("x")));
    /// ```
    pub fn get_mut(&mut self, id: NameId) -> Option<&mut Name> {
        self.0.get_mut(id)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Global(name) => write!(f, "::{}", name.join("::")),
            Name::Local(name) => write!(f, "{}", name),
        }
    }
}
