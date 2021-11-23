use id_arena::{Arena, Id};

pub type NameArena = Arena<Name>;
pub type NameId = Id<Name>;

/// A name used in the IR.
#[derive(Debug, Clone)]
pub enum Name {
    /// An absolute name for global variables, functions, namespaces, etc.
    Global(Vec<String>),

    /// A name for local variables.
    Local(String),
}

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
