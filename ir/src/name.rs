use std::fmt;

/// A name used in the IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    /// A fully-qualified name for global variables, functions, namespaces, etc.
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

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Global(name) => write!(f, "::{}", name.join("::")),
            Name::Local(name) => write!(f, "{}", name),
        }
    }
}
