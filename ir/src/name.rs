use std::fmt;

/// A name used in the IR.
#[derive(Debug, Clone, PartialEq)]
pub enum Name {
    /// A fully-qualified name for global variables, functions, namespaces, etc.
    Global(Vec<String>),

    /// A name for local variables.
    Local(String),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Name::Global(name) => write!(f, "::{}", name.join("::")),
            Name::Local(name) => write!(f, "{}", name),
        }
    }
}
