use crate::token::Token;
use std::collections::HashMap;

/// Defined macros.
#[derive(Debug, Clone)]
pub struct Macros(HashMap<String, Macro>);

/// A macro body.
#[derive(Debug, Clone)]
pub enum Macro {
    /// Object-like macro.
    Obj(Vec<Token>),
}

impl Macros {
    /// Creates a new `Macros`.
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Adds a new object-like macro.
    pub fn add_obj_macro(&mut self, name: impl Into<String>, body: Vec<Token>) {
        self.0.insert(name.into(), Macro::Obj(body));
    }

    pub fn find(&self, name: impl AsRef<str>) -> Option<&Macro> {
        self.0.get(name.as_ref())
    }
}
