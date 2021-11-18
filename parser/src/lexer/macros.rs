use ast::token::Token;
use std::collections::HashMap;

/// Defined macros.
#[derive(Debug, Clone)]
pub struct Macros(HashMap<String, Macro>);

/// A macro body.
#[derive(Debug, Clone)]
pub enum Macro {
    /// Object-like macro.
    Obj(Vec<Token>),

    /// Function-like macro.
    Func(Vec<FuncMacroToken>),
}

/// A function-like macro token.
/// `Param(leading space, n)` represents a `n`th parameter.
/// `Vararg(leading space, n)` represents the `n`th parameter is `...`.
// TODO: Param and Vararg should not have leading space as bool.
#[derive(Debug, Clone)]
pub enum FuncMacroToken {
    Token(Token),
    Param(bool, usize),
    Vararg(bool, usize),
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

    /// Adds a new function-like macro.
    pub fn add_func_macro(&mut self, name: impl Into<String>, body: Vec<FuncMacroToken>) {
        self.0.insert(name.into(), Macro::Func(body));
    }

    /// If `name` is a defined macro name, returns the corresponding macro body.
    pub fn find(&self, name: impl AsRef<str>) -> Option<&Macro> {
        self.0.get(name.as_ref())
    }

    pub fn remove(&mut self, name: impl AsRef<str>) {
        self.0.remove(name.as_ref());
    }
}
