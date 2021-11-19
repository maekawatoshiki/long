/// A statement kind.
#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Decl),
}

/// A declaration statement kind.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
#[derive(Debug, Clone)]
pub enum Decl {
    FuncDef(FuncDef),
}

/// A function definition.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.fct.def.general#nt:function-definition>
#[derive(Debug, Clone)]
pub struct FuncDef {}
