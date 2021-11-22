use super::ty::FuncType;

/// A declaration kind.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
#[derive(Debug, Clone)]
pub enum Decl {
    FuncDef(FuncDef),
}

/// A function definition.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.fct.def.general#nt:function-definition>
#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: DeclaratorId,
    pub ty: FuncType,
}

/// A declarator id.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.decl#nt:declarator-id>
#[derive(Debug, Clone)]
pub enum DeclaratorId {
    Ident(String),
}

impl From<FuncDef> for Decl {
    fn from(func_def: FuncDef) -> Self {
        Decl::FuncDef(func_def)
    }
}
