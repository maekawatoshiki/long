use super::{
    expr::Expr,
    stmt::BlockStmt,
    ty::{FuncType, Type},
};

/// A declaration kind.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
#[derive(Debug, Clone)]
pub enum Decl {
    FuncDef(FuncDef),
    SimpleDecl(SimpleDecl),
}

/// A function definition.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.fct.def.general#nt:function-definition>
#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: DeclaratorId,
    pub ty: FuncType,
    pub body: BlockStmt,
}

/// A simple declaration.
/// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#nt:simple-declaration>
#[derive(Debug, Clone)]
pub struct SimpleDecl {
    pub names: Vec<DeclaratorId>,
    pub types: Vec<Type>,
    pub inits: Vec<Option<Expr>>,
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
