use decl::Decl;
use expr::Expr;
use name::Name;
use stmt::Stmt;
use ty::Type;
use typed_arena::Arena as TypedArena;

pub mod decl;
pub mod expr;
pub mod name;
pub mod stmt;
pub mod ty;

/// A translation unit represented in the IR.
pub struct Context<'a> {
    pub decl_arena: TypedArena<Decl<'a>>,
    pub stmt_arena: TypedArena<Stmt<'a>>,
    pub type_arena: TypedArena<Type<'a>>,
    pub name_arena: TypedArena<Name>,
    pub expr_arena: TypedArena<Expr<'a>>,
}

impl<'a> Context<'a> {
    /// Creates a new `Context`.
    pub fn new() -> Self {
        Self {
            decl_arena: TypedArena::new(),
            stmt_arena: TypedArena::new(),
            type_arena: TypedArena::new(),
            name_arena: TypedArena::new(),
            expr_arena: TypedArena::new(),
        }
    }
}

#[test]
fn test() {
    use decl::FuncDef;
    use decl::FuncSignature;
    use stmt::BlockStmt;
    use ty::Sign;
    let module = Context::new();
    let name = module.name_arena.alloc(Name::global(vec!["x", "y"]));
    let ty = module.type_arena.alloc(Type::Int(Sign::Signed));
    let body = BlockStmt(vec![]);
    let f = module.decl_arena.alloc(Decl::FuncDef(FuncDef {
        name,
        sig: FuncSignature {
            ret: ty,
            params: vec![],
        },
        body,
    }));
    let _decl_seq = vec![f];
}
