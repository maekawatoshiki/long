use long_ast::node::{
    decl::{DeclaratorId, FuncDef},
    ty::{FuncType, Sign, Type as TypeNode},
};
use long_ir::{
    func::{Function, FunctionSignature},
    name::NameId,
    ty::Type as IrType,
    Context,
};

/// Converts the given `FuncDef` into a `Function`.
pub fn lower_function(
    ctx: &mut Context,
    FuncDef {
        name,
        ty: FuncType { ret },
    }: FuncDef,
) -> Function {
    let name = resolve_declarator_id(ctx, name);
    let ret = resolve_type(ctx, ret);
    let sig = FunctionSignature {
        ret,
        params: vec![],
    };
    Function { name, sig }
}

fn resolve_declarator_id(ctx: &mut Context, declarator_id: DeclaratorId) -> NameId {
    // TODO
    match declarator_id {
        // TODO: Check if `name` is a fully-qualified name.
        DeclaratorId::Ident(name) => ctx.name_arena.find_or_create_global(&[name]),
    }
}

fn resolve_type(_ctx: &mut Context, ty_node: TypeNode) -> IrType {
    match ty_node {
        TypeNode::Void => IrType::void(),
        TypeNode::Int(Sign::Signed) => IrType::signed_int(),
        TypeNode::Int(Sign::Unsigned) => IrType::unsigned_int(),
        _ => todo!(),
    }
}
