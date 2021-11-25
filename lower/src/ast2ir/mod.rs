mod stmt;

use anyhow::Result;
use long_ast::node::{
    decl::{DeclaratorId, FuncDef},
    ty::{FuncType, Sign, Type as TypeNode},
};
use long_ir::{
    func::{Function, FunctionSignature},
    name::NameId,
    ty::Type as IrType,
    Module,
};
use stmt::lower_block;

/// Converts the given `FuncDef` into a `Function`.
pub fn lower_function(
    ctx: &mut Module,
    FuncDef {
        name,
        ty: FuncType { ret },
        body,
    }: FuncDef,
) -> Result<Function> {
    let name = resolve_declarator_id(ctx, name);
    let ret = resolve_type(ctx, ret);
    let sig = FunctionSignature {
        ret,
        params: vec![],
    };
    let body = lower_block(ctx, body)?;
    Ok(Function { name, sig, body })
}

fn resolve_declarator_id(ctx: &mut Module, declarator_id: DeclaratorId) -> NameId {
    // TODO
    match declarator_id {
        // TODO: Check if `name` is a fully-qualified name.
        DeclaratorId::Ident(name) => ctx.name_arena.find_or_create_global(&[name]),
    }
}

fn resolve_type(_ctx: &mut Module, ty_node: TypeNode) -> IrType {
    match ty_node {
        TypeNode::Void => IrType::void(),
        TypeNode::Int(Sign::Signed) => IrType::signed_int(),
        TypeNode::Int(Sign::Unsigned) => IrType::unsigned_int(),
        _ => todo!(),
    }
}

#[test]
fn parse_and_lower() {
    use long_ast::node::{decl::Decl, Located};
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int main() { return 0; }"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let mut ctx = Module::new();
    let func_ir = lower_function(
        &mut ctx,
        match inner {
            Decl::FuncDef(funcdef) => funcdef,
        },
    );
    insta::assert_debug_snapshot!(func_ir)
}
