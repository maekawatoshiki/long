mod expr;
mod stmt;

use anyhow::Result;
use long_ast::node::{
    decl::{Decl as AstDecl, DeclaratorId, FuncDef as AstFuncDef},
    ty::{Sign as AstSign, Type as AstType},
};
use long_ir::{
    decl::{Decl as IrDecl, FuncDef as IrFuncDef, FuncSignature},
    name::Name,
    ty::{Sign, Type as IrType},
    Context,
};

use crate::ast2ir::stmt::lower_block_stmt;

pub struct LowerCtx<'a> {
    pub ir_ctx: &'a Context<'a>,
}

impl<'a> LowerCtx<'a> {
    pub fn new(ir_ctx: &'a Context<'a>) -> Self {
        Self { ir_ctx }
    }
}

pub fn lower_decl<'a>(ctx: &mut LowerCtx<'a>, decl: &AstDecl) -> Result<&'a IrDecl<'a>> {
    match decl {
        AstDecl::FuncDef(funcdef) => {
            let ir_funcdef = lower_funcdef(ctx, funcdef)?;
            Ok(ir_funcdef)
        }
    }
}

fn lower_funcdef<'a>(ctx: &mut LowerCtx<'a>, funcdef: &AstFuncDef) -> Result<&'a IrDecl<'a>> {
    let name = resolve_declarator_id(ctx, &funcdef.name)?;
    let sig = FuncSignature {
        ret: resolve_type(ctx, &funcdef.ty.ret)?,
        params: vec![],
    };
    let body = lower_block_stmt(ctx, &funcdef.body)?;
    Ok(ctx
        .ir_ctx
        .decl_arena
        .alloc(IrDecl::FuncDef(IrFuncDef { name, sig, body })))
}

fn resolve_declarator_id<'a>(
    ctx: &mut LowerCtx<'a>,
    declarator_id: &DeclaratorId,
) -> Result<&'a Name> {
    // TODO
    match declarator_id {
        // TODO: Check if `name` is a fully-qualified name.
        DeclaratorId::Ident(name) => Ok(ctx
            .ir_ctx
            .name_arena
            .alloc(Name::global(vec![name.to_owned()]))),
    }
}

fn resolve_type<'a>(ctx: &mut LowerCtx<'a>, ty_node: &AstType) -> Result<&'a IrType<'a>> {
    match ty_node {
        AstType::Void => Ok(ctx.ir_ctx.type_arena.alloc(IrType::Void)),
        AstType::Int(AstSign::Signed) => Ok(ctx.ir_ctx.type_arena.alloc(IrType::Int(Sign::Signed))),
        AstType::Int(AstSign::Unsigned) => {
            Ok(ctx.ir_ctx.type_arena.alloc(IrType::Int(Sign::Unsigned)))
        }
        _ => todo!(),
    }
}

#[test]
fn parse_and_lower() {
    use long_ast::node::Located;
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int main() { return 0; }"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let ir_ctx = Context::new();
    let mut ctx = LowerCtx::new(&ir_ctx);
    let decl = lower_decl(&mut ctx, &inner);
    insta::assert_debug_snapshot!(decl)
}
