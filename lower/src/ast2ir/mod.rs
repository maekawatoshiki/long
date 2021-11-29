mod decl;
mod env;
mod expr;
mod name;
mod stmt;
mod ty;

use crate::ast2ir::{
    decl::lower_simple_decl, name::resolve_declarator_id, stmt::lower_block_stmt, ty::resolve_type,
};
use anyhow::Result;
use env::Envs;
use long_ast::node::decl::{Decl as AstDecl, FuncDef as AstFuncDef};
use long_ir::{
    decl::{Decl as IrDecl, FuncDef as IrFuncDef, FuncSignature, Locals},
    Context,
};
use std::mem::replace;

pub struct LowerCtx<'a> {
    pub ir_ctx: &'a Context<'a>,
    pub envs: Envs<'a>,
    pub locals: Locals<'a>,
}

impl<'a> LowerCtx<'a> {
    pub fn new(ir_ctx: &'a Context<'a>) -> Self {
        Self {
            ir_ctx,
            envs: Envs::new(),
            locals: Locals::new(),
        }
    }
}

pub fn lower_decl<'a>(ctx: &mut LowerCtx<'a>, decl: &AstDecl) -> Result<&'a IrDecl<'a>> {
    match decl {
        AstDecl::FuncDef(funcdef) => {
            let ir_funcdef = lower_funcdef(ctx, funcdef)?;
            Ok(ir_funcdef)
        }
        AstDecl::SimpleDecl(decls) => {
            let mut new_decls = vec![];
            for decl in decls {
                new_decls.push(lower_simple_decl(ctx, decl)?);
            }
            Ok(ctx.ir_ctx.decl_arena.alloc(IrDecl::SimpleDecl(new_decls)))
        }
    }
}

fn lower_funcdef<'a>(ctx: &mut LowerCtx<'a>, funcdef: &AstFuncDef) -> Result<&'a IrDecl<'a>> {
    let name = resolve_declarator_id(ctx, &funcdef.name, false)?;
    let sig = FuncSignature {
        ret: resolve_type(ctx, &funcdef.ty.ret)?,
        params: vec![],
    };
    ctx.envs.push_block();
    let body = lower_block_stmt(ctx, &funcdef.body)?;
    ctx.envs.pop();
    Ok(ctx.ir_ctx.decl_arena.alloc(IrDecl::FuncDef(IrFuncDef {
        name,
        sig,
        body,
        locals: replace(&mut ctx.locals, Locals::new()),
    })))
}

macro_rules! parse_and_lower_test {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            use long_ast::node::Located;
            use long_parser::lexer::Lexer;
            use long_parser::Parser;
            let Located { inner, .. } = Parser::new(&mut Lexer::new($src))
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
    };
}

parse_and_lower_test!(parse_and_lower, "int main() { return 0; }");
parse_and_lower_test!(parse_and_lower_simple_decl, "int i;");
parse_and_lower_test!(parse_and_lower_simple_decl2, "int i, j;");
parse_and_lower_test!(parse_and_lower_func, "int main() { int i; return 0; }");
// parse_and_lower_test!(
//     parse_and_lower_func2,
//     "int main() { int i; i = 10; return 0; }"
// );
