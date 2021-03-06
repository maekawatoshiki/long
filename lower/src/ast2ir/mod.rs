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
use long_ast::node::decl::{Decl as AstDecl, DeclaratorId, FuncDef as AstFuncDef};
use long_ir::{
    decl::{Decl as IrDecl, FuncDef as IrFuncDef, FuncSignature, Globals, Locals},
    Context,
};
use std::mem::replace;

use self::ty::resolve_func_type;

pub struct LowerCtx<'a> {
    pub ir_ctx: &'a Context<'a>,
    pub envs: Envs<'a>,
    pub locals: Locals<'a>,
    pub globals: Globals<'a>,
}

impl<'a> LowerCtx<'a> {
    pub fn new(ir_ctx: &'a Context<'a>) -> Self {
        Self {
            ir_ctx,
            envs: Envs::new(),
            locals: Locals::new(),
            globals: Globals::new(),
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
    let fty = resolve_func_type(ctx, &funcdef.ty)?;
    ctx.envs.add_to_cur_env(
        match &funcdef.name {
            DeclaratorId::Ident(name) => name,
        },
        fty,
    );
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
            let list = Parser::new(&mut Lexer::new($src)).parse_program().unwrap();
            let ir_ctx = Context::new();
            let mut ctx = LowerCtx::new(&ir_ctx);
            let mut decls = vec![];
            for Located { inner, .. } in list {
                decls.push(lower_decl(&mut ctx, &inner));
            }
            insta::assert_debug_snapshot!(decls)
        }
    };
}

parse_and_lower_test!(parse_and_lower, "int main() { return 0; }");
parse_and_lower_test!(parse_and_lower_simple_decl, "int i;");
parse_and_lower_test!(parse_and_lower_simple_decl2, "int i, j;");
parse_and_lower_test!(parse_and_lower_func, "int main() { int i; return 0; }");
parse_and_lower_test!(
    parse_and_lower_func2,
    "int main() { int i; i = 10; return 0; }"
);
parse_and_lower_test!(
    parse_and_lower_funcs,
    r#"
int f() { return 1; }
int main() {
    return f();
}"#
);
