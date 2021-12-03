pub mod decl;
mod expr;
mod name;
mod stmt;
mod ty;

use cranelift::prelude::Configurable;
use cranelift_codegen::{
    isa::{self},
    settings, Context as ClifContext,
};
use cranelift_module::{DataContext, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub struct LowerCtx {
    pub clif_ctx: ClifContext,
    pub data_ctx: DataContext,
    pub module: ObjectModule,
}

impl LowerCtx {
    /// Creates a new `LowerCtx`.
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        let isa_builder = isa::lookup_by_name("x86_64-unknown-unknown-elf").unwrap();
        #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
        let isa_builder = isa::lookup_by_name("aarch64-apple-darwin").unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder));

        let builder = ObjectBuilder::new(
            isa,
            "".to_owned(), // TODO: This will be embedded in the object file.
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let module = ObjectModule::new(builder);

        Self {
            clif_ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
#[test]
fn parse_and_lower_to_clif() {
    use crate::ast2ir;
    use long_ast::node::Located;
    use long_ir::decl::Decl;
    use long_ir::Context as IrContext;
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int main() { return 42; }"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let ctx = IrContext::new();
    let mut ctx = ast2ir::LowerCtx::new(&ctx);
    let decl = ast2ir::lower_decl(&mut ctx, &inner).unwrap();
    let mut ctx = LowerCtx::new();
    let _clif_func = decl::lower_funcdef(
        &mut ctx,
        match decl {
            Decl::FuncDef(funcdef) => funcdef,
            _ => todo!(),
        },
    );
    let product = ctx.module.finish();
    let obj = product.emit().unwrap();
    insta::assert_debug_snapshot!(obj);
}

#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
#[test]
fn parse_and_lower_to_clif2() {
    use crate::ast2ir;
    use long_ast::node::Located;
    use long_ir::Context as IrContext;
    use long_parser::lexer::Lexer;
    use long_parser::Parser;
    let Located { inner, .. } = Parser::new(&mut Lexer::new("int i;"))
        .parse_program()
        .unwrap()
        .into_iter()
        .next()
        .unwrap();
    let ctx = IrContext::new();
    let mut ctx = ast2ir::LowerCtx::new(&ctx);
    let decl = ast2ir::lower_decl(&mut ctx, &inner).unwrap();
    let mut ctx = LowerCtx::new();
    let _clif_func = decl::lower_decl(&mut ctx, decl);
    let product = ctx.module.finish();
    let obj = product.emit().unwrap();
    insta::assert_debug_snapshot!(obj);
}
