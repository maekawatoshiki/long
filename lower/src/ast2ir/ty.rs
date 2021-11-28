use super::LowerCtx;
use anyhow::Result;
use long_ast::node::ty::{Sign as AstSign, Type as AstType};
use long_ir::ty::{Sign, Type as IrType};

pub fn resolve_type<'a>(ctx: &mut LowerCtx<'a>, ty_node: &AstType) -> Result<&'a IrType<'a>> {
    match ty_node {
        AstType::Void => Ok(ctx.ir_ctx.type_arena.alloc(IrType::Void)),
        AstType::Int(AstSign::Signed) => Ok(ctx.ir_ctx.type_arena.alloc(IrType::Int(Sign::Signed))),
        AstType::Int(AstSign::Unsigned) => {
            Ok(ctx.ir_ctx.type_arena.alloc(IrType::Int(Sign::Unsigned)))
        }
        _ => todo!(),
    }
}
