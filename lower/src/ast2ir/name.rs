use super::LowerCtx;
use anyhow::Result;
use long_ast::node::decl::DeclaratorId;
use long_ir::name::Name;

pub fn resolve_declarator_id<'a>(
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
