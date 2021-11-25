use anyhow::Result;
use long_ast::node::stmt::BlockStmt;
use long_ir::{block::Block, Context};

pub(crate) fn lower_block(_ctx: &mut Context, _block: BlockStmt) -> Result<Block> {
    Ok(Block(vec![]))
}
