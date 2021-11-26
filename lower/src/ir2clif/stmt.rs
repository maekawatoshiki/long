use cranelift::{frontend::FunctionBuilder, prelude::InstBuilder};
use cranelift_codegen::ir::types as clif_ty;
use long_ir::{
    block::{InstOrBlock, SimpleBlock},
    func::Function,
    inst::Inst,
    ty::Type,
    Module,
};

use super::inst::lower_inst;

pub fn lower_function_body(
    builder: &mut FunctionBuilder,
    module: &Module,
    func: &Function,
    simple_block: &SimpleBlock,
) {
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);
    builder.seal_block(entry);
    let has_return = matches!(simple_block
        .0
        .last(), Some(InstOrBlock::Inst(id)) if matches!(module.inst_arena[*id], Inst::Return(_)));

    for stmt in simple_block.0.iter() {
        match stmt {
            InstOrBlock::Inst(id) => {
                let inst = &module.inst_arena[*id];
                lower_inst(builder, module, func, inst);
            }
            InstOrBlock::Block(_id) => {
                todo!()
            }
        }
    }

    if !has_return {
        assert!(func.sig.ret == Type::signed_int());
        let ret = builder.ins().iconst(clif_ty::I32, 0);
        builder.ins().return_(&[ret]);
    }
}
