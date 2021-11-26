use cranelift::{frontend::FunctionBuilder, prelude::InstBuilder};
use long_ir::{func::Function, inst::Inst, Module};

use super::value::lower_value;

pub fn lower_inst(builder: &mut FunctionBuilder, module: &Module, func: &Function, inst: &Inst) {
    match inst {
        Inst::Return(Some(id)) => {
            let val = lower_value(builder, module, func, &module.val_arena.0[*id]);
            builder.ins().return_(&[val])
        }
        Inst::Return(None) => builder.ins().return_(&[]),
    };
}
