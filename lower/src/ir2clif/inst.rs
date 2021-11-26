use cranelift::{
    frontend::FunctionBuilder,
    prelude::{InstBuilder, Value},
};
use long_ir::{func::Function, inst::Inst, Module};

use super::value::lower_value;

pub fn lower_inst(
    builder: &mut FunctionBuilder,
    module: &Module,
    func: &Function,
    inst: &Inst,
) -> Value {
    match inst {
        Inst::Add(_, _) => {
            todo!()
        }
        Inst::Return(Some(id)) => {
            let val = lower_inst(builder, module, func, &module.inst_arena[*id]);
            builder.ins().iadd(x, y);
            builder.ins().return_(&[val])
        }
        Inst::Return(None) => builder.ins().return_(&[]),
    }
}
