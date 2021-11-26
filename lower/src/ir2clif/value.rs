use cranelift::{
    frontend::FunctionBuilder,
    prelude::{InstBuilder, Value},
};
use cranelift_codegen::ir::types as clif_ty;
use long_ir::{
    func::Function,
    value::{IntValue, Value as IrValue},
    Module,
};

pub fn lower_value(
    builder: &mut FunctionBuilder,
    _module: &Module,
    _func: &Function,
    value: &IrValue,
) -> Value {
    match value {
        IrValue::Int(IntValue::Int32(i)) => builder.ins().iconst(clif_ty::I32, *i as i64),
    }
}
