use cranelift_codegen::ir::types as clif_ty;
use long_ir::ty as ir_ty;

pub fn convert_type(from: &ir_ty::Type) -> clif_ty::Type {
    match from {
        ir_ty::Type::Void => clif_ty::I8,
        ir_ty::Type::Int(_) => clif_ty::I32,
        _ => todo!(),
    }
}
