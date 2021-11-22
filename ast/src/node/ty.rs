/// A type node.
#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Bool,
    Char(Sign),
    Char16(Sign),
    Char32(Sign),
    WChar(Sign),
    Short(Sign),
    Int(Sign),
    Long(Sign),
    LongLong(Sign),
    Float,
    Double,
    Pointer(Box<Type>),
    LValueRef(Box<Type>),
    RValueRef(Box<Type>),
    Func(Box<FuncType>),
    // TODO: struct, union, enum, class...
}

/// The sign of a type.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Sign {
    Signed,
    Unsigned,
}

/// A function type.
#[derive(Debug, Clone)]
pub struct FuncType {
    pub ret: Type,
    // TODO: params
}

impl Type {
    pub fn into_func_type(self) -> Option<FuncType> {
        match self {
            Type::Func(func) => Some(*func),
            _ => None,
        }
    }
}

impl From<FuncType> for Type {
    fn from(func: FuncType) -> Self {
        Type::Func(Box::new(func))
    }
}
