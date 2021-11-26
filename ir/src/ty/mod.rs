/// A type node.
pub enum Type<'a> {
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
    Pointer(&'a Type<'a>),
    LValueRef(&'a Type<'a>),
    RValueRef(&'a Type<'a>),
    Func(Box<FuncType<'a>>),
    // TODO: struct, union, enum, class...
}

/// The sign of a type.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Sign {
    Signed,
    Unsigned,
}

/// A function type.
pub struct FuncType<'a> {
    pub ret: &'a Type<'a>,
    // TODO: params
}

impl<'a> Type<'a> {
    pub fn into_func_type(self) -> Option<FuncType<'a>> {
        match self {
            Type::Func(func) => Some(*func),
            _ => None,
        }
    }
}

impl<'a> From<FuncType<'a>> for Type<'a> {
    fn from(func: FuncType<'a>) -> Self {
        Type::Func(Box::new(func))
    }
}
