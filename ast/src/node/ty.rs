#[derive(Debug, Clone, Copy)]
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
    // TODO: struct, union, enum, class...
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Sign {
    Signed,
    Unsigned,
}
