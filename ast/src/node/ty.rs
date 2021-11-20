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
}

#[derive(Debug, Clone, Copy)]
pub enum Sign {
    Signed,
    Unsigned,
}
