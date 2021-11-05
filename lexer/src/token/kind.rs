/// The kind of a token.
#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(String),
    Keyword(KeywordKind),
    Symbol(SymbolKind),
}

/// The kind of a symbol.
///
/// Currently, some symbols from <https://timsong-cpp.github.io/cppwp/n3337/lex.operators> are
/// not implemented.
#[derive(Debug, Clone)]
pub enum SymbolKind {
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    OpeningBoxBracket,
    ClosingBoxBracket,
    Comma,
    Semicolon,
    Colon,
    DoubleColon,
    Dot,
    Arrow,
    Inc,
    Dec,
    Add,
    Sub,
    Asterisk,
    Div,
    Mod,
    Not,
    BitwiseNot,
    And,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Xor,
    Or,
    LAnd,
    LOr,
    Question,
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignShl,
    AssignShr,
    AssignAnd,
    AssignOr,
    AssignXor,
    AssignLAnd,
    AssignLOr,
    Hash,
    Vararg,
}

/// The kind of a keyword.
#[derive(Debug, Clone)]
pub enum KeywordKind {
    Alignas,
    Continue,
    Friend,
    Register,
    True,
    Alignof,
    Decltype,
    Goto,
    ReinterpretCast,
    Try,
    Asm,
    Default,
    If,
    Return,
    Typedef,
    Auto,
    Delete,
    Inline,
    Short,
    Typeid,
    Bool,
    Do,
    Int,
    Signed,
    Typename,
    Break,
    Double,
    Long,
    Sizeof,
    Union,
    Case,
    DynamicCast,
    Mutable,
    Static,
    Unsigned,
    Catch,
    Else,
    Namespace,
    StatiAssert,
    Using,
    Char,
    Enum,
    New,
    StaticCast,
    Virtual,
    Char16T,
    Explicit,
    Noexcept,
    Struct,
    Void,
    Char32T,
    Export,
    Nullptr,
    Switch,
    Volatile,
    Class,
    Extern,
    Operator,
    Template,
    WcharT,
    Const,
    False,
    Private,
    This,
    While,
    Constexpr,
    Float,
    Protected,
    ThreadLocal,
    ConstCast,
    For,
    Public,
    Throw,
}

impl SymbolKind {
    pub(crate) fn at_least_starts_with(c: char) -> bool {
        matches!(
            c,
            '(' | ')'
                | '{'
                | '}'
                | '['
                | ']'
                | ','
                | ';'
                | ':'
                | '.'
                | '+'
                | '-'
                | '*'
                | '/'
                | '%'
                | '!'
                | '~'
                | '&'
                | '<'
                | '>'
                | '^'
                | '|'
                | '?'
                | '='
                | '#'
        )
    }

    pub(crate) fn from_char(c: char) -> Option<Self> {
        match c {
            '(' => Some(SymbolKind::OpeningParen),
            ')' => Some(SymbolKind::ClosingParen),
            '{' => Some(SymbolKind::OpeningBrace),
            '}' => Some(SymbolKind::ClosingBrace),
            '[' => Some(SymbolKind::OpeningBoxBracket),
            ']' => Some(SymbolKind::ClosingBoxBracket),
            ',' => Some(SymbolKind::Comma),
            ';' => Some(SymbolKind::Semicolon),
            ':' => Some(SymbolKind::Colon),
            '.' => Some(SymbolKind::Dot),
            '+' => Some(SymbolKind::Add),
            '-' => Some(SymbolKind::Sub),
            '*' => Some(SymbolKind::Asterisk),
            '/' => Some(SymbolKind::Div),
            '%' => Some(SymbolKind::Mod),
            '!' => Some(SymbolKind::Not),
            '~' => Some(SymbolKind::BitwiseNot),
            '&' => Some(SymbolKind::And),
            '<' => Some(SymbolKind::Lt),
            '>' => Some(SymbolKind::Gt),
            '^' => Some(SymbolKind::Xor),
            '|' => Some(SymbolKind::Or),
            '?' => Some(SymbolKind::Question),
            '=' => Some(SymbolKind::Assign),
            '#' => Some(SymbolKind::Hash),
            _ => None,
        }
    }

    pub(crate) fn from_two_chars(x: char, y: char) -> Option<SymbolKind> {
        match (x, y) {
            (':', ':') => Some(SymbolKind::DoubleColon),
            ('-', '>') => Some(SymbolKind::Arrow),
            ('+', '+') => Some(SymbolKind::Inc),
            ('-', '-') => Some(SymbolKind::Dec),
            ('<', '<') => Some(SymbolKind::Shl),
            ('>', '>') => Some(SymbolKind::Shr),
            ('<', '=') => Some(SymbolKind::Le),
            ('>', '=') => Some(SymbolKind::Ge),
            ('=', '=') => Some(SymbolKind::Eq),
            ('!', '=') => Some(SymbolKind::Ne),
            ('&', '&') => Some(SymbolKind::LAnd),
            ('|', '|') => Some(SymbolKind::LOr),
            ('+', '=') => Some(SymbolKind::AssignAdd),
            ('-', '=') => Some(SymbolKind::AssignSub),
            ('*', '=') => Some(SymbolKind::AssignMul),
            ('/', '=') => Some(SymbolKind::AssignDiv),
            ('%', '=') => Some(SymbolKind::AssignMod),
            ('&', '=') => Some(SymbolKind::AssignAnd),
            ('|', '=') => Some(SymbolKind::AssignOr),
            ('^', '=') => Some(SymbolKind::AssignXor),
            _ => None,
        }
    }
}

impl KeywordKind {
    /// Returns the kind of the keyword if `kwd` is a keyword.
    pub(crate) fn from_str(kwd: impl AsRef<str>) -> Option<Self> {
        match kwd.as_ref() {
            "alignas" => Some(Self::Alignas),
            "continue" => Some(Self::Continue),
            "friend" => Some(Self::Friend),
            "register" => Some(Self::Register),
            "true" => Some(Self::True),
            "alignof" => Some(Self::Alignof),
            "decltype" => Some(Self::Decltype),
            "goto" => Some(Self::Goto),
            "reinterpret_cast" => Some(Self::ReinterpretCast),
            "try" => Some(Self::Try),
            "asm" => Some(Self::Asm),
            "default" => Some(Self::Default),
            "if" => Some(Self::If),
            "return" => Some(Self::Return),
            "typedef" => Some(Self::Typedef),
            "auto" => Some(Self::Auto),
            "delete" => Some(Self::Delete),
            "inline" => Some(Self::Inline),
            "short" => Some(Self::Short),
            "typeid" => Some(Self::Typeid),
            "bool" => Some(Self::Bool),
            "do" => Some(Self::Do),
            "int" => Some(Self::Int),
            "signed" => Some(Self::Signed),
            "typename" => Some(Self::Typename),
            "break" => Some(Self::Break),
            "double" => Some(Self::Double),
            "long" => Some(Self::Long),
            "sizeof" => Some(Self::Sizeof),
            "union" => Some(Self::Union),
            "case" => Some(Self::Case),
            "dynamic_cast" => Some(Self::DynamicCast),
            "mutable" => Some(Self::Mutable),
            "static" => Some(Self::Static),
            "unsigned" => Some(Self::Unsigned),
            "catch" => Some(Self::Catch),
            "else" => Some(Self::Else),
            "namespace" => Some(Self::Namespace),
            "static_assert" => Some(Self::StatiAssert),
            "using" => Some(Self::Using),
            "char" => Some(Self::Char),
            "enum" => Some(Self::Enum),
            "new" => Some(Self::New),
            "static_cast" => Some(Self::StaticCast),
            "virtual" => Some(Self::Virtual),
            "char16_t" => Some(Self::Char16T),
            "explicit" => Some(Self::Explicit),
            "noexcept" => Some(Self::Noexcept),
            "struct" => Some(Self::Struct),
            "void" => Some(Self::Void),
            "char32_t" => Some(Self::Char32T),
            "export" => Some(Self::Export),
            "nullptr" => Some(Self::Nullptr),
            "switch" => Some(Self::Switch),
            "volatile" => Some(Self::Volatile),
            "class" => Some(Self::Class),
            "extern" => Some(Self::Extern),
            "operator" => Some(Self::Operator),
            "template" => Some(Self::Template),
            "wchar_t" => Some(Self::WcharT),
            "const" => Some(Self::Const),
            "false" => Some(Self::False),
            "private" => Some(Self::Private),
            "this" => Some(Self::This),
            "while" => Some(Self::While),
            "constexpr" => Some(Self::Constexpr),
            "float" => Some(Self::Float),
            "protected" => Some(Self::Protected),
            "thread_local" => Some(Self::ThreadLocal),
            "const_cast" => Some(Self::ConstCast),
            "for" => Some(Self::For),
            "public" => Some(Self::Public),
            "throw" => Some(Self::Throw),
            _ => None,
        }
    }
}
