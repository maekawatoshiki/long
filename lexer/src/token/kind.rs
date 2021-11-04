/// The kind of a token.
#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(String),
    OpeningParen,
    ClosingParen,
    OpeningBrace,
    ClosingBrace,
    OpeningBoxBracket,
    ClosingBoxBracket,
    Comma,
    Semicolon,
    Colon,
    Point,
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
    Ampersand,
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
    AssignXor,
    AssignOr,
    Hash,
    Vararg,
    Sizeof,
    Symbol(SymbolKind),
}

/// The kind of a symbol.
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
    AssignXor,
    AssignOr,
    Hash,
    Vararg,
    Sizeof,
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
}

impl From<char> for SymbolKind {
    fn from(c: char) -> Self {
        // TODO: Support more symbols.
        // TODO: Is it ok to panic if `c` is an unsupported symbol?
        match c {
            '(' => SymbolKind::OpeningParen,
            ')' => SymbolKind::ClosingParen,
            '{' => SymbolKind::OpeningBrace,
            '}' => SymbolKind::ClosingBrace,
            '[' => SymbolKind::OpeningBoxBracket,
            ']' => SymbolKind::ClosingBoxBracket,
            ',' => SymbolKind::Comma,
            ';' => SymbolKind::Semicolon,
            ':' => SymbolKind::Colon,
            '.' => SymbolKind::Dot,
            '+' => SymbolKind::Add,
            '-' => SymbolKind::Sub,
            '*' => SymbolKind::Asterisk,
            '/' => SymbolKind::Div,
            '%' => SymbolKind::Mod,
            '!' => SymbolKind::Not,
            '~' => SymbolKind::BitwiseNot,
            '&' => SymbolKind::And,
            '<' => SymbolKind::Lt,
            '>' => SymbolKind::Gt,
            '^' => SymbolKind::Xor,
            '|' => SymbolKind::Or,
            '?' => SymbolKind::Question,
            '=' => SymbolKind::Assign,
            '#' => SymbolKind::Hash,
            _ => unreachable!(),
        }
    }
}
