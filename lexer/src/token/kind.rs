/// The kind of a token.
#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(String),
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
