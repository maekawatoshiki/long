/// The kind of a token.
#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(String),
    OpeningParen,
    ClosingParen,
}

impl From<char> for TokenKind {
    fn from(c: char) -> Self {
        // TODO: Support more symbols.
        // TODO: Is it ok to panic if `c` is an unsupported symbol?
        match c {
            '(' => TokenKind::OpeningParen,
            ')' => TokenKind::ClosingParen,
            _ => unreachable!(),
        }
    }
}
