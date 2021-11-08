use crate::token::Token;
use anyhow::Result;
use std::path::PathBuf;

pub trait LexerLike {
    /// Returns the next token.
    fn next(&mut self) -> Result<Option<Token>>;

    /// Returns the file path the lexer currently reads.
    fn filepath(&self) -> Option<&PathBuf>;
}

impl LexerLike for std::vec::IntoIter<Token> {
    fn next(&mut self) -> Result<Option<Token>> {
        Ok(Iterator::next(self))
    }

    fn filepath(&self) -> Option<&PathBuf> {
        None
    }
}

#[test]
fn vec_tokens() {
    use crate::token::*;
    use sourceloc::SourceLoc;
    let tokens = vec![
        Token::new(
            kind::TokenKind::Ident("foo".to_string()),
            SourceLoc::new(0, 1),
        ),
        Token::new(
            kind::TokenKind::Int(kind::IntKind::Int(123)),
            SourceLoc::new(0, 5),
        ),
    ];
    let ll: &mut dyn LexerLike = &mut tokens.into_iter();
    assert!(ll.next().unwrap().is_some());
    assert!(ll.next().unwrap().is_some());
    assert!(ll.next().unwrap().is_none());
}
