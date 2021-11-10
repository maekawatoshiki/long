use crate::token::{kind::TokenKind, Token};
use anyhow::Result;
use std::path::PathBuf;

pub trait LexerLike {
    /// Returns the next token.
    fn next(&mut self) -> Result<Option<Token>>;

    /// Skips the next token only if it matches the given kind.
    fn skip(&mut self, kind: TokenKind) -> bool;

    /// Returns the file path the lexer currently reads.
    fn filepath(&self) -> Option<&PathBuf>;
}

impl LexerLike for std::vec::IntoIter<Token> {
    fn next(&mut self) -> Result<Option<Token>> {
        Ok(Iterator::next(self))
    }

    fn skip(&mut self, kind: TokenKind) -> bool {
        // Unfortunately, we can't use `Peekable` here...
        let skipped = Iterator::next(&mut (*self).clone()).map_or(false, |t| t.kind == kind);
        if skipped {
            LexerLike::next(self).unwrap();
        }
        skipped
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

#[test]
fn vec_tokens_skip() {
    use crate::token::*;
    use sourceloc::SourceLoc;
    let tokens = vec![
        Token::new(kind::SymbolKind::Add.into(), SourceLoc::new(0, 1)),
        Token::new(kind::IntKind::Int(123).into(), SourceLoc::new(0, 5)),
    ];
    let ll: &mut dyn LexerLike = &mut tokens.into_iter();
    assert!(ll.skip(kind::SymbolKind::Add.into()));
    assert!(!ll.skip(kind::SymbolKind::Add.into()));
    assert!(ll.skip(kind::IntKind::Int(123).into()));
    assert!(!ll.skip(kind::IntKind::Int(123).into()));
}
