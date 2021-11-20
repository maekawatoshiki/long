use anyhow::Result;
use ast::token::{kind::TokenKind, Token};
use std::path::PathBuf;

pub trait LexerLike {
    /// Returns the next token.
    fn next(&mut self) -> Result<Option<Token>>;

    /// Peeks the next token.
    fn peek(&mut self) -> Result<Option<Token>>;

    /// Ungets the given `token`.
    fn unget(&mut self, token: Token);

    /// Skips the next token only if it matches the given kind.
    fn skip(&mut self, kind: TokenKind) -> bool;

    /// Returns the file path the lexer currently reads.
    fn filepath(&self) -> Option<&PathBuf>;
}

pub struct TokenStream {
    tokens: Vec<Token>,
    buf: Vec<Token>,
    cur: usize,
}

impl TokenStream {
    /// Creates a new `TokenStream`.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            buf: Vec::new(),
            cur: 0,
        }
    }
}

impl LexerLike for TokenStream {
    fn next(&mut self) -> Result<Option<Token>> {
        if let Some(tok) = self.buf.pop() {
            return Ok(Some(tok));
        }
        self.cur += 1;
        Ok(self.tokens.get(self.cur - 1).cloned())
    }

    fn peek(&mut self) -> Result<Option<Token>> {
        if let Some(tok) = self.buf.last().cloned() {
            return Ok(Some(tok));
        }
        Ok(self.tokens.get(self.cur).cloned())
    }

    fn unget(&mut self, token: Token) {
        self.buf.push(token);
    }

    fn skip(&mut self, kind: TokenKind) -> bool {
        if let Ok(Some(tok)) = self.next() {
            if tok.kind() == &kind {
                return true;
            }
            self.unget(tok);
        }
        false
    }

    fn filepath(&self) -> Option<&PathBuf> {
        None
    }
}

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> Self {
        Self::new(tokens)
    }
}

#[test]
fn vec_tokens() {
    use ast::token::*;
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
    let ll: &mut dyn LexerLike = &mut TokenStream::from(tokens);
    assert!(ll.next().unwrap().is_some());
    assert!(ll.next().unwrap().is_some());
    assert!(ll.next().unwrap().is_none());
}

#[test]
fn vec_tokens_skip() {
    use ast::token::*;
    use sourceloc::SourceLoc;
    let tokens = vec![
        Token::new(kind::SymbolKind::Add.into(), SourceLoc::new(0, 1)),
        Token::new(kind::IntKind::Int(123).into(), SourceLoc::new(0, 5)),
    ];
    let ll: &mut dyn LexerLike = &mut TokenStream::from(tokens);
    assert!(ll.skip(kind::SymbolKind::Add.into()));
    assert!(!ll.skip(kind::SymbolKind::Add.into()));
    assert!(ll.skip(kind::IntKind::Int(123).into()));
    assert!(!ll.skip(kind::IntKind::Int(123).into()));
}
