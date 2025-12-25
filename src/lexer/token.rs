use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Int(i64),
    Real(f64),

    // Keywords (stub)
    Array,
    Begin,
    By,
    Case,
    Const,
    Div,
    Do,
    Else,
    Elsif,
    End,
    False,
    For,
    If,
    Import,
    In,
    Is,
    Mod,
    Module,
    Nil,
    Of,
    Or,
    Pointer,
    Procedure,
    Record,
    Repeat,
    Return,
    Then,
    To,
    True,
    Type,
    Until,
    Var,
    While,

    // Symbols
    Assign,      // :=
    Semi,        // ;
    Colon,       // :
    Comma,       // ,
    Dot,         // .
    LParen,
    RParen,

    Plus,
    Minus,
    Star,
    Slash,

    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind, span: Span { start: 0, end: 0 } }
    }
}