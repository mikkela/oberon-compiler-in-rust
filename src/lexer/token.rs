use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    String(String),
    Int(i64),
    Real(f64),
    Char(u8),

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
    Assign,
    SemiColon,
    Colon,
    Comma,
    Dot,
    DotDot,
    LSquare,
    RSquare,
    LParen,
    RParen,
    LCurly,
    RCurly,

    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Tilde,
    Ampersand,
    Pipe,
    VersatileMark,
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}