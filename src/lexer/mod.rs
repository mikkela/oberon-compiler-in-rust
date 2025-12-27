mod token;
mod lexer;

pub use token::{Token, TokenKind};

use crate::{diagnostics::Diagnostic, error::Result, span::{Span, SourceFile}};
use crate::lexer::lexer::{LexError, Lexer};

pub fn lex(src: &SourceFile) -> std::result::Result<Vec<Token>, LexError> {
    let code = &src.text;
    let mut lexer = Lexer::new(&code);

    lexer.next_token().map(|tok| vec![tok])
}