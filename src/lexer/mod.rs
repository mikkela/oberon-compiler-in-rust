mod token;
mod lexer;

pub use token::{Token, TokenKind};

use crate::lexer::lexer::{LexError, Lexer};
use crate::span::SourceFile;

pub fn lex(src: &SourceFile) -> std::result::Result<Vec<Token>, LexError> {
    let code = &src.text;
    let mut lexer = Lexer::new(&code);

    lexer.next_token().map(|tok| vec![tok])
}