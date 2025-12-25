mod token;
mod lexer;

pub use token::{Token, TokenKind};

use crate::{diagnostics::Diagnostic, error::Result, span::{Span, SourceFile}};

pub fn lex(src: &SourceFile) -> Result<Vec<Token>> {
    let bytes = src.text.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::new();

    while i < bytes.len() {
        let c = bytes[i] as char;

        // whitespace
        if c.is_whitespace() {
            i += 1;
            continue;
        }

        // identifiers / keywords
        if c.is_ascii_alphabetic() || c == '_' {
            let start = i;
            i += 1;
            while i < bytes.len() {
                let ch = bytes[i] as char;
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    i += 1;
                } else {
                    break;
                }
            }
            let sp = Span::new(start, i);
            let s = &src.text[start..i];
            let kind = match s {
                "MODULE" => TokenKind::Module,
                "BEGIN" => TokenKind::Begin,
                "END" => TokenKind::End,
                "VAR" => TokenKind::Var,
                "CONST" => TokenKind::Const,
                "PROCEDURE" => TokenKind::Procedure,
                "RETURN" => TokenKind::Return,
                _ => TokenKind::Ident(s.to_string()),
            };
            out.push(Token { kind, span: sp });
            continue;
        }

        // integer literals
        if c.is_ascii_digit() {
            let start = i;
            i += 1;
            while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                i += 1;
            }
            let sp = Span::new(start, i);
            let s = &src.text[start..i];
            let v = s.parse::<i64>().unwrap();
            out.push(Token { kind: TokenKind::Int(v), span: sp });
            continue;
        }

        // symbols
        let start = i;
        let tok = match c {
            ':' if i + 1 < bytes.len() && bytes[i + 1] as char == '=' => {
                i += 2;
                TokenKind::Assign
            }
            ';' => { i += 1; TokenKind::Semi }
            ':' => { i += 1; TokenKind::Colon }
            ',' => { i += 1; TokenKind::Comma }
            '.' => { i += 1; TokenKind::Dot }
            '(' => { i += 1; TokenKind::LParen }
            ')' => { i += 1; TokenKind::RParen }
            '+' => { i += 1; TokenKind::Plus }
            '-' => { i += 1; TokenKind::Minus }
            '*' => { i += 1; TokenKind::Star }
            '/' => { i += 1; TokenKind::Slash }
            _ => {
                return Err(Diagnostic::error(format!("unexpected character: '{c}'"))
                    .with_span(Span::new(start, start + 1))
                    .with_source(src.clone()));
            }
        };

        out.push(Token { kind: tok, span: Span::new(start, i) });
    }

    out.push(Token { kind: TokenKind::Eof, span: Span::new(bytes.len(), bytes.len()) });
    Ok(out)
}