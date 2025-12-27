use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;
use crate::diagnostics::Diagnostic;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Error, PartialEq)]
pub enum LexError {
    #[error("Unexpected character: '{0}'")]
    UnexpectedChar(char),

    #[error("Invalid number literal: {0}")]
    InvalidNumber(String),

    #[error("Unexpected EOF")]
    UnexpectedEof,
}

impl From<LexError> for Diagnostic {
    fn from(err: LexError) -> Self {
        match err {
            LexError::UnexpectedChar (ch ) => {
                Diagnostic::error(format!("unexpected character '{ch}'"))
                    //.with_span(span)
            }
            LexError::InvalidNumber(text) => {
                Diagnostic::error(format!("invalid number literal '{text}'"))
                    //.with_span(span)
            }
            LexError::UnexpectedEof => {
                Diagnostic::error(format!("unexpected end of file"))
                    //.with_span(span)
            }
        }
    }
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            input: src.chars().peekable(),
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn next_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn is_hex_digit(ch: char) -> bool {
        ch.is_ascii_digit() || matches!(ch, 'A'..='F')
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self)-> bool {
        let mut comment_level = 1;
        while let Some(ch) = self.next_char() {
            match ch {
                '(' => {
                    if self.peek_char() == Some('*') {
                        comment_level += 1;
                        self.next_char();
                    }
                }
                '*' => {
                    if self.peek_char() == Some(')') {
                        comment_level -= 1;
                        self.next_char();
                    }
                }
                _ => {}
            }
            if comment_level == 0 {
                return true;
            }
        }
        return false;
    }
    fn read_number(&mut self, first_digit: char) -> Result<Token, LexError> {
        let mut s = String::new();
        s.push(first_digit);

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() || matches!(ch, 'A'..='F') {
                self.next_char();
                s.push(ch);
            } else {
                break;
            }
        }
        if self.peek_char() == Some('.') {
            self.next_char();
            s.push('.');

            // {digit} efter dot (can be 0 digits)
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    self.next_char();
                    s.push(ch);
                } else {
                    break;
                }
            }

            if self.peek_char() == Some('E') {
                self.next_char();
                s.push('E');

                if matches!(self.peek_char(), Some('+') | Some('-')) {
                    let sign = self.next_char().unwrap();
                    s.push(sign);
                }

                // mindst én digit kræves efter E (+/-)
                let Some(ch) = self.peek_char() else {
                    return Err(LexError::InvalidNumber(s.clone()))
                };
                if !ch.is_ascii_digit() {
                    return Err(LexError::InvalidNumber(s.clone()))
                }

                while let Some(d) = self.peek_char() {
                    if d.is_ascii_digit() {
                        self.next_char();
                        s.push(d);
                    } else {
                        break;
                    }
                }
            }

            let value = s.parse::<f64>()
                .map_err(|_| LexError::InvalidNumber(s.clone()))?;

            return Ok(Token::new(TokenKind::Real(value)));
        }
        if self.peek_char() == Some('H') || self.peek_char() == Some('X') {
            let ch = self.next_char().unwrap();
            s.push(ch);

            let hex_part = &s[..s.len() - 1];

            // Valider at alle tegn er hexDigit (0-9, A-F)
            if !hex_part.chars().all(Self::is_hex_digit) {
                return Err(LexError::InvalidNumber(s.clone()))
            }

            if ch == 'X' {
                let value = u8::from_str_radix(hex_part, 16)
                    .map_err(|_| LexError::InvalidNumber(s))?;

                return Ok(Token::new(TokenKind::Char(value)))
            }
            let value = i64::from_str_radix(hex_part, 16)
                .map_err(|_| LexError::InvalidNumber(s))?;

            return Ok(Token::new(TokenKind::Int(value)))
        }

        let value = s.parse::<i64>().map_err(|_| LexError::InvalidNumber(s.clone()))?;
        Ok(Token::new(TokenKind::Int(value)))
    }

    fn read_string(&mut self) -> Result<Token, LexError> {
        let mut s = String::new();

        while let Some(ch) = self.next_char() {
            if ch == '"' {
                return Ok(Token::new(TokenKind::String(s)))
            }
            else if ch.is_ascii() {
                s.push(ch);
            }
        }
        Err(LexError::UnexpectedEof)
    }
    fn read_ident_or_keyword(&mut self, first_char: char) -> Result<Token, LexError> {
        let mut s = String::new();
        s.push(first_char);

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_alphanumeric()  {
                s.push(ch);
                self.next_char();
            } else {
                break;
            }
        }

        let tok = match s.as_str() {
            "ARRAY" => TokenKind::Array,
            "BEGIN" => TokenKind::Begin,
            "BY" => TokenKind::By,
            "CASE" => TokenKind::Case,
            "CONST" => TokenKind::Const,
            "DIV" => TokenKind::Div,
            "DO" => TokenKind::Do,
            "ELSE" => TokenKind::Else,
            "ELSIF" => TokenKind::Elsif,
            "END" => TokenKind::End,
            "FALSE" => TokenKind::False,
            "FOR" => TokenKind::For,
            "IF" => TokenKind::If,
            "IMPORT" => TokenKind::Import,
            "IN" => TokenKind::In,
            "IS" => TokenKind::Is,
            "MOD" => TokenKind::Mod,
            "MODULE" => TokenKind::Module,
            "NIL" => TokenKind::Nil,
            "OF" => TokenKind::Of,
            "OR" => TokenKind::Or,
            "POINTER" => TokenKind::Pointer,
            "PROCEDURE" => TokenKind::Procedure,
            "RECORD" => TokenKind::Record,
            "REPEAT" => TokenKind::Repeat,
            "RETURN" => TokenKind::Return,
            "THEN" => TokenKind::Then,
            "TO" => TokenKind::To,
            "TRUE" => TokenKind::True,
            "TYPE" => TokenKind::Type,
            "UNTIL" => TokenKind::Until,
            "VAR" => TokenKind::Var,
            "WHILE" => TokenKind::While,
            _ => TokenKind::Ident(s),
        };

        Ok(Token::new(tok))
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let ch = match self.next_char() {
            Some(c) => c,
            None => return Ok(Token::new(TokenKind::Eof)),
        };

        match ch {
            c if c.is_ascii_alphabetic() => self.read_ident_or_keyword(c),
            c if c.is_ascii_digit() => self.read_number(c),
            '+' => Ok(Token::new(TokenKind::Plus)),
            '-' => Ok(Token::new(TokenKind::Minus)),
            '*' => Ok(Token::new(TokenKind::Star)),
            '/' => Ok(Token::new(TokenKind::Slash)),
            '~' => Ok(Token::new(TokenKind::Tilde)),
            '&' => Ok(Token::new(TokenKind::Ampersand)),
            '.' => {
                if self.peek_char() == Some('.') {
                    self.next_char();
                    Ok(Token::new(TokenKind::DotDot))
                } else { Ok(Token::new(TokenKind::Dot)) }
            }
            ',' => Ok(Token::new(TokenKind::Comma)),
            ';' => Ok(Token::new(TokenKind::SemiColon)),
            '|' => Ok(Token::new(TokenKind::Pipe)),
            '(' => {
                if self.peek_char() == Some('*') {
                    self.next_char();
                    if !self.skip_comment()
                        { return Err(LexError::UnexpectedEof) }
                    self.next_token()
                } else { Ok(Token::new(TokenKind::LParen)) }
            }
            '[' => Ok(Token::new(TokenKind::LParen)),
            '{' => Ok(Token::new(TokenKind::LCurly)),
            ':' => {
                if self.peek_char() == Some('=') {
                    self.next_char();
                    Ok(Token::new(TokenKind::Assign))
                } else { Ok(Token::new(TokenKind::Colon)) }
            }
            '^' => Ok(Token::new(TokenKind::VersatileMark)),
            '=' => Ok(Token::new(TokenKind::Equal)),
            '#' => Ok(Token::new(TokenKind::NotEqual)),
            '<' => {
                if self.peek_char() == Some('=') {
                    self.next_char();
                    Ok(Token::new(TokenKind::LessThanOrEqual))
                } else {
                    Ok(Token::new(TokenKind::LessThan))
                }
            }
            '>' => {
                if self.peek_char() == Some('=') {
                    self.next_char();
                    Ok(Token::new(TokenKind::GreaterThanOrEqual))
                } else {
                    Ok(Token::new(TokenKind::GreaterThan))
                }
            }
            ')' => Ok(Token::new(TokenKind::RParen)),
            ']' => Ok(Token::new(TokenKind::RSquare)),
            '}' => Ok(Token::new(TokenKind::RCurly)),
            '"' => self.read_string(),
            other => Err(LexError::UnexpectedChar(other)),
        }
    }

}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::{LexError, Lexer};
    use crate::lexer::{Token, TokenKind};

    fn collect_tokens(src: &str) -> Result<Vec<TokenKind>, LexError> {
        let mut lexer = Lexer::new(src);
        let mut tokens = Vec::new();

        loop {
            let tok = lexer.next_token()?.kind;  // propagér fejl
            let eof = tok == TokenKind::Eof;
            tokens.push(tok);
            if eof {
                break;
            }
        }

        Ok(tokens)
    }

    #[test]
    fn lex_simple_identifier() {
        let tokens = collect_tokens("monkey").unwrap();
        assert_eq!(tokens, vec![TokenKind::Ident("monkey".to_string()), TokenKind::Eof]);
    }

    #[test]
    fn lex_multiple_identifiers() {
        let tokens = collect_tokens("monkey1 monkey2").unwrap();
        assert_eq!(tokens, vec![TokenKind::Ident("monkey1".to_string()), TokenKind::Ident("monkey2".to_string()), TokenKind::Eof]);
    }

    #[test]
    fn lex_keywords() {
        let tokens = collect_tokens("IN BEGIN").unwrap();
        assert_eq!(tokens, vec![TokenKind::In, TokenKind::Begin, TokenKind::Eof]);
    }

    #[test]
    fn lex_keywords_in_lower() {
        let tokens = collect_tokens("then if").unwrap();
        assert_eq!(tokens, vec![TokenKind::Ident("then".to_string()), TokenKind::Ident("if".to_string()), TokenKind::Eof]);
    }

    #[test]
    fn lex_keywords_ident_mixed() {
        let tokens = collect_tokens("PROCEDURE div MODULE").unwrap();
        assert_eq!(tokens, vec![TokenKind::Procedure, TokenKind::Ident("div".to_string()), TokenKind::Module, TokenKind::Eof]);
    }

    #[test]
    fn lex_integer() {
        let tokens = collect_tokens("12345").unwrap();
        assert_eq!(tokens, vec![TokenKind::Int(12345), TokenKind::Eof]);
    }

    #[test]
    fn lex_hexadecimal() {
        let tokens = collect_tokens("0FFH").unwrap();
        assert_eq!(tokens, vec![TokenKind::Int(255), TokenKind::Eof]);
    }

    #[test]
    fn lex_hexadecimal_as_integer() {
        let tokens = collect_tokens("100H").unwrap();
        assert_eq!(tokens, vec![TokenKind::Int(256), TokenKind::Eof]);
    }

    #[test]
    fn lex_hexadecimal_without_h() {
        let mut lexer = Lexer::new("0FF");
        let result = lexer.next_token();

        assert_eq!(result, Err(LexError::InvalidNumber("0FF".parse().unwrap())));
    }

    #[test]
    fn lex_real_number() {
        let tokens = collect_tokens("123.45").unwrap();
        assert_eq!(tokens, vec![TokenKind::Real(123.45), TokenKind::Eof]);
    }

    #[test]
    fn lex_real_number_without_trailing_digits() {
        let tokens = collect_tokens("123.").unwrap();
        assert_eq!(tokens, vec![TokenKind::Real(123.0), TokenKind::Eof]);
    }

    #[test]
    fn lex_real_number_without_trailing_digits_and_with_exponent() {
        let tokens = collect_tokens("123.E02").unwrap();
        assert_eq!(tokens, vec![TokenKind::Real(12300.0), TokenKind::Eof]);
    }

    #[test]
    fn lex_real_number_with_plus_exponent() {
        let tokens = collect_tokens("123.45E+2").unwrap();
        assert_eq!(tokens, vec![TokenKind::Real(12345.0), TokenKind::Eof]);
    }

    #[test]
    fn lex_real_number_with_minus_exponent() {
        let tokens = collect_tokens("123.45E-2").unwrap();
        assert_eq!(tokens, vec![TokenKind::Real(1.2345), TokenKind::Eof]);
    }

    #[test]
    fn lex_real_number_without_sign_exponent() {
        let tokens = collect_tokens("123.45E2").unwrap();
        assert_eq!(tokens, vec![TokenKind::Real(12345.0), TokenKind::Eof]);
    }

    #[test]
    fn lex_real_number_without_dot() {
        let mut lexer = Lexer::new("123E+2");
        let result = lexer.next_token();

        assert_eq!(result, Err(LexError::InvalidNumber("123E".parse().unwrap())));
    }

    #[test]
    fn lex_simple_symbol() {
        let tokens = collect_tokens("{}").unwrap();
        assert_eq!(tokens, vec![TokenKind::LCurly, TokenKind::RCurly, TokenKind::Eof]);
    }

    #[test]
    fn lex_equal_and_colon() {
        let tokens = collect_tokens("=:+:=").unwrap();
        assert_eq!(tokens, vec![
            TokenKind::Equal,
            TokenKind::Colon,
            TokenKind::Plus,
            TokenKind::Assign,
            TokenKind::Eof]);
    }

    #[test]
    fn lex_dot_and_dotdot() {
        let tokens = collect_tokens(". . ...").unwrap();
        assert_eq!(tokens, vec![
            TokenKind::Dot,
            TokenKind::Dot,
            TokenKind::DotDot,
            TokenKind::Dot,
            TokenKind::Eof]);
    }

    #[test]
    fn lex_less_than_equal() {
        let tokens = collect_tokens("< =<= <=").unwrap();
        assert_eq!(tokens, vec![
            TokenKind::LessThan,
            TokenKind::Equal,
            TokenKind::LessThanOrEqual,
            TokenKind::LessThanOrEqual,
            TokenKind::Eof]);
    }

    #[test]
    fn lex_simple_comments() {
        let tokens = collect_tokens("((* Ignore *))").unwrap();
        assert_eq!(tokens, vec![
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Eof]);
    }

    #[test]
    fn lex_nested_comments() {
        let tokens = collect_tokens("(* (* (Ignore) *)*)").unwrap();
        assert_eq!(tokens, vec![TokenKind::Eof]);
    }

    #[test]
    fn lex_unclosed_comments() {
        let mut lexer = Lexer::new("(*(Ignore) )");
        let result = lexer.next_token();

        assert_eq!(result, Err(LexError::UnexpectedEof));
    }

    #[test]
    fn lex_simple_string() {
        let tokens = collect_tokens("\"monkey\"").unwrap();
        assert_eq!(tokens, vec![TokenKind::String("monkey".to_string()), TokenKind::Eof]);
    }

    #[test]
    fn lex_empty_string() {
        let tokens = collect_tokens("\"\"").unwrap();
        assert_eq!(tokens, vec![TokenKind::String("".to_string()), TokenKind::Eof]);
    }

    #[test]
    fn lex_character() {
        let tokens = collect_tokens("22X").unwrap();
        assert_eq!(tokens, vec![TokenKind::Char(34), TokenKind::Eof]);
    }

    #[test]
    fn lex_character_FF() {
        let tokens = collect_tokens("0FFX").unwrap();
        assert_eq!(tokens, vec![TokenKind::Char(255), TokenKind::Eof]);
    }

    #[test]
    fn lex_character_100() {
        let mut lexer = Lexer::new("100XX");
        let result = lexer.next_token();

        assert_eq!(result, Err(LexError::InvalidNumber("100X".parse().unwrap())));
    }
}