use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Error, PartialEq)]
pub enum LexError {
    #[error("Unexpected character: '{0}'")]
    UnexpectedChar(char),

    #[error("Invalid number literal: {0}")]
    InvalidNumber(String),
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
        if self.peek_char() == Some('H') {
            self.next_char();
            s.push('H');

            let hex_part = &s[..s.len() - 1];

            // Valider at alle tegn er hexDigit (0-9, A-F)
            if !hex_part.chars().all(Self::is_hex_digit) {
                return Err(LexError::InvalidNumber(s.clone()))
            }

            let value = i64::from_str_radix(hex_part, 16)
                .map_err(|_| LexError::InvalidNumber(s))?;

            return Ok(Token::new(TokenKind::Int(value)))
        }

        let value = s.parse::<i64>().map_err(|_| LexError::InvalidNumber(s.clone()))?;
        Ok(Token::new(TokenKind::Int(value)))
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
            other => Err(LexError::UnexpectedChar(other)),
        }


        /*



        match ch {
            c if c.is_ascii_digit() => self.read_number(c),
            c if c.is_ascii_alphabetic() || c == '_' => self.read_ident_or_keyword(c),

            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '*' => Ok(Token::Star),
            '/' => Ok(Token::Slash),
            '=' => Ok(Token::Equals),
            '<' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Ok(Token::LTE)
                    }
                    Some('>') => {
                        self.next_char();
                        Ok(Token::LTGT)
                    }
                    _ => Ok(Token::LT),
                }
            }
            '>' => {
                match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Ok(Token::GTE)
                    }
                    _ => Ok(Token::GT),
                }
            }
            '&' => {
                match self.peek_char() {
                    Some('&') => {
                        self.next_char();
                        Ok(Token::And)
                    }
                    _ => Ok(Token::Ampersand),
                }
            }
            '|' => {
                match self.peek_char() {
                    Some('|') => {
                        self.next_char();
                        Ok(Token::Or)
                    }
                    _ => Ok(Token::Pipe),
                }
            }

            other => Err(LexError::UnexpectedChar(other)),
        }


        */
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
}