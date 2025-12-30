use thiserror::Error;
use crate::ast::{Declaration, Expression, Import, Module};
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

#[derive(Debug, Error, PartialEq)]
pub enum ParserError {
    #[error("Unexpected token: '{token:?}'")]
    UnexpectedToken {
        token: Token,
    },

    #[error("Unexpected end of file")]
    UnexpectedEof,

    #[error("Invalid name '{name}'")]
    InvalidName { name: String },
}
pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn bump(&mut self) -> Token {
        let t = self.tokens[self.pos].clone();
        self.pos += 1;
        t
    }

    fn expect_kind(&mut self, expected: TokenKind) -> Result<Token, ParserError> {
        let tok = self.peek();

        if tok.kind == expected {
            Ok(self.bump())
        } else {
            Err(ParserError::UnexpectedToken { token: tok.clone() })
        }
    }

    fn expect_ident(&mut self) -> Result<(String, Span), ParserError> {
        let tok = self.peek();

        match &tok.kind {
            TokenKind::Ident(name) => {
                let span = tok.span;
                let name = name.clone();
                self.bump();
                Ok((name, span))
            }
            _ => Err(ParserError::UnexpectedToken { token: tok.clone() }),
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        self.expect_kind(TokenKind::Module)?;
        let name = self.expect_ident()?.0;
        self.expect_kind(TokenKind::SemiColon)?;
        let mut declarations = vec![];
        let imports = self.parse_imports();
        declarations.extend(self.parse_consts()?);
        self.expect_kind(TokenKind::End)?;
        let end_name = self.expect_ident()?.0;
        if name != end_name {
            return Err(ParserError::InvalidName { name: end_name });
        }
        self.expect_kind(TokenKind::Dot)?;
        Ok(Module {
            name,
            imports: imports?,
            declarations: declarations,
            stmts: vec![],
        })
    }

    fn parse_imports(&mut self) -> Result<Vec<Import>, ParserError> {
        let mut imports = vec![];
        if self.peek().kind == TokenKind::Import {
            self.bump();
            let mut first = true;
            while self.peek().kind != TokenKind::SemiColon {
                if !first {
                    self.expect_kind(TokenKind::Comma)?;
                }
                first = false;
                let name = self.expect_ident().unwrap().0;
                let imported_name = if self.peek().kind == TokenKind::Assign {
                    self.bump();
                    self.expect_ident()?.0
                } else {
                    name.clone()
                };
                imports.push(Import { name, imported_name });
            }
            self.expect_kind(TokenKind::SemiColon)?;
        }
        Ok(imports)
    }

    fn parse_consts(&mut self) -> Result<Vec<Declaration>, ParserError> {
        let mut consts =  vec![];
        if self.peek().kind == TokenKind::Const {
            self.bump();
            while matches!(self.peek().kind, TokenKind::Ident(_)) {
                let name = self.expect_ident()?.0;
                let public = if self.peek().kind == TokenKind::Star { self.bump(); true } else { false };
                self.expect_kind(TokenKind::Equal)?;
                let value = self.parse_expression();
                self.expect_kind(TokenKind::SemiColon)?;
                consts.push(Declaration::Const { name, value: value?, public });
            }
        }
        Ok(consts)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.bump();
        Ok(Expression::Int(0, Span::new(0, 0)))
    }
}

#[cfg(test)]
mod tests {
    use std::string::ParseError;
    use crate::ast::Declaration;
    use crate::lexer::{Token, TokenKind};
    use crate::parser::parser::ParserError::InvalidName;
    use crate::span::Span;

    #[test]
    fn parse_empty_module() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::End, Span::new(14, 17)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(17, 23)),
            Token::new(TokenKind::Dot, Span::new(23, 24)),
            Token::new(TokenKind::Eof, Span::new(24, 24))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.name, "monkey");
        assert_eq!(module.imports.len(), 0);
        assert_eq!(module.stmts.len(), 0);
        assert_eq!(module.declarations.len(), 0);
    }

    #[test]
    fn parse_module_with_invalid_end_name() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::End, Span::new(14, 17)),
            Token::new(TokenKind::Ident("monkey2".to_string()), Span::new(17, 23)),
            Token::new(TokenKind::Dot, Span::new(23, 24)),
            Token::new(TokenKind::Eof, Span::new(24, 24))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let result = parser.parse();
        assert_eq!(result, Err(InvalidName{
            name: "monkey2".to_string()
        }));
    }

    #[test]
    fn parse_import_list() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Import, Span::new(14, 20)),
            Token::new(TokenKind::Ident("foo".to_string()), Span::new(21, 24)),
            Token::new(TokenKind::Comma, Span::new(24, 25)),
            Token::new(TokenKind::Ident("bar".to_string()), Span::new(26, 29)),
            Token::new(TokenKind::Assign, Span::new(29, 30)),
            Token::new(TokenKind::Ident("baz".to_string()), Span::new(31, 34)),
            Token::new(TokenKind::SemiColon, Span::new(34, 35)),
            Token::new(TokenKind::End, Span::new(35, 38)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(38, 44)),
            Token::new(TokenKind::Dot, Span::new(44, 45)),
            Token::new(TokenKind::Eof, Span::new(45, 45))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.imports.len(), 2);
        assert_eq!(module.imports[0].name, "foo");
        assert_eq!(module.imports[0].imported_name, "foo");
        assert_eq!(module.imports[1].name, "bar");
        assert_eq!(module.imports[1].imported_name, "baz");
    }

    #[test]
    fn parse_empty_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::End, Span::new(19, 22)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(23, 29)),
            Token::new(TokenKind::Dot, Span::new(29, 30)),
            Token::new(TokenKind::Eof, Span::new(30, 30))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declarations.len(), 0);
    }

    #[test]
    fn parse_single_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Int(123), Span::new(24, 27)),
            Token::new(TokenKind::SemiColon, Span::new(27, 28)),
            Token::new(TokenKind::End, Span::new(28, 31)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(32, 38)),
            Token::new(TokenKind::Dot, Span::new(38, 39)),
            Token::new(TokenKind::Eof, Span::new(39, 39))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declarations.len(), 1);
        assert!(matches!(
            &module.declarations[0],
            Declaration::Const { name, .. } if name == "Foo"
        ));
    }

    #[test]
    fn parse_multiple_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),

            Token::new(TokenKind::Const, Span::new(14, 19)),

            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Int(123), Span::new(24, 27)),
            Token::new(TokenKind::SemiColon, Span::new(27, 28)),

            Token::new(TokenKind::Ident("Bar".to_string()), Span::new(29, 32)),
            Token::new(TokenKind::Star, Span::new(32, 33)),
            Token::new(TokenKind::Equal, Span::new(33, 34)),
            Token::new(TokenKind::Int(456), Span::new(34, 37)),
            Token::new(TokenKind::SemiColon, Span::new(37, 38)),

            Token::new(TokenKind::End, Span::new(38, 41)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(42, 48)),
            Token::new(TokenKind::Dot, Span::new(48, 49)),
            Token::new(TokenKind::Eof, Span::new(49, 49))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declarations.len(), 2);
        assert!(matches!(
            &module.declarations[0],
            Declaration::Const { name, public, .. } if name == "Foo" && !*public
        ));
        assert!(matches!(
            &module.declarations[1],
            Declaration::Const { name, public, .. } if name == "Bar" && *public
        ));
    }
}