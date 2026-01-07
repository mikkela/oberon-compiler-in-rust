use crate::ast::Expression::Designator;
use crate::ast::Selector::Qualify;
use crate::ast::{ActualParameters, Declaration, Element, Expression, Identifier, IdentifierDef, Import, Module, QualifiedIdentifier, Type};
use crate::lexer::TokenKind::{Array, Ident, LParen, RParen};
use crate::lexer::{Token, TokenKind};
use crate::span::Span;
use thiserror::Error;

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

    fn expect_ident(&mut self) -> Result<Identifier, ParserError> {
        let tok = self.peek().clone();

        match &tok.kind {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.bump();
                Ok(Identifier { identifier: name, span: tok.span } )
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    fn expect_number(&mut self) -> Result<Expression, ParserError> {
        let tok = self.peek().clone();

        match &tok.kind {
            TokenKind::Int(value) => {
                self.bump();
                Ok(Expression::Int { value: *value, span: tok.span } )
            }
            TokenKind::Real(value) => {
                self.bump();
                Ok(Expression::Real { value: *value, span: tok.span } )
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    fn expect_string(&mut self) -> Result<Expression, ParserError> {
        let tok = self.peek().clone();

        match &tok.kind {
            TokenKind::String(value) => {
                self.bump();
                Ok(Expression::String { value: value.to_string(), span: tok.span } )
            }
            TokenKind::Char(value) => {
                self.bump();
                Ok(Expression::String { value: char::from(*value).to_string(), span: tok.span } )
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    fn expect_boolean(&mut self) -> Result<Expression, ParserError> {
        let tok = self.peek().clone();

        match &tok.kind {
            TokenKind::False => {
                self.bump();
                Ok(Expression::Bool { value: false, span: tok.span } )
            }
            TokenKind::True => {
                self.bump();
                Ok(Expression::Bool { value: true, span: tok.span } )
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        let begin = self.expect_kind(TokenKind::Module)?;
        let first_ident = self.expect_ident()?;
        self.expect_kind(TokenKind::SemiColon)?;
        let import_list = self.parse_import_list()?;
        let declaration_sequence = self.parse_declaration_sequence()?;
        self.expect_kind(TokenKind::End)?;
        let second_ident = self.expect_ident()?;
        let end = self.expect_kind(TokenKind::Dot)?;
        Ok(Module {
            first_ident,
            second_ident,
            import_list,
            declaration_sequence,
            stmts: vec![],
            span: Span { start: begin.span.start, end: end.span.end},
        })
    }

    fn parse_declaration_sequence(&mut self) -> Result<Vec<Declaration>, ParserError> {
        let mut declarations = vec![];
        declarations.extend(self.parse_const_declarations()?);
        declarations.extend(self.parse_type_declarations()?);
        Ok(declarations)
    }

    fn parse_import_list(&mut self) -> Result<Vec<Import>, ParserError> {
        let mut imports = vec![];
        if self.peek().kind == TokenKind::Import {
            self.bump();
            let mut first = true;
            while self.peek().kind != TokenKind::SemiColon {
                if !first {
                    self.expect_kind(TokenKind::Comma)?;
                }
                first = false;
                let first_ident = self.expect_ident()?;
                let span_begin = first_ident.span.start;
                let mut span_end = first_ident.span.end;
                let second_ident = if self.peek().kind == TokenKind::Assign {
                    self.bump();
                    let ident = self.expect_ident()?;
                    span_end = ident.span.end;
                   Some( ident)
                } else {
                    None
                };
                imports.push(
                    Import { first_ident, second_ident, span: Span { start: span_begin, end: span_end} });
            }
            self.expect_kind(TokenKind::SemiColon)?;
        }
        Ok(imports)
    }

    fn parse_const_declarations(&mut self) -> Result<Vec<Declaration>, ParserError> {
        let mut consts =  vec![];
        if self.peek().kind == TokenKind::Const {
            self.bump();
            while matches!(self.peek().kind, TokenKind::Ident(_)) {
                let name = self.expect_ident()?;
                let star =
                    if self.peek().kind == TokenKind::Star { Some(self.expect_kind(TokenKind::Star)?) } else { None };
                self.expect_kind(TokenKind::Equal)?;
                let value = self.parse_expression()?;
                let semicolon = self.expect_kind(TokenKind::SemiColon)?;

                let ident = Self::create_identifier_def(&name, star);

                consts.push(Declaration::Const {
                    ident,
                    expression: value,
                    span: Span { start: name.span.start, end: semicolon.span.end },
                });
            }
        }
        Ok(consts)
    }

    fn create_identifier_def(name: &Identifier, star: Option<Token>) -> IdentifierDef {
        let ident = IdentifierDef {
            identifier: name.clone(),
            star: star.is_some(),
            span:
            if star.is_some()
            { Span { start: name.span.start, end: star.unwrap().span.end } } else { name.span },
        };
        ident
    }

    fn parse_type_declarations(&mut self) -> Result<Vec<Declaration>, ParserError> {
        let mut types =  vec![];
        if self.peek().kind == TokenKind::Type {
            self.bump();
            while matches!(self.peek().kind, TokenKind::Ident(_)) {
                let name = self.expect_ident()?;
                let star =
                    if self.peek().kind == TokenKind::Star { Some(self.expect_kind(TokenKind::Star)?) } else { None };
                self.expect_kind(TokenKind::Equal)?;
                let ty = self.parse_type()?;
                let semicolon = self.expect_kind(TokenKind::SemiColon)?;

                let ident = Self::create_identifier_def(&name, star);

                types.push(Declaration::Type {
                    ident,
                    ty,
                    span: Span { start: name.span.start, end: semicolon.span.end },
                });
            }
        }
        Ok(types)
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let peek = self.peek().clone();
        match peek.kind  {
            Ident(_) => {
                let mut second_ident = self.expect_ident()?;
                let mut span = second_ident.span;
                let mut first_ident = None;
                if self.peek().kind == TokenKind::Dot {
                    self.expect_kind(TokenKind::Dot)?;
                    first_ident = Some(second_ident);
                    second_ident = self.expect_ident()?;
                    span = Span { start: span.start, end: second_ident.span.end };
                }
                Ok(Type::Named { name: QualifiedIdentifier {
                    first_ident,
                    second_ident,
                    span,
                }, span })
            }
            Array => {
                let start = self.expect_kind(TokenKind::Array)?.span.start;
                let mut lengths = vec![self.parse_expression()?];
                while self.peek().kind == TokenKind::Comma {
                    self.bump();
                    lengths.push(self.parse_expression()?);
                }
                self.expect_kind(TokenKind::Of)?;
                let element = self.parse_type()?;
                Ok(Type::Array {
                    lengths,
                    element: Box::new(element.clone()),
                    span: Span { start, end: element.span().end },
                })
            }
            _ => Err(ParserError::UnexpectedToken { token: peek })
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_simple_expression()
    }

    fn parse_simple_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<Expression, ParserError> {
        self.parse_factor()
    }

    fn parse_factor(&mut self) -> Result<Expression, ParserError> {
        let peek = self.peek().clone();
        match peek.kind {
            TokenKind::Int(_) => self.expect_number(),
            TokenKind::Real(_) => self.expect_number(),
            TokenKind::String(_) => self.expect_string(),
            TokenKind::Char(_) => self.expect_string(),
            TokenKind::False => self.expect_boolean(),
            TokenKind::True => self.expect_boolean(),
            TokenKind::Nil =>
                {
                    self.bump();
                    Ok(Expression::Nil { span: peek.span })
                }
            TokenKind::LCurly => {
                self.bump();
                let mut elements = vec![self.parse_element()?.into()];
                while self.peek().kind == TokenKind::Comma {
                    self.bump();
                    elements.push(self.parse_element()?.into());
                }
                let end = self.expect_kind(TokenKind::RCurly)?;
                Ok(Expression::Set { value: elements, span: Span {
                    start: peek.span.start, end: end.span.end
                } })
            }
            TokenKind::Ident(_) => {
                let first_ident = self.expect_ident()?;
                match self.peek().kind {
                    TokenKind::Dot => {
                        self.bump();
                        let second_ident = self.expect_ident()?;
                        match self.peek().kind {
                            TokenKind::Dot => {
                                self.bump();
                                let third_ident = self.expect_ident()?;
                                let parameters = self.parse_actual_parameters()?;
                                Ok(Designator {
                                    target: QualifiedIdentifier {
                                        first_ident: Some(first_ident.clone()),
                                        second_ident: second_ident.clone(),
                                        span: Span { start: first_ident.span.start, end: second_ident.span.end },
                                    },
                                    selector: Qualify { name: third_ident.clone(), span: third_ident.span },
                                    parameters: parameters.clone(),
                                    span: Span {
                                        start: first_ident.span.start,
                                        end: if parameters.is_none() {third_ident.span.end } else { parameters.unwrap().span.end}
                                    },
                                })
                            }
                            _ => {
                                let parameters = self.parse_actual_parameters()?;

                                Ok(Designator {
                                    target: QualifiedIdentifier {
                                        first_ident: None,
                                        second_ident: first_ident.clone(),
                                        span: first_ident.span,
                                    },
                                    selector: Qualify { name: second_ident.clone(), span: second_ident.span },
                                    parameters: parameters.clone(),
                                    span: Span {
                                        start: first_ident.span.start,
                                        end: if parameters.is_none() {second_ident.span.end } else { parameters.unwrap().span.end}
                                    }
                                })
                            }

                        }
                    }
                    _ => todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn parse_element(&mut self) -> Result<Element, ParserError> {
        let first =self.parse_expression()?;
        let second = if self.peek().kind == TokenKind::DotDot {
            self.bump();
            Some(self.parse_expression()?)
        } else { None};
        let s = second.clone();
        let span = Span::new(first.span().start, if s.is_some() { s.unwrap().span().end } else { first.span().end });
        Ok(Element {
            first_expression: first.into(),
            second_expression: if second.is_some() { Some(second.unwrap().into()) } else { None },
            span,
        })
    }

    fn parse_actual_parameters(&mut self) -> Result<Option<ActualParameters>, ParserError> {
        match self.peek().kind {
            LParen => {
                let left_par = self.bump();
                let mut expressions = vec![self.parse_expression()?];
                while self.peek().kind == TokenKind::Comma {
                    self.bump();
                    expressions.push(self.parse_expression()?);
                }

                let right_par = self.expect_kind(RParen)?;
                Ok(Some(
                    ActualParameters{
                        parameters: expressions,
                        span: Span { start: left_par.span.start, end: right_par.span.end },
                    }
                ))
            }
            _ => Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Declaration, Expression, Identifier, IdentifierDef, QualifiedIdentifier, Type};
    use crate::lexer::{Token, TokenKind};
    use crate::span::Span;
    use std::ops::Deref;
    use Expression::Int;

    #[test]
    fn parse_empty_module() {
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
        let module = parser.parse().unwrap();
        assert_eq!(module.first_ident.identifier, "monkey");
        assert_eq!(module.second_ident.identifier, "monkey2");
        assert_eq!(module.import_list.len(), 0);
        assert_eq!(module.stmts.len(), 0);
        assert_eq!(module.declaration_sequence.len(), 0);
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
        assert_eq!(module.import_list.len(), 2);
        let first_import = module.import_list[0].clone();
        assert_eq!(first_import.first_ident.identifier, "foo");
        assert_eq!(first_import.second_ident, None);
        let second_import = module.import_list[1].clone();
        assert_eq!(second_import.first_ident.identifier, "bar");
        assert_eq!(second_import.second_ident.unwrap().identifier, "baz");
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
        assert_eq!(module.declaration_sequence.len(), 0);
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
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(matches!(
            &module.declaration_sequence[0],
            Declaration::Const { ident: IdentifierDef { identifier: Identifier { identifier, ..}, .. }, .. }if identifier == "Foo"
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
        assert_eq!(module.declaration_sequence.len(), 2);
        assert!(matches!(
            &module.declaration_sequence[0],
            Declaration::Const { ident: IdentifierDef { identifier: Identifier {identifier, ..}, star, ..}, .. } if identifier == "Foo" && !*star
        ));
        assert!(matches!(
            &module.declaration_sequence[1],
            Declaration::Const { ident: IdentifierDef { identifier: Identifier {identifier, ..}, star, ..}, .. } if identifier == "Bar" && *star
        ));
    }

    #[test]
    fn parse_empty_type() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Type, Span::new(14, 19)),
            Token::new(TokenKind::End, Span::new(19, 22)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(23, 29)),
            Token::new(TokenKind::Dot, Span::new(29, 30)),
            Token::new(TokenKind::Eof, Span::new(30, 30))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 0);
    }

    #[test]
    fn parse_named_type() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Type, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Ident("M".to_string()),Span::new(24, 27)),
            Token::new(TokenKind::Dot, Span::new(27, 29)),
            Token::new(TokenKind::Ident("B".to_string()), Span::new(29, 30)),
            Token::new(TokenKind::SemiColon, Span::new(30, 31)),
            Token::new(TokenKind::End, Span::new(31, 32)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(32, 38)),
            Token::new(TokenKind::Dot, Span::new(38, 39)),
            Token::new(TokenKind::Eof, Span::new(39, 39))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(matches!(
            &module.declaration_sequence[0],
            Declaration::Type {
                ident: IdentifierDef{ identifier: Identifier { ..}, .. },
                ty: Type::Named {
                    name: QualifiedIdentifier {
                        first_ident : Some(Identifier { identifier, ..}),
                        second_ident,
                        ..},
                    .. },
                ..
            } if identifier == "M" && second_ident.identifier == "B"
        ));
    }

    #[test]
    fn parse_array_type() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Type, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Array, Span::new(24, 29)),
            Token::new(TokenKind::Int(2), Span::new(29, 31)),
            Token::new(TokenKind::Comma, Span::new(31, 32)),
            Token::new(TokenKind::Int(3), Span::new(32, 33)),
            Token::new(TokenKind::Of, Span::new(33, 35)),
            Token::new(TokenKind::Ident("T".to_string()),Span::new(36, 39)),
            Token::new(TokenKind::SemiColon, Span::new(39, 40)),
            Token::new(TokenKind::End, Span::new(40, 41)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(41, 47)),
            Token::new(TokenKind::Dot, Span::new(47, 48)),
            Token::new(TokenKind::Eof, Span::new(48, 48))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_type());
        let (n, t) = module.declaration_sequence[0].as_type().unwrap();
        assert_eq!(n.identifier.identifier, "Foo");
        assert_eq!(n.star, false);
        assert!(t.is_array());
        let (lengths, base) = t.as_array().unwrap();
        assert_eq!(lengths, vec![
            Int{ value: 2, span: Span::new(29, 31)},
            Int{ value:3, span: Span::new(32, 33)}
        ]);
        assert!(base.is_named());
        let named = base.as_named().unwrap();
        assert_eq!(named.first_ident, None);
        assert_eq!(named.second_ident.identifier, "T");
    }

    #[test]
    fn parse_set_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::LCurly, Span::new(24, 26)),
            Token::new(TokenKind::Int(1), Span::new(26, 27)),
            Token::new(TokenKind::Comma, Span::new(27, 28)),
            Token::new(TokenKind::Int(2), Span::new(28, 29)),
            Token::new(TokenKind::DotDot, Span::new(29, 31)),
            Token::new(TokenKind::Int(3), Span::new(31, 32)),
            Token::new(TokenKind::RCurly, Span::new(32, 33)),
            Token::new(TokenKind::SemiColon, Span::new(27, 28)),
            Token::new(TokenKind::End, Span::new(28, 31)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(32, 38)),
            Token::new(TokenKind::Dot, Span::new(38, 39)),
            Token::new(TokenKind::Eof, Span::new(39, 39))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_const());
        let e = module.declaration_sequence[0].as_const().unwrap().1;
        assert!(e.is_set());
        let set = e.as_set().unwrap();
        assert_eq!(set.len(), 2);
        assert_eq!(*set[0].first_expression.deref(), Int { value: 1, span: Span::new(26, 27)});
        assert!(set[0].second_expression.is_none());
        assert_eq!(*set[1].first_expression.deref(), Int { value: 2, span: Span::new(28, 29)});
        assert_eq!(*set[1].second_expression.clone().unwrap().deref(), Int { value: 3, span: Span::new(31, 32)});

    }

    #[test]
    fn parse_three_part_qualified_designator_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Ident("M".to_string()), Span::new(24, 27)),
            Token::new(TokenKind::Dot, Span::new(27, 29)),
            Token::new(TokenKind::Ident("B".to_string()), Span::new(29, 30)),
            Token::new(TokenKind::Dot, Span::new(30, 31)),
            Token::new(TokenKind::Ident("Bar".to_string()), Span::new(31, 34)),
            Token::new(TokenKind::SemiColon, Span::new(34, 35)),
            Token::new(TokenKind::End, Span::new(35, 38)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(39, 45)),
            Token::new(TokenKind::Dot, Span::new(45, 46)),
            Token::new(TokenKind::Eof, Span::new(46, 46))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_const());
        let e = module.declaration_sequence[0].as_const().unwrap().1;
        assert!(e.is_designator());
        let (identifier, selector, parameters) = e.as_designator().unwrap();
        assert_eq!(identifier.first_ident.as_ref().unwrap().identifier, "M".to_string());
        assert_eq!(identifier.second_ident.identifier, "B".to_string());
        assert!(selector.is_qualified());
        assert_eq!(selector.as_qualified().unwrap().identifier, "Bar".to_string());
        assert!(parameters.is_none())
    }

    #[test]
    fn parse_three_part_qualified_designator_with_parameters_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Ident("M".to_string()), Span::new(24, 27)),
            Token::new(TokenKind::Dot, Span::new(27, 29)),
            Token::new(TokenKind::Ident("B".to_string()), Span::new(29, 30)),
            Token::new(TokenKind::Dot, Span::new(30, 31)),
            Token::new(TokenKind::Ident("Bar".to_string()), Span::new(31, 34)),
            Token::new(TokenKind::LParen, Span::new(34, 35)),
            Token::new(TokenKind::Int(1), Span::new(35, 36)),
            Token::new(TokenKind::Comma, Span::new(36, 37)),
            Token::new(TokenKind::Int(2), Span::new(37, 38)),
            Token::new(TokenKind::RParen, Span::new(38, 39)),
            Token::new(TokenKind::SemiColon, Span::new(39, 40)),
            Token::new(TokenKind::End, Span::new(40, 43)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(44, 50)),
            Token::new(TokenKind::Dot, Span::new(50, 51)),
            Token::new(TokenKind::Eof, Span::new(51, 51))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_const());
        let e = module.declaration_sequence[0].as_const().unwrap().1;
        assert!(e.is_designator());
        let (identifier, selector, parameters) = e.as_designator().unwrap();
        assert_eq!(identifier.first_ident.as_ref().unwrap().identifier, "M".to_string());
        assert_eq!(identifier.second_ident.identifier, "B".to_string());
        assert!(selector.is_qualified());
        assert_eq!(selector.as_qualified().unwrap().identifier, "Bar".to_string());
        assert!(parameters.is_some());
        let v = parameters.as_ref().unwrap();
        assert_eq!(v.parameters.len(), 2);
        assert_eq!(v.parameters[0], Int { value: 1, span: Span { start: 35, end: 36 } });
        assert_eq!(v.parameters[1], Int { value: 2, span: Span { start: 37, end: 38 } });
    }

    #[test]
    fn parse_two_part_qualified_designator_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Ident("B".to_string()), Span::new(24, 25)),
            Token::new(TokenKind::Dot, Span::new(25, 26)),
            Token::new(TokenKind::Ident("Bar".to_string()), Span::new(26, 29)),
            Token::new(TokenKind::SemiColon, Span::new(29, 30)),
            Token::new(TokenKind::End, Span::new(30, 33)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(34, 40)),
            Token::new(TokenKind::Dot, Span::new(40, 41)),
            Token::new(TokenKind::Eof, Span::new(41, 41))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_const());
        let e = module.declaration_sequence[0].as_const().unwrap().1;
        assert!(e.is_designator());
        let (identifier, selector, parameters) = e.as_designator().unwrap();
        assert_eq!(identifier.first_ident, None);
        assert_eq!(identifier.second_ident.identifier, "B".to_string());
        assert!(selector.is_qualified());
        assert_eq!(selector.as_qualified().unwrap().identifier, "Bar".to_string());
        assert!(parameters.is_none())
    }

    #[test]
    fn parse_two_part_qualified_designator_with_aparameters_const() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),
            Token::new(TokenKind::Ident("B".to_string()), Span::new(24, 25)),
            Token::new(TokenKind::Dot, Span::new(25, 26)),
            Token::new(TokenKind::Ident("Bar".to_string()), Span::new(26, 29)),
            Token::new(TokenKind::LParen, Span::new(29, 30)),
            Token::new(TokenKind::Int(10), Span::new(30, 31)),
            Token::new(TokenKind::RParen, Span::new(31, 32)),
            Token::new(TokenKind::SemiColon, Span::new(32, 33)),
            Token::new(TokenKind::End, Span::new(33, 36)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(37, 43)),
            Token::new(TokenKind::Dot, Span::new(43, 44)),
            Token::new(TokenKind::Eof, Span::new(44, 44))
        ];

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();
        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_const());
        let e = module.declaration_sequence[0].as_const().unwrap().1;
        assert!(e.is_designator());
        let (identifier, selector, parameters) = e.as_designator().unwrap();
        assert_eq!(identifier.first_ident, None);
        assert_eq!(identifier.second_ident.identifier, "B".to_string());
        assert!(selector.is_qualified());
        assert_eq!(selector.as_qualified().unwrap().identifier, "Bar".to_string());
        assert!(parameters.is_some());
        let v = parameters.as_ref().unwrap();
        assert_eq!(v.parameters.len(), 1);
        assert_eq!(v.parameters[0], Int { value: 10, span: Span { start: 30, end: 31 } });
    }
}