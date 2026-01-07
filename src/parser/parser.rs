use crate::ast::Expression::Designator;
use crate::ast::Selector::Qualify;
use crate::ast::{
    ActualParameters, Declaration, Element, Expression, Identifier, IdentifierDef, Import, Module,
    QualifiedIdentifier, Type,
};
use crate::lexer::TokenKind::{Array, Ident, LParen, RParen};
use crate::lexer::{Token, TokenKind};
use crate::span::Span;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ParserError {
    #[error("Unexpected token: '{token:?}'")]
    UnexpectedToken { token: Token },

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
    // -------------------------
    // Cursor helpers
    // -------------------------

    fn peek_opt(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn peek(&self) -> Result<&Token, ParserError> {
        self.peek_opt().ok_or(ParserError::UnexpectedEof)
    }

    fn peek_kind(&self) -> Result<&TokenKind, ParserError> {
        Ok(&self.peek()?.kind)
    }

    fn at(&self, k: &TokenKind) -> bool {
        self.peek_opt().map(|t| &t.kind == k).unwrap_or(false)
    }

    fn bump(&mut self) -> Result<Token, ParserError> {
        let t = self.peek()?.clone();
        self.pos += 1;
        Ok(t)
    }

    fn eat(&mut self, k: TokenKind) -> Result<Option<Token>, ParserError> {
        if self.at(&k) {
            Ok(Some(self.bump()?))
        } else {
            Ok(None)
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParserError> {
        let tok = self.peek()?.clone();
        if tok.kind == expected {
            self.pos += 1;
            Ok(tok)
        } else {
            Err(ParserError::UnexpectedToken { token: tok })
        }
    }

    fn expect_ident(&mut self) -> Result<Identifier, ParserError> {
        let tok = self.peek()?.clone();
        match &tok.kind {
            TokenKind::Ident(name) => {
                self.pos += 1;
                Ok(Identifier {
                    identifier: name.clone(),
                    span: tok.span,
                })
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    fn expect_number(&mut self) -> Result<Expression, ParserError> {
        let tok = self.peek()?.clone();
        match &tok.kind {
            TokenKind::Int(value) => {
                self.pos += 1;
                Ok(Expression::Int {
                    value: *value,
                    span: tok.span,
                })
            }
            TokenKind::Real(value) => {
                self.pos += 1;
                Ok(Expression::Real {
                    value: *value,
                    span: tok.span,
                })
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    fn expect_string(&mut self) -> Result<Expression, ParserError> {
        let tok = self.peek()?.clone();
        match &tok.kind {
            TokenKind::String(value) => {
                self.pos += 1;
                Ok(Expression::String {
                    value: value.to_string(),
                    span: tok.span,
                })
            }
            TokenKind::Char(value) => {
                self.pos += 1;
                Ok(Expression::String {
                    value: char::from(*value).to_string(),
                    span: tok.span,
                })
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    fn expect_boolean(&mut self) -> Result<Expression, ParserError> {
        let tok = self.peek()?.clone();
        match tok.kind {
            TokenKind::False => {
                self.pos += 1;
                Ok(Expression::Bool {
                    value: false,
                    span: tok.span,
                })
            }
            TokenKind::True => {
                self.pos += 1;
                Ok(Expression::Bool {
                    value: true,
                    span: tok.span,
                })
            }
            _ => Err(ParserError::UnexpectedToken { token: tok }),
        }
    }

    // -------------------------
    // Small parsing utilities
    // -------------------------

    fn comma_list_until<T>(
        &mut self,
        until: TokenKind,
        mut item: impl FnMut(&mut Self) -> Result<T, ParserError>,
    ) -> Result<Vec<T>, ParserError> {
        let mut items = vec![];
        while !self.at(&until) {
            if !items.is_empty() {
                self.expect(TokenKind::Comma)?;
            }
            items.push(item(self)?);
        }
        Ok(items)
    }

    fn maybe_star(&mut self) -> Result<Option<Token>, ParserError> {
        self.eat(TokenKind::Star)
    }

    fn create_identifier_def(name: Identifier, star_tok: Option<Token>) -> IdentifierDef {
        let span = match &star_tok {
            Some(t) => Span::new(name.span.start, t.span.end),
            None => name.span,
        };

        IdentifierDef {
            identifier: name,
            star: star_tok.is_some(),
            span,
        }
    }

    fn parse_named_decls<T>(
        &mut self,
        header: TokenKind,
        mut parse_rhs: impl FnMut(&mut Self) -> Result<T, ParserError>,
        mut build: impl FnMut(IdentifierDef, T, Span) -> Declaration,
    ) -> Result<Vec<Declaration>, ParserError> {
        if !self.at(&header) {
            return Ok(vec![]);
        }

        self.bump()?; // header

        let mut out = vec![];
        while matches!(self.peek_kind()?, TokenKind::Ident(_)) {
            let name = self.expect_ident()?;
            let star_tok = self.maybe_star()?;
            self.expect(TokenKind::Equal)?;

            let rhs = parse_rhs(self)?;
            let semi = self.expect(TokenKind::SemiColon)?;

            let decl_span = Span::new(name.span.start, semi.span.end);
            let ident = Self::create_identifier_def(name, star_tok);
            out.push(build(ident, rhs, decl_span));
        }
        Ok(out)
    }

    // -------------------------
    // Top-level
    // -------------------------

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        let begin = self.expect(TokenKind::Module)?;
        let first_ident = self.expect_ident()?;
        self.expect(TokenKind::SemiColon)?;

        let import_list = self.parse_import_list()?;
        let declaration_sequence = self.parse_declaration_sequence()?;

        self.expect(TokenKind::End)?;
        let second_ident = self.expect_ident()?;
        let end = self.expect(TokenKind::Dot)?;

        Ok(Module {
            first_ident,
            second_ident,
            import_list,
            declaration_sequence,
            stmts: vec![],
            span: Span::new(begin.span.start, end.span.end),
        })
    }

    fn parse_declaration_sequence(&mut self) -> Result<Vec<Declaration>, ParserError> {
        let mut declarations = vec![];
        declarations.extend(self.parse_const_declarations()?);
        declarations.extend(self.parse_type_declarations()?);
        Ok(declarations)
    }

    fn parse_import_list(&mut self) -> Result<Vec<Import>, ParserError> {
        if !self.at(&TokenKind::Import) {
            return Ok(vec![]);
        }

        self.bump()?; // IMPORT

        let mut imports = vec![];
        while !self.at(&TokenKind::SemiColon) {
            if !imports.is_empty() {
                self.expect(TokenKind::Comma)?;
            }

            let first_ident = self.expect_ident()?;
            let span_begin = first_ident.span.start;

            let second_ident = if self.at(&TokenKind::Assign) {
                self.bump()?; // :=
                Some(self.expect_ident()?)
            } else {
                None
            };

            let span_end = second_ident
                .as_ref()
                .map(|i| i.span.end)
                .unwrap_or(first_ident.span.end);

            imports.push(Import {
                first_ident,
                second_ident,
                span: Span::new(span_begin, span_end),
            });
        }

        self.expect(TokenKind::SemiColon)?;
        Ok(imports)
    }

    fn parse_const_declarations(&mut self) -> Result<Vec<Declaration>, ParserError> {
        self.parse_named_decls(
            TokenKind::Const,
            |p| p.parse_expression(),
            |ident, expression, span| Declaration::Const {
                ident,
                expression,
                span,
            },
        )
    }

    fn parse_type_declarations(&mut self) -> Result<Vec<Declaration>, ParserError> {
        self.parse_named_decls(
            TokenKind::Type,
            |p| p.parse_type(),
            |ident, ty, span| Declaration::Type { ident, ty, span },
        )
    }

    // -------------------------
    // Types
    // -------------------------

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let peek = self.peek()?.clone();

        match peek.kind {
            Ident(_) => {
                // Named type: Ident | Ident '.' Ident
                let first_or_second = self.expect_ident()?;
                if self.at(&TokenKind::Dot) {
                    self.bump()?; // .
                    let second_ident = self.expect_ident()?;
                    let span = Span::new(first_or_second.span.start, second_ident.span.end);

                    Ok(Type::Named {
                        name: QualifiedIdentifier {
                            first_ident: Some(first_or_second),
                            second_ident,
                            span,
                        },
                        span,
                    })
                } else {
                    let span = first_or_second.span;
                    Ok(Type::Named {
                        name: QualifiedIdentifier {
                            first_ident: None,
                            second_ident: first_or_second,
                            span,
                        },
                        span,
                    })
                }
            }

            Array => {
                let start = self.expect(TokenKind::Array)?.span.start;

                // ARRAY len {, len} OF type
                let mut lengths = vec![self.parse_expression()?];
                while self.at(&TokenKind::Comma) {
                    self.bump()?;
                    lengths.push(self.parse_expression()?);
                }

                self.expect(TokenKind::Of)?;
                let element = self.parse_type()?;
                let end = element.span().end;

                Ok(Type::Array {
                    lengths,
                    element: Box::new(element),
                    span: Span::new(start, end),
                })
            }

            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    // -------------------------
    // Expressions
    // -------------------------

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
        let peek = self.peek()?.clone();

        match peek.kind {
            TokenKind::Int(_) | TokenKind::Real(_) => self.expect_number(),
            TokenKind::String(_) | TokenKind::Char(_) => self.expect_string(),
            TokenKind::False | TokenKind::True => self.expect_boolean(),

            TokenKind::Nil => {
                self.bump()?;
                Ok(Expression::Nil { span: peek.span })
            }

            TokenKind::LCurly => self.parse_set_literal(),

            TokenKind::Ident(_) => {
                let first = self.expect_ident()?;
                self.parse_designator_or_error(first)
            }

            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    fn parse_set_literal(&mut self) -> Result<Expression, ParserError> {
        let lcurly = self.expect(TokenKind::LCurly)?;

        let elements = self.comma_list_until(TokenKind::RCurly, |p| p.parse_element())?;
        let rcurly = self.expect(TokenKind::RCurly)?;

        Ok(Expression::Set {
            value: elements.into_iter().map(Into::into).collect(),
            span: Span::new(lcurly.span.start, rcurly.span.end),
        })
    }

    /// Din gamle kode håndterede kun:
    ///   Ident '.' Ident [ '.' Ident ] [ActualParameters]
    /// og havde todo!() for "bare Ident".
    ///
    /// Her returnerer vi en pæn fejl, hvis det ikke er et designator-mønster.
    fn parse_designator_or_error(&mut self, first: Identifier) -> Result<Expression, ParserError> {
        if !self.at(&TokenKind::Dot) {
            // samme semantik som din todo!(), bare uden panic:
            let tok = self.peek()?.clone();
            return Err(ParserError::UnexpectedToken { token: tok });
        }

        self.bump()?; // '.'
        let second = self.expect_ident()?;

        // 3-part: M.B.Bar(...)
        if self.eat(TokenKind::Dot)?.is_some() {
            let third = self.expect_ident()?;
            let params = self.parse_actual_parameters()?;

            let target_span = Span::new(first.span.start, second.span.end);
            let end = params
                .as_ref()
                .map(|p| p.span.end)
                .unwrap_or(third.span.end);

            Ok(Designator {
                target: QualifiedIdentifier {
                    first_ident: Some(first),
                    second_ident: second,
                    span: target_span,
                },
                selector: Qualify {
                    name: third.clone(),
                    span: third.span,
                },
                parameters: params,
                span: Span::new(target_span.start, end),
            })
        } else {
            // 2-part: B.Bar(...)
            let params = self.parse_actual_parameters()?;
            let end = params
                .as_ref()
                .map(|p| p.span.end)
                .unwrap_or(second.span.end);

            Ok(Designator {
                target: QualifiedIdentifier {
                    first_ident: None,
                    second_ident: first.clone(),
                    span: first.span,
                },
                selector: Qualify {
                    name: second.clone(),
                    span: second.span,
                },
                parameters: params,
                span: Span::new(first.span.start, end),
            })
        }
    }

    fn parse_element(&mut self) -> Result<Element, ParserError> {
        let first = self.parse_expression()?;
        let second = if self.at(&TokenKind::DotDot) {
            self.bump()?;
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = second.as_ref().map(|e| e.span().end).unwrap_or(first.span().end);
        let span = Span::new(first.span().start, end);

        Ok(Element {
            first_expression: first.into(),
            second_expression: second.map(Into::into),
            span,
        })
    }

    fn parse_actual_parameters(&mut self) -> Result<Option<ActualParameters>, ParserError> {
        if !self.at(&TokenKind::LParen) {
            return Ok(None);
        }

        let lpar = self.bump()?;
        let params = self.comma_list_until(TokenKind::RParen, |p| p.parse_expression())?;
        let rpar = self.expect(TokenKind::RParen)?;

        Ok(Some(ActualParameters {
            parameters: params,
            span: Span::new(lpar.span.start, rpar.span.end),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::{Declaration, Expression, Identifier, IdentifierDef, QualifiedIdentifier, Type};
    use crate::lexer::{Token, TokenKind};
    use crate::span::Span;
    use std::ops::Deref;
    use Expression::Int;

    // -------------------------
    // Token helpers
    // -------------------------

    fn t(kind: TokenKind, start: usize, end: usize) -> Token {
        Token::new(kind, Span::new(start, end))
    }

    fn ident(name: &str, start: usize, end: usize) -> Token {
        t(TokenKind::Ident(name.to_string()), start, end)
    }

    fn int(v: i64, start: usize, end: usize) -> Token {
        t(TokenKind::Int(v), start, end)
    }

    macro_rules! tok {
    (Module $a:expr, $b:expr) => { t(TokenKind::Module, $a, $b) };
    (Import $a:expr, $b:expr) => { t(TokenKind::Import, $a, $b) };
    (Const  $a:expr, $b:expr) => { t(TokenKind::Const,  $a, $b) };
    (Type   $a:expr, $b:expr) => { t(TokenKind::Type,   $a, $b) };
    (Array  $a:expr, $b:expr) => { t(TokenKind::Array,  $a, $b) };
    (Of     $a:expr, $b:expr) => { t(TokenKind::Of,     $a, $b) };

    (Ident $s:literal, $a:expr, $b:expr) => { ident($s, $a, $b) };
    (Int   $v:expr,    $a:expr, $b:expr) => { int($v, $a, $b) };

    (Semi   $a:expr, $b:expr) => { t(TokenKind::SemiColon, $a, $b) };
    (Comma  $a:expr, $b:expr) => { t(TokenKind::Comma,    $a, $b) };
    (Assign $a:expr, $b:expr) => { t(TokenKind::Assign,   $a, $b) };
    (Equal  $a:expr, $b:expr) => { t(TokenKind::Equal,    $a, $b) };
    (Star   $a:expr, $b:expr) => { t(TokenKind::Star,     $a, $b) };
    (Dot    $a:expr, $b:expr) => { t(TokenKind::Dot,      $a, $b) };
    (DotDot $a:expr, $b:expr) => { t(TokenKind::DotDot,   $a, $b) };

    (LParen $a:expr, $b:expr) => { t(TokenKind::LParen,   $a, $b) };
    (RParen $a:expr, $b:expr) => { t(TokenKind::RParen,   $a, $b) };

    (LCurly $a:expr, $b:expr) => { t(TokenKind::LCurly,   $a, $b) };
    (RCurly $a:expr, $b:expr) => { t(TokenKind::RCurly,   $a, $b) };

    (End $a:expr, $b:expr) => { t(TokenKind::End, $a, $b) };
    (Eof $a:expr, $b:expr) => { t(TokenKind::Eof, $a, $b) };
    }

    // -------------------------
    // Module wrapper + parse helper
    // -------------------------

    /// Wraps "body" tokens in:
    /// MODULE <module_name> ; <body> END <end_name> . EOF
    fn module_tokens(module_name: &str, end_name: &str, mut body: Vec<Token>) -> Vec<Token> {
        // Keep the same header spans you used a lot (0..6, 7..13, 13..14)
        let mut tokens = vec![
            tok!(Module 0, 6),
            tok!(Ident "DUMMY", 0, 0), // placeholder, overwritten below
            tok!(Semi 13, 14),
        ];

        tokens[1] = ident(module_name, 7, 13);

        tokens.append(&mut body);

        // Default tail spans (usually irrelevant for these tests)
        tokens.extend([
            tok!(End 1000, 1003),
            ident(end_name, 1004, 1010),
            tok!(Dot 1010, 1011),
            tok!(Eof 1011, 1011),
        ]);

        tokens
    }

    fn parse_module(tokens: Vec<Token>) -> crate::ast::Module {
        let mut parser = Parser { tokens: &tokens, pos: 0 };
        parser.parse().unwrap()
    }

    // -------------------------
    // Tests
    // -------------------------

    #[test]
    fn parse_empty_module() {
        let tokens = module_tokens("monkey", "monkey2", vec![]);
        let module = parse_module(tokens);

        assert_eq!(module.first_ident.identifier, "monkey");
        assert_eq!(module.second_ident.identifier, "monkey2");
        assert_eq!(module.import_list.len(), 0);
        assert_eq!(module.stmts.len(), 0);
        assert_eq!(module.declaration_sequence.len(), 0);
    }

    #[test]
    fn parse_import_list() {
        let body = vec![
            tok!(Import 14, 20),
            tok!(Ident "foo", 21, 24),
            tok!(Comma 24, 25),
            tok!(Ident "bar", 26, 29),
            tok!(Assign 29, 30),
            tok!(Ident "baz", 31, 34),
            tok!(Semi 34, 35),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

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
        let body = vec![
            tok!(Const 14, 19),
            // empty const section
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 0);
    }

    #[test]
    fn parse_single_const() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Int 123, 24, 27),
            tok!(Semi 27, 28),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(matches!(
            &module.declaration_sequence[0],
            Declaration::Const { ident: IdentifierDef { identifier: Identifier { identifier, .. }, .. }, .. }
                if identifier == "Foo"
        ));
    }

    #[test]
    fn parse_multiple_const() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Int 123, 24, 27),
            tok!(Semi 27, 28),
            tok!(Ident "Bar", 29, 32),
            tok!(Star 32, 33),
            tok!(Equal 33, 34),
            tok!(Int 456, 34, 37),
            tok!(Semi 37, 38),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 2);

        assert!(matches!(
            &module.declaration_sequence[0],
            Declaration::Const { ident: IdentifierDef { identifier: Identifier { identifier, .. }, star, .. }, .. }
                if identifier == "Foo" && !*star
        ));

        assert!(matches!(
            &module.declaration_sequence[1],
            Declaration::Const { ident: IdentifierDef { identifier: Identifier { identifier, .. }, star, .. }, .. }
                if identifier == "Bar" && *star
        ));
    }

    #[test]
    fn parse_empty_type() {
        let body = vec![
            tok!(Type 14, 19),
            // empty type section
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 0);
    }

    #[test]
    fn parse_named_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Ident "M", 24, 27),
            tok!(Dot 27, 29),
            tok!(Ident "B", 29, 30),
            tok!(Semi 30, 31),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(matches!(
            &module.declaration_sequence[0],
            Declaration::Type {
                ident: IdentifierDef { .. },
                ty: Type::Named {
                    name: QualifiedIdentifier { first_ident: Some(Identifier { identifier, .. }), second_ident, .. },
                    ..
                },
                ..
            } if identifier == "M" && second_ident.identifier == "B"
        ));
    }

    #[test]
    fn parse_array_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Array 24, 29),
            tok!(Int 2, 29, 31),
            tok!(Comma 31, 32),
            tok!(Int 3, 32, 33),
            tok!(Of 33, 35),
            tok!(Ident "T", 36, 39),
            tok!(Semi 39, 40),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_type());

        let (n, tpe) = module.declaration_sequence[0].as_type().unwrap();
        assert_eq!(n.identifier.identifier, "Foo");
        assert_eq!(n.star, false);

        assert!(tpe.is_array());
        let (lengths, base) = tpe.as_array().unwrap();

        assert_eq!(
            lengths,
            vec![
                Int { value: 2, span: Span::new(29, 31) },
                Int { value: 3, span: Span::new(32, 33) }
            ]
        );

        assert!(base.is_named());
        let named = base.as_named().unwrap();
        assert_eq!(named.first_ident, None);
        assert_eq!(named.second_ident.identifier, "T");
    }

    #[test]
    fn parse_set_const() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(LCurly 24, 26),
            tok!(Int 1, 26, 27),
            tok!(Comma 27, 28),
            tok!(Int 2, 28, 29),
            tok!(DotDot 29, 31),
            tok!(Int 3, 31, 32),
            tok!(RCurly 32, 33),
            tok!(Semi 27, 28),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declaration_sequence.len(), 1);
        assert!(module.declaration_sequence[0].is_const());

        let e = module.declaration_sequence[0].as_const().unwrap().1;
        assert!(e.is_set());

        let set = e.as_set().unwrap();
        assert_eq!(set.len(), 2);

        assert_eq!(
            *set[0].first_expression.deref(),
            Int { value: 1, span: Span::new(26, 27) }
        );
        assert!(set[0].second_expression.is_none());

        assert_eq!(
            *set[1].first_expression.deref(),
            Int { value: 2, span: Span::new(28, 29) }
        );
        assert_eq!(
            *set[1].second_expression.clone().unwrap().deref(),
            Int { value: 3, span: Span::new(31, 32) }
        );
    }

    #[test]
    fn parse_three_part_qualified_designator_const() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Ident "M", 24, 27),
            tok!(Dot 27, 29),
            tok!(Ident "B", 29, 30),
            tok!(Dot 30, 31),
            tok!(Ident "Bar", 31, 34),
            tok!(Semi 34, 35),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

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
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Ident "M", 24, 27),
            tok!(Dot 27, 29),
            tok!(Ident "B", 29, 30),
            tok!(Dot 30, 31),
            tok!(Ident "Bar", 31, 34),
            tok!(LParen 34, 35),
            tok!(Int 1, 35, 36),
            tok!(Comma 36, 37),
            tok!(Int 2, 37, 38),
            tok!(RParen 38, 39),
            tok!(Semi 39, 40),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

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
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Ident "B", 24, 25),
            tok!(Dot 25, 26),
            tok!(Ident "Bar", 26, 29),
            tok!(Semi 29, 30),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

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
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Ident "B", 24, 25),
            tok!(Dot 25, 26),
            tok!(Ident "Bar", 26, 29),
            tok!(LParen 29, 30),
            tok!(Int 10, 30, 31),
            tok!(RParen 31, 32),
            tok!(Semi 32, 33),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

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