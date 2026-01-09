use crate::ast::{BinaryOperation, Case, Designator, ElsIf, FPSection, FieldList, FormalParameters, FormalType, Label, LabelValue, Spanned, Statement, UnaryOperation};
use crate::ast::{
    Declaration, Element, Expression, Identifier, IdentifierDef, Import, Module,
    QualifiedIdentifier, Type,
};
use crate::lexer::{Token, TokenKind};
use crate::span::Span;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ParserError {
    #[error("Unexpected token: '{token:?}'")]
    UnexpectedToken { token: Token },

    #[error("Unexpected end of file")]
    UnexpectedEof,
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
                    text: name.clone(),
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

    fn list_until<T>(
        &mut self,
        until: Vec<TokenKind>,
        separator: TokenKind,
        mut item: impl FnMut(&mut Self) -> Result<T, ParserError>,
    ) -> Result<Vec<T>, ParserError> {
        let mut items = vec![];
        while !until.contains(&self.peek()?.kind) {
            if !items.is_empty() {
                self.expect(separator.clone())?;
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
            ident: name,
            span,
            exported: star_tok.is_some(),
        }
    }

    fn parse_qualified_identifier(&mut self) -> Result<QualifiedIdentifier, ParserError> {
        let first = self.expect_ident()?;

        if self.at(&TokenKind::Dot) {
            self.bump()?; // '.'
            let second = self.expect_ident()?;
            Ok(QualifiedIdentifier::new(vec![first, second]))
        } else {
            Ok(QualifiedIdentifier::new(vec![first]))
        }
    }

    fn parse_identifier_def(&mut self) -> Result<IdentifierDef, ParserError> {
        let name = self.expect_ident()?;
        let star_tok = self.maybe_star()?;
        Ok(Self::create_identifier_def(name, star_tok))
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

        let stmts = if self.at(&TokenKind::Begin) {
            self.bump()?;
            self.parse_statements()?
        } else {
            vec![]
        };

        self.expect(TokenKind::End)?;
        let second_ident = self.expect_ident()?;
        let end = self.expect(TokenKind::Dot)?;

        Ok(Module {
            name: first_ident,
            import_list,
            stmts,
            span: Span::new(begin.span.start, end.span.end),
            end_name: second_ident,
            declarations: declaration_sequence,
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

            // Første ident: enten module (IMPORT M;) eller alias (IMPORT A := M;)
            let first = self.expect_ident()?;

            let imp = if self.at(&TokenKind::Assign) {
                self.bump()?; // :=
                let module = self.expect_ident()?; // det rigtige modulnavn (Bar)

                let span = Span::new(first.span.start, module.span.end);

                Import {
                    module,
                    alias: Some(first),
                    span,
                }
            } else {
                let span = first.span;

                Import {
                    module: first,
                    alias: None,
                    span,
                }
            };

            imports.push(imp);
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
                value: expression,
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
            TokenKind::Ident(_) => {
                // Named type: Ident | Ident '.' Ident

                Ok(self.parse_named_type()?)
            }

            TokenKind::Array => {
                let start = self.expect(TokenKind::Array)?.span.start;

                // ARRAY len {, len} OF type
                let lengths =
                    self.list_until(vec![TokenKind::Of], TokenKind::Comma, |p|p.parse_expression())?;
                self.expect(TokenKind::Of)?;
                let element = self.parse_type()?;
                let end = element.span().end;

                Ok(Type::Array {
                    lengths,
                    element: Box::new(element),
                    span: Span::new(start, end),
                })
            }

            TokenKind::Record => {
                let start = self.expect(TokenKind::Record)?.span.start;
                let base =
                    if self.at(&TokenKind::LParen) {
                        self.bump()?;
                        let base = self.parse_qualified_identifier()?;
                        self.expect(TokenKind::RParen)?;
                        Some( base)
                    } else {
                        None
                    };
                let field_lists = self.list_until(vec![TokenKind::End], TokenKind::SemiColon,
                                                      |p| p.parse_field_list())?;
                let end = self.expect(TokenKind::End)?.span.end;

                Ok(Type::Record {
                    base,
                    field_lists,
                    span: Span { start, end },
                })
            }

            TokenKind::Pointer => {
                let start = self.expect(TokenKind::Pointer)?.span.start;
                self.expect(TokenKind::To)?;
                let pointee = self.parse_type()?;
                let end = pointee.span().end;
                Ok(Type::Pointer { pointee: Box::new(pointee), span: Span::new(start, end) })
            }

            TokenKind::Procedure => {
                let initial_span = self.expect(TokenKind::Procedure)?.span;
                let params = self.parse_formal_parameters()?;
                if params.is_some() {
                    let end = params.clone().unwrap().span().end;
                    Ok(Type::Procedure { params, span: Span::new(initial_span.start, end) })
                } else {
                    Ok(Type::Procedure { params: None, span: initial_span })
                }
            }

            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    fn parse_field_list(&mut self) -> Result<FieldList, ParserError> {
        let fields = self.list_until(vec![TokenKind::Colon], TokenKind::Comma,
                                         |p| p.parse_identifier_def())?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok(FieldList{
            fields,
            ty
        })
    }

    fn parse_named_type(&mut self) -> Result<Type, ParserError> {
        let first = self.expect_ident()?;

        let name = if self.at(&TokenKind::Dot) {
            self.bump()?; // '.'
            let second = self.expect_ident()?;
            QualifiedIdentifier::new(vec![first, second])
        } else {
            QualifiedIdentifier::new(vec![first])
        };

        let span = name.span();

        Ok(Type::Named { name, span })
    }

    fn parse_formal_parameters(&mut self) -> Result<Option<FormalParameters>, ParserError> {
        if self.at(&TokenKind::LParen) {
            let start = self.expect(TokenKind::LParen)?.span.start;
            let sections = if !self.at(&TokenKind::RParen) {
                self.list_until(vec![TokenKind::RParen], TokenKind::SemiColon, |p| p.parse_fp_section())?
            } else { vec![]};
            let end = self.expect(TokenKind::RParen)?.span.end;

            if self.at(&TokenKind::Colon) {
               self.bump()?;
                let return_type = self.parse_qualified_identifier()?;
                let end = return_type.span().end;
                Ok(Some(FormalParameters {
                    sections,
                    return_type: Some(return_type),
                    span: Span { start, end},
                }))
            }
            else {
                Ok(Some(FormalParameters {
                    sections,
                    return_type: None,
                    span: Span { start, end},
                }))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_fp_section(&mut self) -> Result<FPSection, ParserError> {
        let start = self.peek()?.span.start;
        let by_ref = self.at(&TokenKind::Var);
        if by_ref { self.bump()?; }
        let names = self.list_until(vec![TokenKind::Colon], TokenKind::Comma,
                                    |p| p.expect_ident())?;
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_formal_type()?;
        let end = ty.span().end;
        Ok(FPSection{
            by_ref,
            names,
            ty,
            span: Span { start, end},
        })
    }

    fn parse_formal_type(&mut self) -> Result<FormalType, ParserError> {
        let mut open_arrays = 0;
        let start = self.peek()?.span.start;
        let base = loop {
            match self.peek()?.kind {
                TokenKind::Array => {
                    self.bump()?;
                    self.expect(TokenKind::Of)?;
                    open_arrays += 1;
                }
                _ => break self.parse_qualified_identifier()?,
            }
        };
        let end = base.span().end;
        Ok(FormalType{
            open_arrays,
            base,
            span: Span { start, end },
        })
    }

    // -------------------------
    // Statements
    // -------------------------
    fn parse_statements(&mut self) -> Result<Vec<Statement>, ParserError> {
        let result = self.list_until(
            vec![TokenKind::End, TokenKind::Else, TokenKind::Elsif, TokenKind::Pipe, TokenKind::Until],
            TokenKind::SemiColon, |p| p.parse_statement())?;
        Ok(result)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let peek = self.peek()?.clone();

        match peek.kind {
            TokenKind::Ident(_) => {
                let first = self.expect_ident()?;
                let start = first.span.start;
                let designator = self.parse_designator_or_error(first)?;
                let designator_end = designator.span().end;
                if self.at(&TokenKind::Assign) {
                    self.bump()?;
                    let value = self.parse_expression()?;
                    let value_end = value.span().end;
                    Ok(Statement::Assign {target: designator, value, span: Span {start, end: value_end}})
                } else {
                    Ok(Statement::Call { callee: designator, span: Span {start, end: designator_end} })
                }

            }

            TokenKind::If => {
                let start = self.expect(TokenKind::If)?.span.start;
                let cond = self.parse_expression()?;
                self.expect(TokenKind::Then)?;
                let then_branch = self.parse_statements()?;
                let elsif_branches = self.parse_elsif_statements(TokenKind::Then)?;
                let else_branch = if self.at(&TokenKind::Else) {
                    self.bump()?;
                    Some(self.parse_statements()?)
                } else {
                    None
                };

                let end = self.expect(TokenKind::End)?.span.end;
                Ok(Statement::If {
                    cond,
                    then_branch,
                    elsif_branches,
                    else_branch,
                    span: Span { start, end },
                })
            }

            TokenKind::Case => {
                let start = self.expect(TokenKind::Case)?.span.start;
                let expr = self.parse_expression()?;
                self.expect(TokenKind::Of)?;
                let branches = self.list_until(vec![TokenKind::End], TokenKind::Pipe, |p| p.parse_case())?;
                let end = self.expect(TokenKind::End)?.span.end;
                Ok(Statement::Case { expr, branches, span: Span { start, end } })
            }

            TokenKind::While => {
                let start = self.expect(TokenKind::While)?.span.start;
                let cond = self.parse_expression()?;
                self.expect(TokenKind::Do)?;
                let body = self.parse_statements()?;
                let elsif_branches = self.parse_elsif_statements(TokenKind::Do)?;
                let end = self.expect(TokenKind::End)?.span.end;
                Ok(Statement::While {
                    cond,
                    body,
                    elsif_branches,
                    span: Span { start, end},
                })
            }

            TokenKind::Repeat => {
                let start = self.expect(TokenKind::Repeat)?.span.start;
                let body = self.parse_statements()?;
                self.expect(TokenKind::Until)?;
                let cond = self.parse_expression()?;
                let end = self.peek()?.span.end;
                Ok(Statement::Repeat { body, cond, span: Span { start, end } })
            }

            TokenKind::For => {
                let start = self.expect(TokenKind::For)?.span.start;
                let var = self.expect_ident()?;
                self.expect(TokenKind::Assign)?;
                let low = self.parse_expression()?;
                self.expect(TokenKind::To)?;
                let high = self.parse_expression()?;
                let by = if self.at(&TokenKind::By) {
                    self.bump()?;
                    Some(self.parse_expression()?)
                } else { None };
                self.expect(TokenKind::Do)?;
                let body = self.parse_statements()?;
                let end = self.expect(TokenKind::End)?.span.end;
                Ok(Statement::For { var, low, high, by, body, span: Span { start, end } })
            }
            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    fn parse_elsif_statements(&mut self, consequence_token: TokenKind) -> Result<Vec<ElsIf>, ParserError> {
        let mut result = vec![];
        let mut peek = self.peek()?.clone();
        let mut start = peek.span.start;
        while peek.kind == TokenKind::Elsif {
            self.bump()?;
            let cond = self.parse_expression()?;
            self.expect(consequence_token.clone())?;
            let alternate_branch = self.parse_statements()?;
            peek = self.peek()?.clone();
            let end = peek.span.start - 1;
            result.push(ElsIf {
                cond,
                alternate_branch,
                span: Span { start, end},
            });
            start = end + 1;
        }
        Ok(result)
    }

    fn parse_case(&mut self) -> Result<Case, ParserError> {
        let start = self.peek()?.span.start;
        let label_list = self.list_until(vec![TokenKind::Colon], TokenKind::Comma,
                                         |p|p.parse_label())?;
        self.expect(TokenKind::Colon)?;
        let statements = self.parse_statements()?;
        let end = self.peek()?.span.end;
        Ok(Case{
            label_list,
            statements,
            span: Span { start, end },
        })
    }

    fn parse_label(&mut self) -> Result<Label, ParserError> {
        let part_1 = self.parse_label_value()?;
        if self.at(&TokenKind::DotDot) {
            self.bump()?;
            let part_2 = self.parse_label_value()?;
            Ok(Label::Range { low: part_1, high: part_2 })
        } else {
            Ok(Label::Single { value: part_1 })
        }
    }

    fn parse_label_value(&mut self) -> Result<LabelValue, ParserError> {
        let peek = self.peek()?.clone();
        match peek.kind {
            TokenKind::Int(value) => {
                let span = self.bump()?.span;
                Ok(LabelValue::Integer { value, span })
            }
            TokenKind::String(value) => {
                let span = self.bump()?.span;
                Ok(LabelValue::String { value, span })
            }
            TokenKind::Ident(_) => {
                let qualified = self.parse_qualified_identifier()?;
                Ok(LabelValue::QualifiedIdentifier(qualified))
            }
            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    // -------------------------
    // Expressions
    // -------------------------

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let lhs = self.parse_simple_expression()?;
        let op = match self.peek()?.kind {
            TokenKind::Equal => Some(BinaryOperation::Eq),
            TokenKind::NotEqual => Some(BinaryOperation::Neq),
            TokenKind::LessThan => Some(BinaryOperation::Lt),
            TokenKind::LessThanOrEqual => Some(BinaryOperation::Le),
            TokenKind::GreaterThan => Some(BinaryOperation::Gt),
            TokenKind::GreaterThanOrEqual => Some(BinaryOperation::Ge),
            TokenKind::In => Some(BinaryOperation::In),
            TokenKind::Is => Some(BinaryOperation::Is),
            _ => None,
        };

        if let Some(op) = op {
            self.bump()?; // consume relation token
            let rhs = self.parse_simple_expression()?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            Ok(Expression::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span
            })
        } else {
            Ok(lhs)
        }
    }

    fn parse_simple_expression(&mut self) -> Result<Expression, ParserError> {
        // optional prefix +/-
        let prefix_tok = self.peek()?.clone();
        let prefix = match prefix_tok.kind {
            TokenKind::Plus => { self.bump()?; Some(UnaryOperation::Plus) }
            TokenKind::Minus => { self.bump()?; Some(UnaryOperation::Minus) }
            _ => None,
        };

        let mut expr = self.parse_term()?;

        // { AddOperator term }
        loop {
            let op = match self.peek()?.kind {
                TokenKind::Plus => Some(BinaryOperation::Addition),
                TokenKind::Minus => Some(BinaryOperation::Subtraction),
                TokenKind::Or => Some(BinaryOperation::Or),
                _ => None,
            };

            let Some(op) = op else { break; };

            self.bump()?; // consume operator
            let rhs = self.parse_term()?;
            let span = Span::new(expr.span().start, rhs.span().end);

            expr = Expression::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span,
            };
        }

        // apply prefix last (binds tighter than addops, looser than factor-level ~ if you want Oberon semantics)
        if let Some(op) = prefix {
            let span = Span::new(prefix_tok.span.start, expr.span().end);
            Ok(Expression::Unary {
                op,
                operand: Box::new(expr.clone()),
                span,
            })
        } else {
            Ok(expr)
        }
    }


    fn parse_term(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_factor()?;

        loop {
            let op = match self.peek()?.kind {
                TokenKind::Star => Some(BinaryOperation::Multiplication),
                TokenKind::Slash => Some(BinaryOperation::Division),
                TokenKind::Div => Some(BinaryOperation::Div),
                TokenKind::Mod => Some(BinaryOperation::Mod),
                TokenKind::Ampersand => Some(BinaryOperation::And),
                _ => None,
            };

            let Some(op) = op else { break; };

            self.bump()?; // consume mulop
            let rhs = self.parse_factor()?;
            let span = Span::new(expr.span().start, rhs.span().end);

            expr = Expression::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span,
            };
        }

        Ok(expr)
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
                Ok(Expression::Designator(self.parse_designator_or_error(first)?))
            }

            TokenKind::LParen => {
                self.bump()?;
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }

            TokenKind::Tilde => {
                let tilde = self.bump()?;
                let operand = self.parse_factor()?;
                Ok(Expression::Unary {
                    op: UnaryOperation::Not,
                    operand: Box::new(operand.clone()),
                    span: Span { start: tilde.span.start, end: operand.span().end },
                })
            }
            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    fn parse_set_literal(&mut self) -> Result<Expression, ParserError> {
        let lcurly = self.expect(TokenKind::LCurly)?;

        let elements = self.list_until(vec![TokenKind::RCurly], TokenKind::Comma, |p| p.parse_element())?;
        let rcurly = self.expect(TokenKind::RCurly)?;

        Ok(Expression::Set {
            elements,
            span: Span::new(lcurly.span.start, rcurly.span.end),
        })
    }

    fn parse_designator_or_error(&mut self, first: Identifier) -> Result<Designator, ParserError> {
        // parse head: either [first] or [first, second] if "first.second"
        let mut parts = vec![first];

        if self.at(&TokenKind::Dot) {
            self.bump()?; // '.'
            parts.push(self.expect_ident()?);
        }

        let head = crate::ast::QualifiedIdentifier::new(parts);

        // parse selectors: (.field | [index] | ^ | (call))* in any order
        let mut selectors: Vec<crate::ast::Selector> = vec![];

        loop {
            if self.at(&TokenKind::Dot) {
                self.bump()?; // '.'
                let field = self.expect_ident()?;
                selectors.push(crate::ast::Selector::Field(field));
                continue;
            }

            if let Some(index_sel) = self.parse_index_selector()? {
                selectors.push(index_sel);
                continue;
            }

            if let Some(sel) = self.parse_deref_selector()? {
                selectors.push(sel);
                continue;
            }

            if let Some(sel) = self.parse_type_guard_selector()? {
                selectors.push(sel);
                continue;
            }

            if let Some((args, call_span)) = self.parse_call_args()? {
                selectors.push(crate::ast::Selector::Call(args, call_span));
                continue;
            }

            break;
        }

        // Hvis der hverken var ".field", "[...]" eller "(...)" osv, er det stadig et designator: bare qualident
        // (men du kan vælge at fejle hvis du *kun* kalder denne funktion i situationer hvor du forventer selector)
        let end = selectors
            .last()
            .map(|s| s.span().end) // kræver `use crate::ast::Spanned;`
            .unwrap_or(head.span().end);

        let span = Span::new(head.span().start, end);

        Ok(crate::ast::Designator {
            head,
            selectors,
            span,
        })
    }

    fn parse_index_selector(&mut self) -> Result<Option<crate::ast::Selector>, ParserError> {
        if !self.at(&TokenKind::LSquare) {
            return Ok(None);
        }

        self.bump()?; // '['

        // (Oberon) index kan være flere udtryk adskilt af comma: a[i, j]
        let exprs = self.list_until(vec![TokenKind::RSquare], TokenKind::Comma,
                                        |p| p.parse_expression())?;
        self.expect(TokenKind::RSquare)?;
        // Hvis du vil have span på Index, er det bedst at gemme span i Selector::Index.
        // I min AST-variant var Index(Vec<Expression>) uden span; det fungerer, men span bliver “best effort”.
        // Her er den enkle version:
        Ok(Some(crate::ast::Selector::Index(exprs)))
    }

    fn parse_type_guard_selector(&mut self) -> Result<Option<crate::ast::Selector>, ParserError> {
        if !self.at(&TokenKind::LParen) {
            return Ok(None);
        }

        // Lookahead: ( Ident ( . Ident )? )
        // Vi må ikke "forbruge" tokens hvis det ikke matcher.
        // Den nemme løsning: kig på tokens med pos-indekser.

        let Some(t0) = self.tokens.get(self.pos) else { return Ok(None) };       // '('
        let Some(t1) = self.tokens.get(self.pos + 1) else { return Ok(None) };   // Ident?
        if t0.kind != TokenKind::LParen {
            return Ok(None);
        }
        let TokenKind::Ident(_) = &t1.kind else {
            return Ok(None);
        };

        // Nu kan det være enten:
        // ( Ident )
        // ( Ident . Ident )
        // Vi skal se om der findes ')' passende.
        // Case A: ( Ident )
        let is_close_after_one =
            matches!(self.tokens.get(self.pos + 2).map(|t| &t.kind), Some(TokenKind::RParen));

        // Case B: ( Ident . Ident )
        let is_two_part =
            matches!(self.tokens.get(self.pos + 2).map(|t| &t.kind), Some(TokenKind::Dot))
                && matches!(self.tokens.get(self.pos + 3).map(|t| &t.kind), Some(TokenKind::Ident(_)))
                && matches!(self.tokens.get(self.pos + 4).map(|t| &t.kind), Some(TokenKind::RParen));

        if !(is_close_after_one || is_two_part) {
            return Ok(None);
        }

        // Det matcher => forbrug tokens rigtigt
        let lpar = self.bump()?; // '('
        let first = self.expect_ident()?;

        let mut parts = vec![first];
        if self.at(&TokenKind::Dot) {
            self.bump()?;
            parts.push(self.expect_ident()?);
        }

        let rpar = self.expect(TokenKind::RParen)?;
        let qid = crate::ast::QualifiedIdentifier::new(parts);
        let span = Span::new(lpar.span.start, rpar.span.end);

        Ok(Some(crate::ast::Selector::TypeGuard(qid, span)))
    }

    fn parse_deref_selector(&mut self) -> Result<Option<crate::ast::Selector>, ParserError> {
        if !self.at(&TokenKind::Caret) {   // brug dit token-navn her
            return Ok(None);
        }

        let tok = self.bump()?; // '^'
        Ok(Some(crate::ast::Selector::Deref(tok.span)))
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
            first,
            span,
            second,
        })
    }

    fn parse_call_args(&mut self) -> Result<Option<(Vec<Expression>, Span)>, ParserError> {
        if !self.at(&TokenKind::LParen) { return Ok(None); }
        let lpar = self.bump()?;
        let args = self.list_until(vec![TokenKind::RParen], TokenKind::Comma, |p| p.parse_expression())?;
        let rpar = self.expect(TokenKind::RParen)?;
        Ok(Some((args, Span::new(lpar.span.start, rpar.span.end))))
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::*;
    use crate::lexer::{Token, TokenKind};
    use crate::span::Span;

    // ============================================================
    // helpers
    // ============================================================
    mod h {
        use super::*;

        pub fn sp(a: usize, b: usize) -> Span {
            Span::new(a, b)
        }

        pub fn t(kind: TokenKind, a: usize, b: usize) -> Token {
            Token::new(kind, sp(a, b))
        }

        // "Spanless" tokens for compact tests
        pub fn t0(kind: TokenKind) -> Token {
            Token::new(kind, sp(0, 0))
        }

        pub fn id0(name: &str) -> Token {
            t0(TokenKind::Ident(name.to_string()))
        }

        pub fn int0(v: i64) -> Token {
            t0(TokenKind::Int(v))
        }

        pub fn str0(s: &str) -> Token {
            t0(TokenKind::String(s.to_string()))
        }

        /// MODULE M; <body> END M.
        pub fn wrap_module(body: Vec<Token>) -> Vec<Token> {
            let mut v = vec![t0(TokenKind::Module), id0("M"), t0(TokenKind::SemiColon)];
            v.extend(body);
            v.extend([t0(TokenKind::End), id0("M"), t0(TokenKind::Dot), t0(TokenKind::Eof)]);
            v
        }

        /// MODULE M; CONST Foo = <expr>; END M.
        pub fn wrap_const_expr(expr: Vec<Token>) -> Vec<Token> {
            wrap_module({
                let mut v = vec![
                    t0(TokenKind::Const),
                    id0("Foo"),
                    t0(TokenKind::Equal),
                ];
                v.extend(expr);
                v.push(t0(TokenKind::SemiColon));
                v
            })
        }

        /// MODULE M; TYPE T = <type>; END M.
        pub fn wrap_type_decl(type_rhs: Vec<Token>) -> Vec<Token> {
            wrap_module({
                let mut v = vec![
                    t0(TokenKind::Type),
                    id0("T"),
                    t0(TokenKind::Equal),
                ];
                v.extend(type_rhs);
                v.push(t0(TokenKind::SemiColon));
                v
            })
        }

        pub fn parse_module(tokens: Vec<Token>) -> Module {
            let mut p = Parser { tokens: &tokens, pos: 0 };
            p.parse().unwrap()
        }

        pub fn parse_const_expr(expr_tokens: Vec<Token>) -> Expression {
            let m = parse_module(wrap_const_expr(expr_tokens));
            let [first, ..] = &m.declarations[..] else {
                panic!("expected at least one declaration");
            };
            let Declaration::Const { value, .. } = first else {
                panic!("expected first decl = Const, got {:?}", first);
            };
            value.clone()
        }

        pub fn parse_type(type_tokens: Vec<Token>) -> Type {
            let m = parse_module(wrap_type_decl(type_tokens));
            let [first, ..] = &m.declarations[..] else {
                panic!("expected at least one declaration");
            };
            let Declaration::Type { ty, .. } = first else {
                panic!("expected first decl = Type, got {:?}", first);
            };
            ty.clone()
        }

        // ---------- AST asserts (small, reusable) ----------

        pub fn assert_int(e: &Expression, v: i64) {
            match e {
                Expression::Int { value, .. } => assert_eq!(*value, v),
                other => panic!("expected Int({v}), got {other:?}"),
            }
        }

        pub fn assert_bool(e: &Expression, v: bool) {
            match e {
                Expression::Bool { value, .. } => assert_eq!(*value, v),
                other => panic!("expected Bool({v}), got {other:?}"),
            }
        }

        pub fn assert_string(e: &Expression, s: &str) {
            match e {
                Expression::String { value, .. } => assert_eq!(value, s),
                other => panic!("expected String({s}), got {other:?}"),
            }
        }

        pub fn assert_binary<'a>(
            e: &'a Expression,
            op: BinaryOperation,
        ) -> (&'a Expression, &'a Expression) {
            match e {
                Expression::Binary { op: o, lhs, rhs, .. } if *o == op => (lhs.as_ref(), rhs.as_ref()),
                other => panic!("expected Binary({op:?}), got {other:?}"),
            }
        }

        pub fn assert_designator<'a>(e: &'a Expression) -> &'a Designator {
            match e {
                Expression::Designator(d) => d,
                other => panic!("expected Designator, got {other:?}"),
            }
        }

        pub fn assert_named_type(t: &Type) -> &QualifiedIdentifier {
            match t {
                Type::Named { name, .. } => name,
                other => panic!("expected Named type, got {other:?}"),
            }
        }

        pub fn assert_array_type(t: &Type) -> (&[Expression], &Type) {
            match t {
                Type::Array { lengths, element, .. } => (lengths.as_slice(), element.as_ref()),
                other => panic!("expected Array type, got {other:?}"),
            }
        }
    }

    // ============================================================
    // module parsing
    // ============================================================
    mod module {
        use super::h::*;
        use super::*;

        #[test]
        fn empty_module_ok() {
            let tokens = wrap_module(vec![]);
            let m = parse_module(tokens);

            assert_eq!(m.name.text, "M");
            assert_eq!(m.end_name.text, "M");
            assert!(m.import_list.is_empty());
            assert!(m.declarations.is_empty());
            assert!(m.stmts.is_empty());
        }

        #[test]
        fn module_with_begin_block_ok() {
            // MODULE M; BEGIN END M.
            let tokens = wrap_module(vec![t0(TokenKind::Begin)]);
            let m = parse_module(tokens);
            assert_eq!(m.stmts.len(), 0);
        }
    }

    // ============================================================
    // imports
    // ============================================================
    mod imports {
        use super::h::*;
        use super::*;

        #[test]
        fn import_single_module() {
            // IMPORT Foo;
            let tokens = wrap_module(vec![
                t0(TokenKind::Import),
                id0("Foo"),
                t0(TokenKind::SemiColon),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.import_list.len(), 1);

            let imp = &m.import_list[0];
            assert_eq!(imp.module.text, "Foo");
            assert!(imp.alias.is_none());
        }

        #[test]
        fn import_with_alias() {
            // IMPORT A := Foo;
            let tokens = wrap_module(vec![
                t0(TokenKind::Import),
                id0("A"),
                t0(TokenKind::Assign),
                id0("Foo"),
                t0(TokenKind::SemiColon),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.import_list.len(), 1);

            let imp = &m.import_list[0];
            assert_eq!(imp.module.text, "Foo");               // module
            assert_eq!(imp.alias.as_ref().unwrap().text, "A");// alias
        }

        #[test]
        fn import_list_multiple() {
            // IMPORT Foo, A := Bar;
            let tokens = wrap_module(vec![
                t0(TokenKind::Import),
                id0("Foo"),
                t0(TokenKind::Comma),
                id0("A"),
                t0(TokenKind::Assign),
                id0("Bar"),
                t0(TokenKind::SemiColon),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.import_list.len(), 2);

            assert_eq!(m.import_list[0].module.text, "Foo");
            assert!(m.import_list[0].alias.is_none());

            assert_eq!(m.import_list[1].module.text, "Bar");
            assert_eq!(m.import_list[1].alias.as_ref().unwrap().text, "A");
        }
    }

    // ============================================================
    // declarations (const/type)
    // ============================================================
    mod decls {
        use super::h::*;
        use super::*;

        #[test]
        fn const_single() {
            let m = parse_module(wrap_module(vec![
                t0(TokenKind::Const),
                id0("Foo"),
                t0(TokenKind::Equal),
                int0(123),
                t0(TokenKind::SemiColon),
            ]));

            assert_eq!(m.declarations.len(), 1);
            let Declaration::Const { ident, value, .. } = &m.declarations[0] else {
                panic!("expected Const");
            };

            assert_eq!(ident.ident.text, "Foo");
            assert_eq!(ident.exported, false);
            assert_int(value, 123);
        }

        #[test]
        fn const_multiple_with_export() {
            // CONST Foo = 1; Bar* = 2;
            let m = parse_module(wrap_module(vec![
                t0(TokenKind::Const),

                id0("Foo"),
                t0(TokenKind::Equal),
                int0(1),
                t0(TokenKind::SemiColon),

                id0("Bar"),
                t0(TokenKind::Star),
                t0(TokenKind::Equal),
                int0(2),
                t0(TokenKind::SemiColon),
            ]));

            assert_eq!(m.declarations.len(), 2);

            let Declaration::Const { ident, .. } = &m.declarations[0] else { panic!("Const"); };
            assert_eq!(ident.ident.text, "Foo");
            assert!(!ident.exported);

            let Declaration::Const { ident, .. } = &m.declarations[1] else { panic!("Const"); };
            assert_eq!(ident.ident.text, "Bar");
            assert!(ident.exported);
        }

        #[test]
        fn type_named_single() {
            // TYPE T = M.B;
            let m = parse_module(wrap_module(vec![
                t0(TokenKind::Type),
                id0("T"),
                t0(TokenKind::Equal),
                id0("M"),
                t0(TokenKind::Dot),
                id0("B"),
                t0(TokenKind::SemiColon),
            ]));

            assert_eq!(m.declarations.len(), 1);
            let Declaration::Type { ident, ty, .. } = &m.declarations[0] else {
                panic!("expected Type");
            };
            assert_eq!(ident.ident.text, "T");

            let q = assert_named_type(ty);
            assert_eq!(q.parts.len(), 2);
            assert_eq!(q.parts[0].text, "M");
            assert_eq!(q.parts[1].text, "B");
        }
    }

    // ============================================================
    // types
    // ============================================================
    mod types {
        use super::h::*;
        use super::*;

        #[test]
        fn parse_named_type_one_part() {
            let ty = parse_type(vec![id0("INTEGER")]);
            let q = assert_named_type(&ty);
            assert_eq!(q.parts.len(), 1);
            assert_eq!(q.parts[0].text, "INTEGER");
        }

        #[test]
        fn parse_named_type_two_part() {
            let ty = parse_type(vec![id0("M"), t0(TokenKind::Dot), id0("T")]);
            let q = assert_named_type(&ty);
            assert_eq!(q.parts.len(), 2);
            assert_eq!(q.parts[0].text, "M");
            assert_eq!(q.parts[1].text, "T");
        }

        #[test]
        fn parse_array_type_with_multiple_lengths() {
            // ARRAY 2,3 OF T
            let ty = parse_type(vec![
                t0(TokenKind::Array),
                int0(2),
                t0(TokenKind::Comma),
                int0(3),
                t0(TokenKind::Of),
                id0("T"),
            ]);

            let (lens, base) = assert_array_type(&ty);
            assert_eq!(lens.len(), 2);
            assert_int(&lens[0], 2);
            assert_int(&lens[1], 3);

            let q = assert_named_type(base);
            assert_eq!(q.parts[0].text, "T");
        }

        #[test]
        fn parse_procedure_type_without_formals() {
            // PROCEDURE
            let ty = parse_type(vec![t0(TokenKind::Procedure)]);
            match ty {
                Type::Procedure { params, .. } => assert!(params.is_none()),
                other => panic!("expected Procedure type, got {other:?}"),
            }
        }

        #[test]
        fn parse_procedure_type_with_formals_and_return_type() {
            // PROCEDURE (x: INTEGER): BOOLEAN
            let ty = parse_type(vec![
                t0(TokenKind::Procedure),
                t0(TokenKind::LParen),
                id0("x"),
                t0(TokenKind::Colon),
                id0("INTEGER"),
                t0(TokenKind::RParen),
                t0(TokenKind::Colon),
                id0("BOOLEAN"),
            ]);

            match ty {
                Type::Procedure { params: Some(fp), .. } => {
                    assert_eq!(fp.sections.len(), 1);
                    assert!(fp.return_type.is_some());
                    assert_eq!(fp.return_type.unwrap().parts[0].text, "BOOLEAN");
                }
                other => panic!("expected Procedure(params=Some), got {other:?}"),
            }
        }
    }

    // ============================================================
    // expressions (literals, precedence, relation)
    // ============================================================
    mod expressions {
        use super::h::*;
        use super::*;

        #[test]
        fn literal_int() {
            let e = parse_const_expr(vec![int0(42)]);
            assert_int(&e, 42);
        }

        #[test]
        fn literal_string() {
            let e = parse_const_expr(vec![str0("hi")]);
            assert_string(&e, "hi");
        }

        #[test]
        fn precedence_mul_tighter_than_add() {
            // 1 + 2 * 3  => Add(1, Mul(2,3))
            let e = parse_const_expr(vec![
                int0(1),
                t0(TokenKind::Plus),
                int0(2),
                t0(TokenKind::Star),
                int0(3),
            ]);

            let (lhs, rhs) = assert_binary(&e, BinaryOperation::Addition);
            assert_int(lhs, 1);

            let (r1, r2) = assert_binary(rhs, BinaryOperation::Multiplication);
            assert_int(r1, 2);
            assert_int(r2, 3);
        }

        #[test]
        fn parentheses_override_precedence() {
            // (1 + 2) * 3  => Mul(Add(1,2),3)
            let e = parse_const_expr(vec![
                t0(TokenKind::LParen),
                int0(1),
                t0(TokenKind::Plus),
                int0(2),
                t0(TokenKind::RParen),
                t0(TokenKind::Star),
                int0(3),
            ]);

            let (lhs, rhs) = assert_binary(&e, BinaryOperation::Multiplication);
            let (a, b) = assert_binary(lhs, BinaryOperation::Addition);
            assert_int(a, 1);
            assert_int(b, 2);
            assert_int(rhs, 3);
        }

        #[test]
        fn relation_binds_looser_than_add_mul() {
            // 1 + 2 = 3 * 4  => Eq(Add(1,2), Mul(3,4))
            let e = parse_const_expr(vec![
                int0(1),
                t0(TokenKind::Plus),
                int0(2),
                t0(TokenKind::Equal),
                int0(3),
                t0(TokenKind::Star),
                int0(4),
            ]);

            let (lhs, rhs) = assert_binary(&e, BinaryOperation::Eq);

            let (a, b) = assert_binary(lhs, BinaryOperation::Addition);
            assert_int(a, 1);
            assert_int(b, 2);

            let (c, d) = assert_binary(rhs, BinaryOperation::Multiplication);
            assert_int(c, 3);
            assert_int(d, 4);
        }

        #[test]
        fn relation_is_not_chainable_should_error() {
            // expression = SimpleExpression [relation SimpleExpression]
            // => 1 < 2 < 3 should fail in a strict Oberon grammar
            let tokens = wrap_const_expr(vec![
                int0(1),
                t0(TokenKind::LessThan),
                int0(2),
                t0(TokenKind::LessThan),
                int0(3),
            ]);

            let mut p = Parser { tokens: &tokens, pos: 0 };
            assert!(p.parse().is_err());
        }

        #[test]
        fn set_literal_with_range() {
            // {1, 2..3}
            let e = parse_const_expr(vec![
                t0(TokenKind::LCurly),
                int0(1),
                t0(TokenKind::Comma),
                int0(2),
                t0(TokenKind::DotDot),
                int0(3),
                t0(TokenKind::RCurly),
            ]);

            let Expression::Set { elements, .. } = e else {
                panic!("expected Set");
            };
            assert_eq!(elements.len(), 2);

            assert_int(&elements[0].first, 1);
            assert!(elements[0].second.is_none());

            assert_int(&elements[1].first, 2);
            assert_int(elements[1].second.as_ref().unwrap(), 3);
        }
    }

    // ============================================================
    // designators + selectors
    // ============================================================
    mod designators {
        use super::h::*;
        use super::*;

        #[test]
        fn designator_plain_qualident() {
            // Foo  (parsed via factor: Ident => designator)
            let e = parse_const_expr(vec![id0("Foo")]);
            let d = assert_designator(&e);
            assert_eq!(d.head.parts.len(), 1);
            assert_eq!(d.head.parts[0].text, "Foo");
            assert!(d.selectors.is_empty());
        }

        #[test]
        fn designator_two_part_head_and_field() {
            // M.B.Bar
            let e = parse_const_expr(vec![
                id0("M"),
                t0(TokenKind::Dot),
                id0("B"),
                t0(TokenKind::Dot),
                id0("Bar"),
            ]);

            let d = assert_designator(&e);
            assert_eq!(d.head.parts.len(), 2);
            assert_eq!(d.head.parts[0].text, "M");
            assert_eq!(d.head.parts[1].text, "B");

            assert_eq!(d.selectors.len(), 1);
            match &d.selectors[0] {
                Selector::Field(id) => assert_eq!(id.text, "Bar"),
                other => panic!("expected Field selector, got {other:?}"),
            }
        }

        #[test]
        fn designator_index_then_field_then_call_then_deref() {
            // M.B[i+1,2].Bar(10)^
            let e = parse_const_expr(vec![
                id0("M"),
                t0(TokenKind::Dot),
                id0("B"),

                t0(TokenKind::LSquare),
                id0("i"),
                t0(TokenKind::Plus),
                int0(1),
                t0(TokenKind::Comma),
                int0(2),
                t0(TokenKind::RSquare),

                t0(TokenKind::Dot),
                id0("Bar"),

                t0(TokenKind::LParen),
                int0(10),
                t0(TokenKind::RParen),

                t0(TokenKind::Caret),
            ]);

            let d = assert_designator(&e);
            assert_eq!(d.head.parts.len(), 2);

            assert_eq!(d.selectors.len(), 4);

            // Index
            let Selector::Index(ix) = &d.selectors[0] else { panic!("expected Index"); };
            assert_eq!(ix.len(), 2);
            // i+1
            let (lhs, rhs) = assert_binary(&ix[0], BinaryOperation::Addition);
            // lhs should be designator 'i'
            let di = assert_designator(lhs);
            assert_eq!(di.head.parts[0].text, "i");
            assert_int(rhs, 1);
            assert_int(&ix[1], 2);

            // Field
            match &d.selectors[1] {
                Selector::Field(id) => assert_eq!(id.text, "Bar"),
                other => panic!("expected Field, got {other:?}"),
            }

            // Call
            let Selector::Call(args, _span) = &d.selectors[2] else { panic!("expected Call"); };
            assert_eq!(args.len(), 1);
            assert_int(&args[0], 10);

            // Deref
            match &d.selectors[3] {
                Selector::Deref(_sp) => {}
                other => panic!("expected Deref, got {other:?}"),
            }
        }

        #[test]
        fn designator_type_guard_selector() {
            // x(T)  - type guard selector (depends on your Selector::TypeGuard variant)
            // NOTE: This only works if your designator selector grammar supports "(qualident)" as selector
            let e = parse_const_expr(vec![
                id0("x"),
                t0(TokenKind::LParen),
                id0("T"),
                t0(TokenKind::RParen),
            ]);

            let d = assert_designator(&e);
            assert_eq!(d.selectors.len(), 1);

            match &d.selectors[0] {
                Selector::TypeGuard(qid, _sp) => {
                    assert_eq!(qid.parts.len(), 1);
                    assert_eq!(qid.parts[0].text, "T");
                }
                other => panic!("expected TypeGuard, got {other:?}"),
            }
        }
    }

    // ============================================================
    // statements (basic coverage)
    // ============================================================
    mod statements {
        use super::h::*;
        use super::*;

        #[test]
        fn begin_assign_statement() {
            // BEGIN x := 1
            let tokens = wrap_module(vec![
                t0(TokenKind::Begin),
                id0("x"),
                t0(TokenKind::Assign),
                int0(1),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.stmts.len(), 1);

            match &m.stmts[0] {
                Statement::Assign { target, value, .. } => {
                    let d = target; // Designator
                    assert_eq!(d.head.parts[0].text, "x");
                    assert_int(value, 1);
                }
                other => panic!("expected Assign, got {other:?}"),
            }
        }

        #[test]
        fn begin_call_statement_no_args() {
            // BEGIN Foo
            let tokens = wrap_module(vec![
                t0(TokenKind::Begin),
                id0("Foo"),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.stmts.len(), 1);

            match &m.stmts[0] {
                Statement::Call { callee, .. } => {
                    assert_eq!(callee.head.parts[0].text, "Foo");
                }
                other => panic!("expected Call, got {other:?}"),
            }
        }

        #[test]
        fn if_then_else() {
            // BEGIN IF TRUE THEN Foo ELSE Bar END
            let tokens = wrap_module(vec![
                t0(TokenKind::Begin),

                t0(TokenKind::If),
                t0(TokenKind::True),
                t0(TokenKind::Then),
                id0("Foo"),

                t0(TokenKind::Else),
                id0("Bar"),

                t0(TokenKind::End),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.stmts.len(), 1);

            let Statement::If { cond, then_branch, else_branch, .. } = &m.stmts[0] else {
                panic!("expected If statement");
            };

            assert_bool(cond, true);
            assert_eq!(then_branch.len(), 1);
            assert!(else_branch.is_some());
            assert_eq!(else_branch.as_ref().unwrap().len(), 1);
        }

        #[test]
        fn while_do_end() {
            // BEGIN WHILE TRUE DO Foo END
            let tokens = wrap_module(vec![
                t0(TokenKind::Begin),

                t0(TokenKind::While),
                t0(TokenKind::True),
                t0(TokenKind::Do),
                id0("Foo"),
                t0(TokenKind::End),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.stmts.len(), 1);

            match &m.stmts[0] {
                Statement::While { cond, body, .. } => {
                    assert_bool(cond, true);
                    assert_eq!(body.len(), 1);
                }
                other => panic!("expected While, got {other:?}"),
            }
        }

        #[test]
        fn for_loop_with_by() {
            // BEGIN FOR i := 1 TO 10 BY 2 DO Foo END
            let tokens = wrap_module(vec![
                t0(TokenKind::Begin),

                t0(TokenKind::For),
                id0("i"),
                t0(TokenKind::Assign),
                int0(1),
                t0(TokenKind::To),
                int0(10),
                t0(TokenKind::By),
                int0(2),
                t0(TokenKind::Do),
                id0("Foo"),
                t0(TokenKind::End),
            ]);

            let m = parse_module(tokens);
            assert_eq!(m.stmts.len(), 1);

            match &m.stmts[0] {
                Statement::For { var, low, high, by, body, .. } => {
                    assert_eq!(var.text, "i");
                    assert_int(low, 1);
                    assert_int(high, 10);
                    assert!(by.is_some());
                    assert_int(by.as_ref().unwrap(), 2);
                    assert_eq!(body.len(), 1);
                }
                other => panic!("expected For, got {other:?}"),
            }
        }
    }
}
