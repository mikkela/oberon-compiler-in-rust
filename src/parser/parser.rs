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
    use crate::ast::{BinaryOperation, Declaration, Expression, Identifier, IdentifierDef, Selector, Type, UnaryOperation};
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
    (Record $a:expr, $b:expr) => { t(TokenKind::Record, $a, $b) };
    (Pointer $a:expr, $b:expr) => { t(TokenKind::Pointer, $a, $b) };
    (To $a:expr, $b:expr) => { t(TokenKind::To, $a, $b) };
    (Procedure $a:expr, $b:expr) => { t(TokenKind::Procedure, $a, $b) };
    (Var $a:expr, $b:expr) => { t(TokenKind::Var, $a, $b) };

    (Ident $s:literal, $a:expr, $b:expr) => { ident($s, $a, $b) };
    (Int   $v:expr,    $a:expr, $b:expr) => { int($v, $a, $b) };

    (True $a:expr, $b:expr) => { t(TokenKind::True, $a, $b) };
    (False $a:expr, $b:expr) => { t(TokenKind::False, $a, $b) };

    (Semi   $a:expr, $b:expr) => { t(TokenKind::SemiColon,$a, $b) };
    (Comma  $a:expr, $b:expr) => { t(TokenKind::Comma,    $a, $b) };
    (Assign $a:expr, $b:expr) => { t(TokenKind::Assign,   $a, $b) };
    (Equal  $a:expr, $b:expr) => { t(TokenKind::Equal,    $a, $b) };
    (Star   $a:expr, $b:expr) => { t(TokenKind::Star,     $a, $b) };
    (Dot    $a:expr, $b:expr) => { t(TokenKind::Dot,      $a, $b) };
    (DotDot $a:expr, $b:expr) => { t(TokenKind::DotDot,   $a, $b) };
    (Colon  $a:expr, $b:expr) => { t(TokenKind::Colon,    $a, $b) };
    (Caret  $a:expr, $b:expr) => { t(TokenKind::Caret,    $a, $b) };
    (Tilde  $a:expr, $b:expr) => { t(TokenKind::Tilde,    $a, $b) };
    (Assign $a:expr, $b:expr) => { t(TokenKind::Assign,   $a, $b) };
    (Pipe   $a:expr, $b:expr) => { t(TokenKind::Pipe,     $a, $b) };

    (LParen $a:expr, $b:expr) => { t(TokenKind::LParen,   $a, $b) };
    (RParen $a:expr, $b:expr) => { t(TokenKind::RParen,   $a, $b) };

    (LCurly $a:expr, $b:expr) => { t(TokenKind::LCurly,   $a, $b) };
    (RCurly $a:expr, $b:expr) => { t(TokenKind::RCurly,   $a, $b) };

    (LSquare $a:expr, $b:expr) => { t(TokenKind::LSquare, $a, $b) };
    (RSquare $a:expr, $b:expr) => { t(TokenKind::RSquare, $a, $b) };

    (Begin $a:expr, $b:expr) => { t(TokenKind::Begin,     $a, $b) };
    (End $a:expr, $b:expr) => { t(TokenKind::End,         $a, $b) };
    (Eof $a:expr, $b:expr) => { t(TokenKind::Eof,         $a, $b) };

    (Ampersand $a:expr, $b:expr) => { t(TokenKind::Ampersand, $a, $b) };
    (Or $a:expr, $b:expr) => { t(TokenKind::Or, $a, $b) };
    (Neq $a:expr, $b:expr) => { t(TokenKind::NotEqual, $a, $b) };

    (If $a:expr, $b:expr) => { t(TokenKind::If, $a, $b) };
    (Then $a:expr, $b:expr) => { t(TokenKind::Then, $a, $b) };
    (Else $a:expr, $b:expr) => { t(TokenKind::Else, $a, $b) };
    (Elsif $a:expr, $b:expr) => { t(TokenKind::Elsif, $a, $b) };
    (Case $a:expr, $b:expr) => { t(TokenKind::Case, $a, $b) };
    (While $a:expr, $b:expr) => { t(TokenKind::While, $a, $b) };
    (Do $a:expr, $b:expr) => { t(TokenKind::Do, $a, $b) };
    (Repeat $a:expr, $b:expr) => { t(TokenKind::Repeat, $a, $b) };
    (Until $a:expr, $b:expr) => { t(TokenKind::Until, $a, $b) };
    (For $a:expr, $b:expr) => { t(TokenKind::For, $a, $b) };
    (To $a:expr, $b:expr) => { t(TokenKind::To, $a, $b) };
    (By $a:expr, $b:expr) => { t(TokenKind::By, $a, $b) };
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

        assert_eq!(module.name.text, "monkey");
        assert_eq!(module.end_name.text, "monkey2");
        assert_eq!(module.import_list.len(), 0);
        assert_eq!(module.stmts.len(), 0);
        assert_eq!(module.declarations.len(), 0);
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
        assert_eq!(first_import.module.text, "foo");
        assert_eq!(first_import.alias, None);

        let second_import = module.import_list[1].clone();
        assert_eq!(second_import.module.text, "baz");
        assert_eq!(second_import.alias.unwrap().text, "bar");
    }

    #[test]
    fn parse_empty_const() {
        let body = vec![
            tok!(Const 14, 19),
            // empty const section
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 0);
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

        assert_eq!(module.declarations.len(), 1);
        assert!(matches!(
            &module.declarations[0],
            Declaration::Const { ident: IdentifierDef { ident: Identifier { text: identifier, .. }, .. }, .. }
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

        assert_eq!(module.declarations.len(), 2);

        assert!(matches!(
            &module.declarations[0],
            Declaration::Const { ident: IdentifierDef { ident: Identifier { text: identifier, .. }, exported, .. }, .. }
                if identifier == "Foo" && !*exported
        ));

        assert!(matches!(
            &module.declarations[1],
            Declaration::Const { ident: IdentifierDef { ident: Identifier { text: identifier, .. }, exported, .. }, .. }
                if identifier == "Bar" && *exported
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

        assert_eq!(module.declarations.len(), 0);
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

        assert_eq!(module.declarations.len(), 1);
        let decl = &module.declarations[0];

        if let Declaration::Type { ty: Type::Named { name, .. }, .. } = decl {
            assert_eq!(name.parts.len(), 2);
            assert_eq!(name.parts[0].text, "M");
            assert_eq!(name.parts[1].text, "B");
        } else {
            panic!("expected a named type declaration");
        }
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

        assert_eq!(module.declarations.len(), 1);

        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Array { lengths, element, .. } = ty else {
            panic!("expected Array type");
        };

        assert_eq!(
            lengths,
            &vec![
                Int { value: 2, span: Span::new(29, 31) },
                Int { value: 3, span: Span::new(32, 33) }
            ]
        );

        let Type::Named { name, .. } = element.as_ref() else {
            panic!("expected named element type");
        };
        assert_eq!(name.parts.len(), 1);
        assert_eq!(name.parts[0].text, "T");
    }

    #[test]
    fn parse_record_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Record 24, 31),
            tok!(LParen 31, 32),
            tok!(Ident "A", 32, 33),
            tok!(RParen 33, 34),
            tok!(Ident "b", 35, 36),
            tok!(Comma 36, 37),
            tok!(Ident "c", 37, 38),
            tok!(Star 38, 39),
            tok!(Colon 39, 40),
            tok!(Ident "T", 41, 44),
            tok!(Semi 44, 45),
            tok!(Ident "d", 46, 47),
            tok!(Colon 47, 48),
            tok!(Ident "U", 49, 51),
            tok!(End 51, 54),
            tok!(Semi 54, 55),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Record { base, field_lists, .. } = ty else {
            panic!("expected Array type");
        };
        assert!(base.is_some());
        let name = base.as_ref().unwrap();

        assert_eq!(name.parts.len(), 1);
        assert_eq!(name.parts[0].text, "A");

        assert_eq!(field_lists.len(), 2);
        let field_list_1 = field_lists[0].clone();
        assert_eq!(field_list_1.fields.len(), 2);
        assert_eq!(field_list_1.fields[0].ident.text, "b");
        assert_eq!(field_list_1.fields[0].exported, false);
        assert_eq!(field_list_1.fields[1].ident.text, "c");
        assert_eq!(field_list_1.fields[1].exported, true);
        let Type::Named { name, .. } = field_list_1.ty else {
            panic!("expected named element type");
        };
        assert_eq!(name.parts.len(), 1);
        assert_eq!(name.parts[0].text, "T");

        let field_list_2 = field_lists[1].clone();
        assert_eq!(field_list_2.fields.len(), 1);
        assert_eq!(field_list_2.fields[0].ident.text, "d");
        assert_eq!(field_list_2.fields[0].exported, false);
        let Type::Named { name, .. } = field_list_2.ty else {
            panic!("expected named element type");
        };
        assert_eq!(name.parts.len(), 1);
        assert_eq!(name.parts[0].text, "U");
    }

    #[test]
    fn parse_pointer_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Pointer 24, 31),
            tok!(To 31, 32),
            tok!(Ident "A", 32, 33),
            tok!(Semi 54, 55),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Pointer { pointee, .. } = ty else {
            panic!("expected Array type");
        };

        let Type::Named { name , .. } = pointee.deref() else {
            panic!("expected Named type");
        };
        assert_eq!(name.parts.len(), 1);
        assert_eq!(name.parts[0].text, "A");
    }

    #[test]
    fn parse_minimal_procedure_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(Semi 31, 32),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_none());
    }

    #[test]
    fn parse_procedure_with_empty_parameters_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(LParen 31, 32),
            tok!(RParen 32, 33),
            tok!(Semi 34, 35),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_some());
        let parameters = params.as_ref().unwrap();
        assert_eq!(parameters.return_type, None);
        assert_eq!(parameters.sections.len(), 0);
    }

    #[test]
    fn parse_procedure_with_empty_parameters_and_return_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(LParen 31, 32),
            tok!(RParen 32, 33),
            tok!(Colon 33, 34),
            tok!(Ident "T", 35, 38),
            tok!(Semi 38, 39),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_some());
        let parameters = params.as_ref().unwrap();
        assert_eq!(parameters.sections.len(), 0);
        assert!(parameters.return_type.is_some());
        let return_type = parameters.return_type.clone().unwrap();
        assert!(return_type.parts.len() == 1);
        assert_eq!(return_type.parts[0].text, "T");
    }

    #[test]
    fn parse_procedure_with_one_fp_section_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(LParen 31, 32),
            tok!(Ident "a", 32, 33),
            tok!(Colon 33, 34),
            tok!(Ident "T", 35, 38),
            tok!(RParen 38, 39),
            tok!(Semi 39, 40),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_some());
        let parameters = params.as_ref().unwrap();
        assert_eq!(parameters.return_type, None);
        assert_eq!(parameters.sections.len(), 1);
        assert_eq!(parameters.sections[0].names.len(), 1);
        assert_eq!(parameters.sections[0].names[0].text, "a");
        assert_eq!(parameters.sections[0].ty.open_arrays, 0);
        assert_eq!(parameters.sections[0].ty.base.parts.len(), 1);
        assert_eq!(parameters.sections[0].ty.base.parts[0].text, "T");
        assert_eq!(parameters.sections[0].by_ref, false);
    }

    #[test]
    fn parse_procedure_with_one_fp_section_with_multiple_args_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(LParen 31, 32),
            tok!(Ident "a", 32, 33),
            tok!(Comma 33, 34),
            tok!(Ident "b", 34, 35),
            tok!(Colon 35, 36),
            tok!(Ident "T", 37, 40),
            tok!(RParen 40, 41),
            tok!(Semi 41, 42),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_some());
        let parameters = params.as_ref().unwrap();
        assert_eq!(parameters.return_type, None);
        assert_eq!(parameters.sections.len(), 1);
        assert_eq!(parameters.sections[0].names.len(), 2);
        assert_eq!(parameters.sections[0].names[0].text, "a");
        assert_eq!(parameters.sections[0].names[1].text, "b");
        assert_eq!(parameters.sections[0].ty.open_arrays, 0);
        assert_eq!(parameters.sections[0].ty.base.parts.len(), 1);
        assert_eq!(parameters.sections[0].ty.base.parts[0].text, "T");
        assert_eq!(parameters.sections[0].by_ref, false);
    }

    #[test]
    fn parse_procedure_with_one_fp_section_with_var_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(LParen 31, 32),
            tok!(Var 32, 35),
            tok!(Ident "a", 36, 37),
            tok!(Colon 37, 38),
            tok!(Ident "T", 39, 40),
            tok!(RParen 40, 41),
            tok!(Semi 41, 42),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_some());
        let parameters = params.as_ref().unwrap();
        assert_eq!(parameters.return_type, None);
        assert_eq!(parameters.sections.len(), 1);
        assert_eq!(parameters.sections[0].names.len(), 1);
        assert_eq!(parameters.sections[0].names[0].text, "a");
        assert_eq!(parameters.sections[0].ty.open_arrays, 0);
        assert_eq!(parameters.sections[0].ty.base.parts.len(), 1);
        assert_eq!(parameters.sections[0].ty.base.parts[0].text, "T");
        assert_eq!(parameters.sections[0].by_ref, true);
    }

    #[test]
    fn parse_procedure_with_two_fp_sections_type() {
        let body = vec![
            tok!(Type 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            tok!(Procedure 24, 31),
            tok!(LParen 31, 32),
            tok!(Ident "a", 32, 33),
            tok!(Colon 33, 34),
            tok!(Ident "T", 35, 38),
            tok!(Semi 38, 39),
            tok!(Ident "b", 39, 40),
            tok!(Colon 40, 41),
            tok!(Ident "U", 42, 44),
            tok!(RParen 44, 45),
            tok!(Semi 45, 46)
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let module = parse_module(tokens);

        assert_eq!(module.declarations.len(), 1);
        let Declaration::Type { ident, ty, .. } = &module.declarations[0] else {
            panic!("expected Type declaration");
        };
        assert_eq!(ident.ident.text, "Foo");
        assert_eq!(ident.exported, false);

        let Type::Procedure { params, .. } = ty else {
            panic!("expected Procedure type");
        };

        assert!(params.is_some());
        let parameters = params.as_ref().unwrap();
        assert_eq!(parameters.return_type, None);
        assert_eq!(parameters.sections.len(), 2);
        assert_eq!(parameters.sections[0].names.len(), 1);
        assert_eq!(parameters.sections[0].names[0].text, "a");
        assert_eq!(parameters.sections[0].ty.open_arrays, 0);
        assert_eq!(parameters.sections[0].ty.base.parts.len(), 1);
        assert_eq!(parameters.sections[0].ty.base.parts[0].text, "T");
        assert_eq!(parameters.sections[0].by_ref, false);
        assert_eq!(parameters.sections[1].names.len(), 1);
        assert_eq!(parameters.sections[1].names[0].text, "b");
        assert_eq!(parameters.sections[1].ty.open_arrays, 0);
        assert_eq!(parameters.sections[1].ty.base.parts.len(), 1);
        assert_eq!(parameters.sections[1].ty.base.parts[0].text, "U");
        assert_eq!(parameters.sections[1].by_ref, false);
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

        assert_eq!(module.declarations.len(), 1);

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };

        let Expression::Set { elements, .. } = value else {
            panic!("expected Set expression");
        };

        assert_eq!(elements.len(), 2);

        assert_eq!(
            elements[0].first,
            Int { value: 1, span: Span::new(26, 27) }
        );
        assert!(elements[0].second.is_none());

        assert_eq!(
            elements[1].first,
            Int { value: 2, span: Span::new(28, 29) }
        );
        assert_eq!(
            elements[1].second.as_ref().unwrap(),
            &Int { value: 3, span: Span::new(31, 32) }
        );
    }

    #[test]
    fn parse_type_guard_selector_simple() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // x(T)
            tok!(Ident "x", 25, 26),
            tok!(LParen 26, 27),
            tok!(Ident "T", 27, 28),
            tok!(RParen 28, 29),
            tok!(Semi 29, 30),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        assert_eq!(module.declarations.len(), 1);

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };

        let Expression::Designator(d) = value else {
            panic!("expected Designator expression");
        };

        // head = x
        assert_eq!(d.head.parts.len(), 1);
        assert_eq!(d.head.parts[0].text, "x");

        // selectors = [(T)] as TypeGuard
        assert_eq!(d.selectors.len(), 1);
        match &d.selectors[0] {
            Selector::TypeGuard(qid, span) => {
                assert_eq!(*span, Span::new(26, 29)); // span covering "(T)"
                assert_eq!(qid.parts.len(), 1);
                assert_eq!(qid.parts[0].text, "T");
            }
            other => panic!("expected TypeGuard selector, got {other:?}"),
        }
    }

    #[test]
    fn parse_type_guard_selector_qualified() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // x(M.T)
            tok!(Ident "x", 25, 26),
            tok!(LParen 26, 27),
            tok!(Ident "M", 27, 28),
            tok!(Dot 28, 29),
            tok!(Ident "T", 29, 30),
            tok!(RParen 30, 31),
            tok!(Semi 31, 32)
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Designator(d) = value else {
            panic!("expected Designator expression");
        };

        assert_eq!(d.head.parts[0].text, "x");
        assert_eq!(d.selectors.len(), 1);

        match &d.selectors[0] {
            Selector::TypeGuard(qid, span) => {
                assert_eq!(*span, Span::new(26, 31)); // "(M.T)"
                assert_eq!(qid.parts.len(), 2);
                assert_eq!(qid.parts[0].text, "M");
                assert_eq!(qid.parts[1].text, "T");
            }
            other => panic!("expected TypeGuard selector, got {other:?}"),
        }
    }

    #[test]
    fn parse_call_not_type_guard_disambiguation() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // x(1)  -> MUST be Call, not TypeGuard
            tok!(Ident "x", 25, 26),
            tok!(LParen 26, 27),
            tok!(Int 1, 27, 28),
            tok!(RParen 28, 29),
            tok!(Semi 29, 30),
        ];

        let tokens = module_tokens("monkey", "monkey", body);

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Designator(d) = value else {
            panic!("expected Designator expression");
        };

        assert_eq!(d.selectors.len(), 1);
        match &d.selectors[0] {
            Selector::Call(args, span) => {
                assert_eq!(*span, Span::new(26, 29));
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], Expression::Int { value: 1, span: Span::new(27, 28) });
            }
            other => panic!("expected Call selector, got {other:?}"),
        }
    }

    #[test]
    fn parse_type_guard_then_call_chain() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // x(T)(1)
            tok!(Ident "x", 25, 26),
            tok!(LParen 26, 27),
            tok!(Ident "T", 27, 28),
            tok!(RParen 28, 29),
            tok!(LParen 29, 30),
            tok!(Int 1, 30, 31),
            tok!(RParen 31, 32),
            tok!(Semi 32, 33),
        ];

        let tokens = module_tokens("monkey", "monkey", body);

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Designator(d) = value else {
            panic!("expected Designator expression");
        };

        assert_eq!(d.head.parts.len(), 1);
        assert_eq!(d.head.parts[0].text, "x");
        assert_eq!(d.selectors.len(), 2);

        // (T)
        match &d.selectors[0] {
            Selector::TypeGuard(qid, span) => {
                assert_eq!(*span, Span::new(26, 29));
                assert_eq!(qid.parts.len(), 1);
                assert_eq!(qid.parts[0].text, "T");
            }
            other => panic!("expected TypeGuard selector, got {other:?}"),
        }

        // (1)
        match &d.selectors[1] {
            Selector::Call(args, span) => {
                assert_eq!(*span, Span::new(29, 32));
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], Expression::Int { value: 1, span: Span::new(30, 31) });
            }
            other => panic!("expected Call selector, got {other:?}"),
        }
    }
    #[test]
    fn parse_index_call_deref_field_chain() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // x[1](2)^.y
            tok!(Ident "x", 25, 26),
            tok!(LSquare 26, 27),
            tok!(Int 1, 27, 28),
            tok!(RSquare 28, 29),
            tok!(LParen 29, 30),
            tok!(Int 2, 30, 31),
            tok!(RParen 31, 32),
            tok!(Caret 32, 33),
            tok!(Dot 33, 34),
            tok!(Ident "y", 35, 37),
            tok!(Semi 37, 38),
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Designator(d) = value else {
            panic!("expected Designator expression");
        };

        assert_eq!(d.head.parts.len(), 1);
        assert_eq!(d.head.parts[0].text, "x");
        assert_eq!(d.selectors.len(), 4);

        // [1]
        match &d.selectors[0] {
            Selector::Index(exprs) => {
                assert_eq!(exprs.len(), 1);
                assert_eq!(exprs[0], Expression::Int { value: 1, span: Span::new(27, 28) });
            }
            other => panic!("expected Index selector, got {other:?}"),
        }

        // (2)
        match &d.selectors[1] {
            Selector::Call(args, span) => {
                assert_eq!(*span, Span::new(29, 32));
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], Expression::Int { value: 2, span: Span::new(30, 31) });
            }
            other => panic!("expected Call selector, got {other:?}"),
        }

        // ^
        match &d.selectors[2] {
            Selector::Deref(span) => {
                assert_eq!(*span, Span::new(32, 33));
            }
            other => panic!("expected Deref selector, got {other:?}"),
        }

        // .y
        match &d.selectors[3] {
            Selector::Field(id) => {
                assert_eq!(id.text, "y");
                assert_eq!(id.span, Span::new(35, 37));
            }
            other => panic!("expected Field selector, got {other:?}"),
        }
    }

    #[test]
    fn parse_tilde_expression() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // ~TRUE
            tok!(Tilde 25, 26),
            tok!(True 26, 29),
            tok!(Semi 29, 30)
        ];

        let tokens = module_tokens("monkey", "monkey", body);

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Unary { op, operand, ..} = value else {
            panic!("expected Unary expression");
        };

        assert_eq!(*op, UnaryOperation::Not);
        let Expression::Bool { value, .. } = (*operand).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, true)
    }

    #[test]
    fn parse_parenthesis_expression() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // (TRUE)
            tok!(LParen 25, 26),
            tok!(True 26, 29),
            tok!(RParen 29, 30),
            tok!(Semi 30, 31)
        ];

        let tokens = module_tokens("monkey", "monkey", body);

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Bool { value, .. } = value else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, true)
    }

    #[test]
    fn parse_binary_term_expression() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // TRUE & FALSE
            tok!(True 25, 28),
            tok!(Ampersand 28, 29),
            tok!(False 29, 33),
            tok!(Semi 33, 34)
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Binary { op, lhs, rhs, ..} = value else {
            panic!("expected Binary expression");
        };

        assert_eq!(*op, BinaryOperation::And);
        let Expression::Bool { value, .. } = (*lhs).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, true);
        let Expression::Bool { value, .. } = (*rhs).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, false);
    }

    #[test]
    fn parse_binary_simple_expression() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // TRUE or FALSE
            tok!(True 25, 28),
            tok!(Or 28, 30),
            tok!(False 30, 34),
            tok!(Semi 34, 35)
        ];

        let tokens = module_tokens("monkey", "monkey", body);
        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Binary { op, lhs, rhs, ..} = value else {
            panic!("expected Binary expression");
        };

        assert_eq!(*op, BinaryOperation::Or);
        let Expression::Bool { value, .. } = (*lhs).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, true);
        let Expression::Bool { value, .. } = (*rhs).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, false);
    }

    #[test]
    fn parse_binary_relation_expression() {
        let body = vec![
            tok!(Const 14, 19),
            tok!(Ident "Foo", 20, 23),
            tok!(Equal 23, 24),
            // TRUE # FALSE
            tok!(True 25, 28),
            tok!(Neq 28, 29),
            tok!(False 29, 33),
            tok!(Semi 33, 34)
        ];

        let tokens = module_tokens("monkey", "monkey", body);

        let mut parser = super::Parser { tokens: &tokens, pos: 0 };
        let module = parser.parse().unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        let Expression::Binary { op, lhs, rhs, ..} = value else {
            panic!("expected Binary expression");
        };

        assert_eq!(*op, BinaryOperation::Neq);
        let Expression::Bool { value, .. } = (*lhs).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, true);
        let Expression::Bool { value, .. } = (*rhs).deref() else {
            panic!("expected Bool literal");
        };
        assert_eq!(*value, false);
    }

    mod statements {
        use super::*;
        use crate::ast::{Label, LabelValue, Statement};

        #[test]
        fn parse_simplest_call() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Ident "a", 20, 21),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Call { callee, .. } = stmt else {
                panic!("expected Call Statement");
            };
            assert_eq!(callee.head.parts.len(), 1);
            assert_eq!(
                callee.head.parts[0].text,
                "a"
            );
            assert_eq!(callee.selectors.len(), 0);
        }

        #[test]
        fn parse_no_parameters_call() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Ident "a", 20, 21),
                tok!(LParen 21, 22),
                tok!(RParen 22, 23),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Call { callee, .. } = stmt else {
                panic!("expected Call Statement");
            };
            assert_eq!(callee.head.parts.len(), 1);
            assert_eq!(
                callee.head.parts[0].text,
                "a"
            );
            assert_eq!(callee.selectors.len(), 1);
            let selector = &callee.selectors[0];
            match selector {
                Selector::Call(args, _) => assert_eq!(args.len(), 0),
                other => panic!("expected Call selector, got {other:?}"),
            }
        }

        #[test]
        fn parse_single_parameters_call() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Ident "a", 20, 21),
                tok!(LParen 21, 22),
                tok!(Int 1, 22, 23),
                tok!(RParen 23, 24),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Call { callee, .. } = stmt else {
                panic!("expected Call Statement");
            };
            assert_eq!(callee.head.parts.len(), 1);
            assert_eq!(
                callee.head.parts[0].text,
                "a"
            );
            assert_eq!(callee.selectors.len(), 1);
            let selector = &callee.selectors[0];
            let Selector::Call(args, _) = selector else {
                panic!("expected Call selector")
            };
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Expression::Int { value: 1, span: Span::new(22, 23) });
        }

        #[test]
        fn parse_multiple_parameters_call() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Ident "a", 20, 21),
                tok!(LParen  21, 22),
                tok!(Int 2, 23, 24),
                tok!(Comma  24, 25),
                tok!(Int 10, 25, 26),
                tok!(RParen 26, 27),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Call { callee, .. } = stmt else {
                panic!("expected Call Statement");
            };
            assert_eq!(callee.head.parts.len(), 1);
            assert_eq!(
                callee.head.parts[0].text,
                "a"
            );
            assert_eq!(callee.selectors.len(), 1);
            let selector = &callee.selectors[0];
            let Selector::Call(args, _) = selector else {
                panic!("expected Call selector")
            };
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Expression::Int { value: 2, span: Span::new(23, 24) });
            assert_eq!(args[1], Expression::Int { value: 10, span: Span::new(25, 26) });
        }

        #[test]
        fn parse_assignment() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Ident "a", 20, 21),
                tok!(Assign 21, 23),
                tok!(Int 1, 23, 24),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Assign { target, value, .. } = stmt else {
                panic!("expected Assign Statement");
            };
            assert_eq!(target.head.parts.len(), 1);
            assert_eq!(
                target.head.parts[0].text,
                "a"
            );
            assert_eq!(target.selectors.len(), 0);
            assert_eq!(value, Expression::Int { value: 1, span: Span::new(23, 24) });
        }

        #[test]
        fn parse_if_then_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(If 20, 22),
                tok!(True 23, 27),
                tok!(Then 27, 31),
                tok!(Ident "a", 32, 33),
                tok!(End 34, 37),

            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::If { cond, then_branch, elsif_branches, else_branch, .. } = stmt else {
                panic!("expected If Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(23, 27) });
            assert_eq!(then_branch.len(), 1);
            assert!(elsif_branches.is_empty());
            assert!(else_branch.is_none());
        }

        #[test]
        fn parse_if_then_else_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(If 20, 22),
                tok!(True 23, 27),
                tok!(Then 27, 31),
                tok!(Ident "a", 32, 33),
                tok!(Else 33, 36),
                tok!(Ident "b", 37, 38),
                tok!(End 39, 41),

            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::If { cond, then_branch, elsif_branches, else_branch, .. } = stmt else {
                panic!("expected If Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(23, 27) });
            assert_eq!(then_branch.len(), 1);
            assert!(elsif_branches.is_empty());
            assert!(else_branch.is_some());
            assert_eq!(else_branch.unwrap().len(), 1);
        }

        #[test]
        fn parse_if_then_elsif_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(If 20, 22),
                tok!(True 23, 27),
                tok!(Then 27, 31),
                tok!(Ident "a", 32, 33),
                tok!(Elsif 33, 36),
                tok!(False 37, 41),
                tok!(Then 41, 45),
                tok!(Ident "b", 46, 47),
                tok!(End 48, 50),

            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::If { cond, then_branch, elsif_branches, else_branch, .. } = stmt else {
                panic!("expected If Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(23, 27) });
            assert_eq!(then_branch.len(), 1);
            assert_eq!(elsif_branches.len(), 1);
            assert!(else_branch.is_none());
        }

        #[test]
        fn parse_if_then_elsif_else_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(If 20, 22),
                tok!(True 23, 27),
                tok!(Then 27, 31),
                tok!(Ident "a", 32, 33),
                tok!(Elsif 33, 36),
                tok!(False 37, 41),
                tok!(Then 41, 45),
                tok!(Ident "b", 46, 47),
                tok!(Elsif 47, 50),
                tok!(True 51, 55),
                tok!(Then 55, 59),
                tok!(Ident "c", 60, 61),
                tok!(Else 61, 64),
                tok!(Ident "d", 65, 66),
                tok!(End 48, 50),

            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::If { cond, then_branch, elsif_branches, else_branch, .. } = stmt else {
                panic!("expected If Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(23, 27) });
            assert_eq!(then_branch.len(), 1);
            assert_eq!(elsif_branches.len(), 2);
            assert!(else_branch.is_some());
        }

        #[test]
        fn parse_case_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Case 20, 24),
                tok!(Ident "a", 24, 25),
                tok!(Of 25, 27),
                tok!(Int 1, 27, 28),
                tok!(Colon 28, 29),
                tok!(Ident "b", 29, 30),
                tok!(End 31, 34),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Case { branches, .. } = stmt else {
                panic!("expected Case Statement");
            };
            assert_eq!(branches.len(), 1);
            assert_eq!(branches[0].label_list.len(), 1);
            let label = &branches[0].label_list[0];
            let Label::Single { value, ..} = label else {
                panic!("expected Single label");
            };
            let LabelValue::Integer { value: 1, .. } = value else {
                panic!("expected Int label value");
            };
        }

        #[test]
        fn parse_case_with_range_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Case 20, 24),
                tok!(Ident "a", 24, 25),
                tok!(Of 25, 27),
                tok!(Int 1, 27, 28),
                tok!(DotDot 28, 30),
                tok!(Int 10, 30, 31),
                tok!(Colon 31, 32),
                tok!(Ident "b", 32, 33),
                tok!(End 34, 37),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Case { branches, .. } = stmt else {
                panic!("expected Case Statement");
            };
            assert_eq!(branches.len(), 1);
            assert_eq!(branches[0].label_list.len(), 1);
            let label = &branches[0].label_list[0];
            let Label::Range { low, high, ..} = label else {
                panic!("expected Single label");
            };
            let LabelValue::Integer { value: 1, .. } = low else {
                panic!("expected Int label value");
            };
            let LabelValue::Integer { value: 10, .. } = high else {
                panic!("expected Int label value");
            };
        }

        #[test]
        fn parse_case_with_multiple_labels_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Case 20, 24),
                tok!(Ident "a", 24, 25),
                tok!(Of 25, 27),
                tok!(Int 1, 27, 28),
                tok!(Comma  28, 30),
                tok!(Int 10, 30, 31),
                tok!(Colon 31, 32),
                tok!(Ident "b", 32, 33),
                tok!(End 34, 37),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Case { branches, .. } = stmt else {
                panic!("expected Case Statement");
            };
            assert_eq!(branches.len(), 1);
            assert_eq!(branches[0].label_list.len(), 2);
            let label_1 = &branches[0].label_list[0];
            let label_2 = &branches[0].label_list[1];
            let Label::Single { value, ..} = label_1 else {
                panic!("expected Single label");
            };
            let LabelValue::Integer { value: 1, .. } = value else {
                panic!("expected Int label value");
            };
            let Label::Single { value, ..} = label_2 else {
                panic!("expected Single label");
            };
            let LabelValue::Integer { value: 10, .. } = value else {
                panic!("expected Int label value");
            };
        }

        #[test]
        fn parse_case_with_multiple_cases_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Case 20, 24),
                tok!(Ident "a", 24, 25),
                tok!(Of 25, 27),
                tok!(Int 1, 27, 28),
                tok!(Colon 28, 29),
                tok!(Ident "b", 29, 30),
                tok!(Pipe 30, 32),
                tok!(Int 10, 32, 33),
                tok!(Colon 33, 34),
                tok!(Ident "c", 35, 36),
                tok!(End 37, 40),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Case { branches, .. } = stmt else {
                panic!("expected Case Statement");
            };
            assert_eq!(branches.len(), 2);
            assert_eq!(branches[0].label_list.len(), 1);
            let label_1 = &branches[0].label_list[0];
            let Label::Single { value, ..} = label_1 else {
                panic!("expected Single label");
            };
            let LabelValue::Integer { value: 1, .. } = value else {
                panic!("expected Int label value");
            };
            let label_2 = &branches[1].label_list[0];
            let Label::Single { value, ..} = label_2 else {
                panic!("expected Single label");
            };
            let LabelValue::Integer { value: 10, .. } = value else {
                panic!("expected Int label value");
            };
        }

        #[test]
        fn parse_while_do_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(While 20, 22),
                tok!(True 23, 27),
                tok!(Do 27, 31),
                tok!(Ident "a", 32, 33),
                tok!(End 34, 37),

            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::While { cond, body, elsif_branches, .. } = stmt else {
                panic!("expected While Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(23, 27) });
            assert_eq!(body.len(), 1);
            assert!(elsif_branches.is_empty());
        }

        #[test]
        fn parse_while_do_elsif_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(While 20, 22),
                tok!(True 23, 27),
                tok!(Do 27, 31),
                tok!(Ident "a", 32, 33),
                tok!(Elsif 33, 36),
                tok!(False 37, 41),
                tok!(Do 41, 45),
                tok!(Ident "b", 46, 47),
                tok!(End 48, 50),

            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::While { cond, body, elsif_branches, .. } = stmt else {
                panic!("expected If Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(23, 27) });
            assert_eq!(body.len(), 1);
            assert_eq!(elsif_branches.len(), 1);
        }

        #[test]
        fn parse_repeat_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(Repeat  20, 22),
                tok!(Ident "a", 23, 27),
                tok!(Semi 27, 28),
                tok!(Ident "b", 29, 30),
                tok!(Until 30, 34),
                tok!(True 35, 39),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::Repeat { cond, body, .. } = stmt else {
                panic!("expected Repeat Statement");
            };
            assert_eq!(cond, Expression::Bool { value: true, span: Span::new(35, 39) });
            assert_eq!(body.len(), 2);
        }

        #[test]
        fn parse_for_to_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(For  20, 22),
                tok!(Ident "i", 23, 24),
                tok!(Assign 24, 25),
                tok!(Int 1, 25, 26),
                tok!(To 26, 28),
                tok!(Int 10, 29, 31),
                tok!(Do 31, 34),
                tok!(Ident "b", 29, 30),
                tok!(End 34, 37),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::For { var, low, high, by, body, .. } = stmt else {
                panic!("expected For Statement");
            };
            assert_eq!(var.text, "i");
            assert_eq!(low, Expression::Int { value: 1, span: Span::new(25, 26) });
            assert_eq!(high, Expression::Int { value: 10, span: Span::new(29, 31) });
            assert!(by.is_none());
            assert_eq!(body.len(), 1);
        }

        #[test]
        fn parse_for_to_by_statement() {
            let body = vec![
                tok!(Begin 14, 19),
                tok!(For  20, 22),
                tok!(Ident "i", 23, 24),
                tok!(Assign 24, 25),
                tok!(Int 1, 25, 26),
                tok!(To 26, 28),
                tok!(Int 10, 29, 31),
                tok!(By 31, 33),
                tok!(Int 2, 33, 34),
                tok!(Do 35, 37),
                tok!(Ident "b", 38, 39),
                tok!(End 40, 43),
            ];
            let tokens = module_tokens("monkey", "monkey2", body);
            let module = parse_module(tokens);

            assert_eq!(module.stmts.len(), 1);
            let stmt = module.stmts[0].clone();
            let Statement::For { var, low, high, by, body, .. } = stmt else {
                panic!("expected For Statement");
            };
            assert_eq!(var.text, "i");
            assert_eq!(low, Expression::Int { value: 1, span: Span::new(25, 26) });
            assert_eq!(high, Expression::Int { value: 10, span: Span::new(29, 31) });
            assert_eq!(by.unwrap(), Expression::Int { value: 2, span: Span::new(33, 34) });
            assert_eq!(body.len(), 1);
        }
    }
}

#[cfg(test)]
mod expr_tests {
    use super::Parser;
    use crate::ast::{BinaryOperation, Declaration, Designator, Expression, Selector};
    use crate::lexer::{Token, TokenKind};
    use crate::span::Span;

    // -------------------------
    // Token helpers (ONE style)
    // -------------------------

    fn t(kind: TokenKind, a: usize, b: usize) -> Token {
        Token::new(kind, Span::new(a, b))
    }
    fn id(name: &str, a: usize, b: usize) -> Token {
        t(TokenKind::Ident(name.to_string()), a, b)
    }
    fn int(v: i64, a: usize, b: usize) -> Token {
        t(TokenKind::Int(v), a, b)
    }

    // Wrap expression tokens into: MODULE M; CONST Foo = <expr>; END M.
    fn module_with_const_expr(expr_tokens: Vec<Token>) -> Vec<Token> {
        let mut v = vec![
            t(TokenKind::Module, 0, 0),
            id("M", 0, 0),
            t(TokenKind::SemiColon, 0, 0),

            t(TokenKind::Const, 0, 0),
            id("Foo", 0, 0),
            t(TokenKind::Equal, 0, 0),
        ];
        v.extend(expr_tokens);
        v.extend([
            t(TokenKind::SemiColon, 0, 0),
            t(TokenKind::End, 0, 0),
            id("M", 0, 0),
            t(TokenKind::Dot, 0, 0),
            t(TokenKind::Eof, 0, 0),
        ]);
        v
    }

    fn parse_const_expr(expr_tokens: Vec<Token>) -> Expression {
        let tokens = module_with_const_expr(expr_tokens);
        let mut p = Parser { tokens: &tokens, pos: 0 };

        let m = p.parse();
        let module = m.unwrap();

        let Declaration::Const { value, .. } = &module.declarations[0] else {
            panic!("expected Const declaration");
        };
        value.clone()
    }

    // -------------------------
    // AST shape helpers
    // -------------------------

    fn assert_int(e: &Expression, v: i64) {
        match e {
            Expression::Int { value, .. } => assert_eq!(*value, v),
            other => panic!("expected Int({v}), got {other:?}"),
        }
    }

    fn assert_bin<'a>(e: &'a Expression, op: BinaryOperation) -> (&'a Expression, &'a Expression) {
        match e {
            Expression::Binary { op: o, lhs, rhs, .. } if *o == op => (lhs.as_ref(), rhs.as_ref()),
            other => panic!("expected Binary({op:?}), got {other:?}"),
        }
    }

    fn assert_designator<'a>(e: &'a Expression) -> &'a Designator {
        match e {
            Expression::Designator(d) => d,
            other => panic!("expected Designator, got {other:?}"),
        }
    }

    // -------------------------
    // Precedence tests
    // -------------------------

    #[test]
    fn precedence_mul_binds_tighter_than_add() {
        // 1 + 2 * 3  => Add(1, Mul(2,3))
        let expr = parse_const_expr(vec![
            int(1, 0, 0),
            t(TokenKind::Plus, 0, 0),
            int(2, 0, 0),
            t(TokenKind::Star, 0, 0),
            int(3, 0, 0),
        ]);

        let (lhs, rhs) = assert_bin(&expr, BinaryOperation::Addition);
        assert_int(lhs, 1);

        let (r1, r2) = assert_bin(rhs, BinaryOperation::Multiplication);
        assert_int(r1, 2);
        assert_int(r2, 3);
    }

    #[test]
    fn parentheses_override_precedence() {
        // (1 + 2) * 3 => Mul(Add(1,2), 3)
        let expr = parse_const_expr(vec![
            t(TokenKind::LParen, 0, 0),
            int(1, 0, 0),
            t(TokenKind::Plus, 0, 0),
            int(2, 0, 0),
            t(TokenKind::RParen, 0, 0),
            t(TokenKind::Star, 0, 0),
            int(3, 0, 0),
        ]);

        let (lhs, rhs) = assert_bin(&expr, BinaryOperation::Multiplication);
        let (l1, l2) = assert_bin(lhs, BinaryOperation::Addition);
        assert_int(l1, 1);
        assert_int(l2, 2);
        assert_int(rhs, 3);
    }

    #[test]
    fn addops_are_left_associative() {
        // 10 - 3 - 2 => Sub(Sub(10,3),2)
        let expr = parse_const_expr(vec![
            int(10, 0, 0),
            t(TokenKind::Minus, 0, 0),
            int(3, 0, 0),
            t(TokenKind::Minus, 0, 0),
            int(2, 0, 0),
        ]);

        let (lhs, rhs) = assert_bin(&expr, BinaryOperation::Subtraction);
        let (l1, l2) = assert_bin(lhs, BinaryOperation::Subtraction);
        assert_int(l1, 10);
        assert_int(l2, 3);
        assert_int(rhs, 2);
    }

    // -------------------------
    // Relation: binds loosest + not chainable
    //
    // Enable when you add relation ops (Eq, Neq, Lt, Le, Gt, Ge, In, Is) to BinaryOperation
    // -------------------------

    #[test]
    fn relation_binds_looser_than_add_and_mul() {
        // 1 + 2 = 3 * 4 => Eq(Add(1,2), Mul(3,4))
        let expr = parse_const_expr(vec![
            int(1, 0, 0),
            t(TokenKind::Plus, 0, 0),
            int(2, 0, 0),
            t(TokenKind::Equal, 0, 0),
            int(3, 0, 0),
            t(TokenKind::Star, 0, 0),
            int(4, 0, 0),
        ]);

        let (lhs, rhs) = assert_bin(&expr, BinaryOperation::Eq);

        let (a, b) = assert_bin(lhs, BinaryOperation::Addition);
        assert_int(a, 1);
        assert_int(b, 2);

        let (c, d) = assert_bin(rhs, BinaryOperation::Multiplication);
        assert_int(c, 3);
        assert_int(d, 4);
    }

    #[test]
    fn relation_is_not_chainable() {
        // expression = SimpleExpression [relation SimpleExpression].
        // 1 < 2 < 3 should be an error (once you parse 1<2, the next < is unexpected)
        let tokens = module_with_const_expr(vec![
            int(1, 0, 0),
            t(TokenKind::LessThan, 0, 0), // rename to your token
            int(2, 0, 0),
            t(TokenKind::LessThan, 0, 0),
            int(3, 0, 0),
        ]);

        let mut p = Parser { tokens: &tokens, pos: 0 };
        assert!(p.parse().is_err());
    }

    // -------------------------
    // Set + element ranges
    // -------------------------

    #[test]
    fn set_with_range_elements() {
        // {1, 2..3}
        let expr = parse_const_expr(vec![
            t(TokenKind::LCurly, 0, 0),
            int(1, 0, 0),
            t(TokenKind::Comma, 0, 0),
            int(2, 0, 0),
            t(TokenKind::DotDot, 0, 0),
            int(3, 0, 0),
            t(TokenKind::RCurly, 0, 0),
        ]);

        let Expression::Set { elements, .. } = expr else {
            panic!("expected Set expression");
        };

        assert_eq!(elements.len(), 2);
        assert_int(&elements[0].first, 1);
        assert!(elements[0].second.is_none());
        assert_int(&elements[1].first, 2);
        assert_int(elements[1].second.as_ref().unwrap(), 3);
    }

    // -------------------------
    // Designator: selectors + precedence inside index/call
    // -------------------------

    #[test]
    fn designator_with_index_field_call_deref() {
        // M.B[i + 1, 2].Bar(10)^
        //
        // head parts: ["M","B"]
        // selectors:  Index([Add(i,1), 2]), Field("Bar"), Call([10]), Deref
        let expr = parse_const_expr(vec![
            id("M", 0, 0),
            t(TokenKind::Dot, 0, 0),
            id("B", 0, 0),

            t(TokenKind::LSquare, 0, 0), // rename to your token
            id("i", 0, 0),
            t(TokenKind::Plus, 0, 0),
            int(1, 0, 0),
            t(TokenKind::Comma, 0, 0),
            int(2, 0, 0),
            t(TokenKind::RSquare, 0, 0),

            t(TokenKind::Dot, 0, 0),
            id("Bar", 0, 0),

            t(TokenKind::LParen, 0, 0),
            int(10, 0, 0),
            t(TokenKind::RParen, 0, 0),

            t(TokenKind::Caret, 0, 0), // rename to your token for '^'
        ]);

        let d = assert_designator(&expr);

        assert_eq!(d.head.parts.len(), 2);
        assert_eq!(d.head.parts[0].text, "M");
        assert_eq!(d.head.parts[1].text, "B");

        assert_eq!(d.selectors.len(), 4);

        // Index(...)
        let Selector::Index(ix) = &d.selectors[0] else { panic!("expected Index selector"); };
        assert_eq!(ix.len(), 2);

        // ix[0] should be Add(i,1)
        let (l, r) = assert_bin(&ix[0], BinaryOperation::Addition);
        let di = assert_designator(l);
        assert_eq!(di.head.parts.len(), 1);
        assert_eq!(di.head.parts[0].text, "i");
        assert_int(r, 1);

        assert_int(&ix[1], 2);

        // Field(Bar)
        assert!(matches!(&d.selectors[1], Selector::Field(fid) if fid.text == "Bar"));

        // Call(10)
        let Selector::Call(args, _call_span) = &d.selectors[2] else { panic!("expected Call selector"); };
        assert_eq!(args.len(), 1);
        assert_int(&args[0], 10);

        // Deref
        assert!(matches!(&d.selectors[3], Selector::Deref(_)));
    }
}
