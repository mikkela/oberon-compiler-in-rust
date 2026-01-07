use crate::ast::{BinaryOperation, FieldList, Spanned, UnaryOperation};
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
            ident: name,
            span,
            exported: star_tok.is_some(),
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

        self.expect(TokenKind::End)?;
        let second_ident = self.expect_ident()?;
        let end = self.expect(TokenKind::Dot)?;

        Ok(Module {
            name: first_ident,
            import_list,
            stmts: vec![],
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

            TokenKind::Record => {
                let start = self.expect(TokenKind::Record)?.span.start;
                let base =
                    if self.at(&TokenKind::LParen) {
                        self.bump()?;
                        let base = self.parse_named_type()?;
                        self.expect(TokenKind::RParen)?;
                        Some( Box::new(base))
                    } else {
                        None
                    };
                let mut field_lists = vec![self.parse_field_list()?];
                while self.at(&TokenKind::SemiColon) {
                    self.bump()?;
                    field_lists.push(self.parse_field_list()?);
                };
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

            _ => Err(ParserError::UnexpectedToken { token: peek }),
        }
    }

    fn parse_field_list(&mut self) -> Result<FieldList, ParserError> {
        let mut fields = vec![self.parse_identifier_def()?];
        while self.at(&TokenKind::Comma) {
            self.bump()?;
            fields.push(self.parse_identifier_def()?);
        };
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
                self.parse_designator_or_error(first)
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

        let elements = self.comma_list_until(TokenKind::RCurly, |p| p.parse_element())?;
        let rcurly = self.expect(TokenKind::RCurly)?;

        Ok(Expression::Set {
            elements,
            span: Span::new(lcurly.span.start, rcurly.span.end),
        })
    }

    fn parse_designator_or_error(&mut self, first: Identifier) -> Result<Expression, ParserError> {
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

        Ok(crate::ast::Expression::Designator(crate::ast::Designator {
            head,
            selectors,
            span,
        }))
    }

    fn parse_index_selector(&mut self) -> Result<Option<crate::ast::Selector>, ParserError> {
        if !self.at(&TokenKind::LSquare) {
            return Ok(None);
        }

        self.bump()?; // '['

        // (Oberon) index kan være flere udtryk adskilt af comma: a[i, j]
        let mut exprs = vec![];
        if !self.at(&TokenKind::RSquare) {
            exprs.push(self.parse_expression()?);
            while self.eat(TokenKind::Comma)?.is_some() {
                exprs.push(self.parse_expression()?);
            }
        }

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
        let args = self.comma_list_until(TokenKind::RParen, |p| p.parse_expression())?;
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

    (Ident $s:literal, $a:expr, $b:expr) => { ident($s, $a, $b) };
    (Int   $v:expr,    $a:expr, $b:expr) => { int($v, $a, $b) };

    (Semi   $a:expr, $b:expr) => { t(TokenKind::SemiColon, $a, $b) };
    (Comma  $a:expr, $b:expr) => { t(TokenKind::Comma,    $a, $b) };
    (Assign $a:expr, $b:expr) => { t(TokenKind::Assign,   $a, $b) };
    (Equal  $a:expr, $b:expr) => { t(TokenKind::Equal,    $a, $b) };
    (Star   $a:expr, $b:expr) => { t(TokenKind::Star,     $a, $b) };
    (Dot    $a:expr, $b:expr) => { t(TokenKind::Dot,      $a, $b) };
    (DotDot $a:expr, $b:expr) => { t(TokenKind::DotDot,   $a, $b) };
    (Colon  $a:expr, $b:expr) => { t(TokenKind::Colon,    $a, $b) };

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
        let base_type = base.as_ref().unwrap();

        let Type::Named { name , .. } = base_type.deref() else {
            panic!("expected Named type");
        };
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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // x(T)
            Token::new(TokenKind::Ident("x".to_string()), Span::new(25, 26)),
            Token::new(TokenKind::LParen, Span::new(26, 27)),
            Token::new(TokenKind::Ident("T".to_string()), Span::new(27, 28)),
            Token::new(TokenKind::RParen, Span::new(28, 29)),
            Token::new(TokenKind::SemiColon, Span::new(29, 30)),
            Token::new(TokenKind::End, Span::new(30, 33)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(34, 40)),
            Token::new(TokenKind::Dot, Span::new(40, 41)),
            Token::new(TokenKind::Eof, Span::new(41, 41)),
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // x(M.T)
            Token::new(TokenKind::Ident("x".to_string()), Span::new(25, 26)),
            Token::new(TokenKind::LParen, Span::new(26, 27)),
            Token::new(TokenKind::Ident("M".to_string()), Span::new(27, 28)),
            Token::new(TokenKind::Dot, Span::new(28, 29)),
            Token::new(TokenKind::Ident("T".to_string()), Span::new(29, 30)),
            Token::new(TokenKind::RParen, Span::new(30, 31)),
            Token::new(TokenKind::SemiColon, Span::new(31, 32)),
            Token::new(TokenKind::End, Span::new(32, 35)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(36, 42)),
            Token::new(TokenKind::Dot, Span::new(42, 43)),
            Token::new(TokenKind::Eof, Span::new(43, 43)),
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // x(1)  -> MUST be Call, not TypeGuard
            Token::new(TokenKind::Ident("x".to_string()), Span::new(25, 26)),
            Token::new(TokenKind::LParen, Span::new(26, 27)),
            Token::new(TokenKind::Int(1), Span::new(27, 28)),
            Token::new(TokenKind::RParen, Span::new(28, 29)),
            Token::new(TokenKind::SemiColon, Span::new(29, 30)),
            Token::new(TokenKind::End, Span::new(30, 33)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(34, 40)),
            Token::new(TokenKind::Dot, Span::new(40, 41)),
            Token::new(TokenKind::Eof, Span::new(41, 41)),
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // x(T)(1)
            Token::new(TokenKind::Ident("x".to_string()), Span::new(25, 26)),
            Token::new(TokenKind::LParen, Span::new(26, 27)),
            Token::new(TokenKind::Ident("T".to_string()), Span::new(27, 28)),
            Token::new(TokenKind::RParen, Span::new(28, 29)),
            Token::new(TokenKind::LParen, Span::new(29, 30)),
            Token::new(TokenKind::Int(1), Span::new(30, 31)),
            Token::new(TokenKind::RParen, Span::new(31, 32)),
            Token::new(TokenKind::SemiColon, Span::new(32, 33)),
            Token::new(TokenKind::End, Span::new(33, 36)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(37, 43)),
            Token::new(TokenKind::Dot, Span::new(43, 44)),
            Token::new(TokenKind::Eof, Span::new(44, 44)),
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // x^.y
            Token::new(TokenKind::Ident("x".to_string()), Span::new(25, 26)),
            Token::new(TokenKind::LSquare, Span::new(26, 27)),
            Token::new(TokenKind::Int(1), Span::new(27, 28)),
            Token::new(TokenKind::RSquare, Span::new(28, 29)),
            Token::new(TokenKind::LParen, Span::new(29, 30)),
            Token::new(TokenKind::Int(2), Span::new(30, 31)),
            Token::new(TokenKind::RParen, Span::new(31, 32)),
            Token::new(TokenKind::Caret, Span::new(32, 33)),
            Token::new(TokenKind::Dot, Span::new(33, 34)),
            Token::new(TokenKind::Ident("y".to_string()), Span::new(34, 35)),
            Token::new(TokenKind::SemiColon, Span::new(35, 36)),
            Token::new(TokenKind::End, Span::new(36, 39)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(40, 46)),
            Token::new(TokenKind::Dot, Span::new(46, 47)),
            Token::new(TokenKind::Eof, Span::new(47, 47)),
        ];

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
                assert_eq!(id.span, Span::new(34, 35));
            }
            other => panic!("expected Field selector, got {other:?}"),
        }
    }

    #[test]
    fn parse_tilde_expression() {
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // ~TRUE
            Token::new(TokenKind::Tilde, Span::new(25, 26)),
            Token::new(TokenKind::True, Span::new(26, 29)),

            Token::new(TokenKind::SemiColon, Span::new(29, 30)),
            Token::new(TokenKind::End, Span::new(30, 33)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(34, 40)),
            Token::new(TokenKind::Dot, Span::new(40, 41)),
            Token::new(TokenKind::Eof, Span::new(41, 41))
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // (TRUE)
            Token::new(TokenKind::LParen, Span::new(25, 26)),
            Token::new(TokenKind::True, Span::new(26, 29)),
            Token::new(TokenKind::RParen, Span::new(29, 30)),

            Token::new(TokenKind::SemiColon, Span::new(30, 31)),
            Token::new(TokenKind::End, Span::new(31, 34)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(35, 41)),
            Token::new(TokenKind::Dot, Span::new(41, 42)),
            Token::new(TokenKind::Eof, Span::new(42, 42))
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // TRUE & FALSE
            Token::new(TokenKind::True, Span::new(25, 28)),
            Token::new(TokenKind::Ampersand, Span::new(28, 29)),
            Token::new(TokenKind::False, Span::new(30, 34)),

            Token::new(TokenKind::SemiColon, Span::new(34, 35)),
            Token::new(TokenKind::End, Span::new(36, 39)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(40, 46)),
            Token::new(TokenKind::Dot, Span::new(46, 47)),
            Token::new(TokenKind::Eof, Span::new(47, 47))
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // TRUE & FALSE
            Token::new(TokenKind::True, Span::new(25, 28)),
            Token::new(TokenKind::Or, Span::new(28, 29)),
            Token::new(TokenKind::False, Span::new(30, 34)),

            Token::new(TokenKind::SemiColon, Span::new(34, 35)),
            Token::new(TokenKind::End, Span::new(36, 39)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(40, 46)),
            Token::new(TokenKind::Dot, Span::new(46, 47)),
            Token::new(TokenKind::Eof, Span::new(47, 47))
        ];

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
        let tokens = vec![
            Token::new(TokenKind::Module, Span::new(0, 6)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(7, 13)),
            Token::new(TokenKind::SemiColon, Span::new(13, 14)),
            Token::new(TokenKind::Const, Span::new(14, 19)),
            Token::new(TokenKind::Ident("Foo".to_string()), Span::new(20, 23)),
            Token::new(TokenKind::Equal, Span::new(23, 24)),

            // TRUE # FALSE
            Token::new(TokenKind::True, Span::new(25, 28)),
            Token::new(TokenKind::NotEqual, Span::new(28, 29)),
            Token::new(TokenKind::False, Span::new(30, 34)),

            Token::new(TokenKind::SemiColon, Span::new(34, 35)),
            Token::new(TokenKind::End, Span::new(36, 39)),
            Token::new(TokenKind::Ident("monkey".to_string()), Span::new(40, 46)),
            Token::new(TokenKind::Dot, Span::new(46, 47)),
            Token::new(TokenKind::Eof, Span::new(47, 47))
        ];

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
