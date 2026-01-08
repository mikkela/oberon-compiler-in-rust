use crate::span::Span;

// -------------------------
// Common
// -------------------------

pub trait Spanned {
    fn span(&self) -> Span;
}

// -------------------------
// Identifiers
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub text: String,
    pub span: Span,
}

impl Spanned for Identifier {
    fn span(&self) -> Span { self.span }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierDef {
    pub ident: Identifier,
    pub exported: bool, // star
    pub span: Span,
}

impl Spanned for IdentifierDef {
    fn span(&self) -> Span { self.span }
}

/// Instead of (Option<Identifier>, Identifier), we store the parts.
/// In classic Oberon you’ll typically have 1 or 2 parts, but this scales.
#[derive(Clone, Debug, PartialEq)]
pub struct QualifiedIdentifier {
    pub parts: Vec<Identifier>, // len >= 1
}

impl QualifiedIdentifier {
    pub fn new(parts: Vec<Identifier>) -> Self {
        debug_assert!(!parts.is_empty());
        Self { parts }
    }

    pub fn first(&self) -> Option<&Identifier> {
        if self.parts.len() >= 2 { Some(&self.parts[0]) } else { None }
    }

    pub fn last(&self) -> &Identifier {
        self.parts.last().unwrap()
    }
}

impl Spanned for QualifiedIdentifier {
    fn span(&self) -> Span {
        let start = self.parts.first().unwrap().span.start;
        let end = self.parts.last().unwrap().span.end;
        Span::new(start, end)
    }
}

// -------------------------
// Module / Imports
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: Identifier,
    pub end_name: Identifier,
    pub import_list: Vec<Import>,
    pub declarations: Vec<Declaration>,
    pub stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub module: Identifier,
    pub alias: Option<Identifier>,
    pub span: Span,
}

// -------------------------
// Declarations
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Const { ident: IdentifierDef, value: Expression, span: Span },
    Type  { ident: IdentifierDef, ty: Type, span: Span },

    // Keep placeholders (you can evolve them later)
    Var   { ident: IdentifierDef, ty: QualifiedIdentifier, span: Span },
    Proc  { ident: IdentifierDef, params: Vec<Parameter>, ret: Option<QualifiedIdentifier>, body: Vec<Statement>, span: Span },
}

impl Spanned for Declaration {
    fn span(&self) -> Span {
        match self {
            Declaration::Const { span, .. } => *span,
            Declaration::Type  { span, .. } => *span,
            Declaration::Var   { span, .. } => *span,
            Declaration::Proc  { span, .. } => *span,
        }
    }
}

// -------------------------
// Types
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct FieldList {
    pub fields: Vec<IdentifierDef>,
    pub ty: Type,
}

impl Spanned for FieldList {
    fn span(&self) -> Span {
        let start = self.fields.first().unwrap().span.start;
        let end = self.ty.span().end;
        Span::new(start, end)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Named   { name: QualifiedIdentifier, span: Span },
    Array   { lengths: Vec<Expression>, element: Box<Type>, span: Span },
    Record  { base: Option<QualifiedIdentifier>, field_lists: Vec<FieldList>, span: Span },
    Pointer { pointee: Box<Type>, span: Span },
    Procedure { params: Option<FormalParameters>, span: Span },
}

impl Spanned for Type {
    fn span(&self) -> Span {
        match self {
            Type::Named { span, .. } => *span,
            Type::Array { span, .. } => *span,
            Type::Record { span, .. } => *span,
            Type::Pointer { span, .. } => *span,
            Type::Procedure { span, .. } => *span,
        }
    }
}

/// FormalType = {ARRAY OF} qualident
#[derive(Clone, Debug, PartialEq)]
pub struct FormalType {
    pub open_arrays: usize,            // number of leading "ARRAY OF"
    pub base: QualifiedIdentifier,     // qualident
    pub span: Span,
}
impl Spanned for FormalType { fn span(&self) -> Span { self.span } }

/// FPSection = [VAR] ident {"," ident} ":" FormalType
#[derive(Clone, Debug, PartialEq)]
pub struct FPSection {
    pub by_ref: bool,                  // VAR present
    pub names: Vec<Identifier>,      // ident list (optionally exported if you allow it)
    pub ty: FormalType,
    pub span: Span,
}
impl Spanned for FPSection { fn span(&self) -> Span { self.span } }

/// FormalParameters = "(" [FPSection {";" FPSection}] ")" [":" qualident]
#[derive(Clone, Debug, PartialEq)]
pub struct FormalParameters {
    pub sections: Vec<FPSection>,              // empty allowed
    pub return_type: Option<QualifiedIdentifier>,
    pub span: Span,                             // from '(' to end of return type (if any)
}
impl Spanned for FormalParameters { fn span(&self) -> Span { self.span } }

// -------------------------
// Statements
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: IdentifierDef,
    pub ty: QualifiedIdentifier,
    pub span: Span,
}

impl Spanned for Parameter {
    fn span(&self) -> Span { self.span }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assign { target: Designator, value: Expression, span: Span },
    Call   { callee: Designator, span: Span },
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Assign { span, .. } => *span,
            Statement::Call   { span, .. } => *span,
        }
    }
}

// -------------------------
// Designators & selectors
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Designator {
    pub head: QualifiedIdentifier,
    pub selectors: Vec<Selector>, // 0..n
    pub span: Span,
}

impl Spanned for Designator {
    fn span(&self) -> Span { self.span }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Selector {
    Field(Identifier),          // .x
    Index(Vec<Expression>),     // [e1, e2]
    Deref(Span),                // ^
    Call(Vec<Expression>, Span), // (args)
    TypeGuard(QualifiedIdentifier, Span), // (ident)
}

impl Spanned for Selector {
    fn span(&self) -> Span {
        match self {
            Selector::Field(id) => id.span(),
            Selector::Index(exprs) => {
                // you might want to store explicit span; this is “best effort”
                let start = exprs.first().map(|e| e.span().start).unwrap_or(0);
                let end   = exprs.last().map(|e| e.span().end).unwrap_or(0);
                Span::new(start, end)
            }
            Selector::Deref(span) => *span,
            Selector::Call(_, span) => *span,
            Selector::TypeGuard(_, span) => *span,
        }
    }
}

// -------------------------
// Expressions
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    pub first: Expression,
    pub second: Option<Expression>,
    pub span: Span,
}

impl Spanned for Element {
    fn span(&self) -> Span { self.span }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Int    { value: i64, span: Span },
    Real   { value: f64, span: Span },
    String { value: String, span: Span },
    Bool   { value: bool, span: Span },
    Nil    { span: Span },

    Set { elements: Vec<Element>, span: Span },

    Designator(Designator),

    Unary { op: UnaryOperation, operand: Box<Expression>, span: Span },
    Binary { op: BinaryOperation, lhs: Box<Expression>, rhs: Box<Expression>, span: Span },
}

impl Spanned for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Int { span, .. } => *span,
            Expression::Real { span, .. } => *span,
            Expression::String { span, .. } => *span,
            Expression::Bool { span, .. } => *span,
            Expression::Nil { span } => *span,
            Expression::Set { span, .. } => *span,
            Expression::Designator(d) => d.span(),
            Expression::Binary { span, .. } => *span,
            Expression::Unary { span, .. } => *span,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperation {
    Not, Plus, Minus
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperation {
    Addition, Subtraction, Multiplication, Division, Mod, Div, And, Or,
    Eq, Neq, Lt, Le, Gt, Ge, In, Is
}
