use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
    pub stmts: Vec<Statement>
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub name: String,
    pub imported_name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Const { name: String, value: Expression, public: bool },
    Var { name: String, ty: TypeReference, span: Span },
    Proc { name: String, params: Vec<Parameter>, ret: Option<TypeReference>, body: Vec<Statement>, span: Span },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeReference,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assign { target: String, value: Expression, span: Span },
    Call { name: String, args: Vec<Expression>, span: Span },
    Return { value: Option<Expression>, span: Span },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Int(i64, Span),
    Ident(String, Span),
    Binary { op: BinaryOperation, lhs: Box<Expression>, rhs: Box<Expression>, span: Span },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperation {
    Add, Sub, Mul, Div,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeReference {
    pub name: String,
    pub span: Span,
}