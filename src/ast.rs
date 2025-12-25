use crate::span::Span;

#[derive(Clone, Debug)]
pub struct Module {
    pub name: String,
    pub decls: Vec<Declaration>,
    pub stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    Const { name: String, value: Expression, span: Span },
    Var { name: String, ty: TypeReference, span: Span },
    Proc { name: String, params: Vec<Parameter>, ret: Option<TypeReference>, body: Vec<Statement>, span: Span },
}

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeReference,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assign { target: String, value: Expression, span: Span },
    Call { name: String, args: Vec<Expression>, span: Span },
    Return { value: Option<Expression>, span: Span },
}

#[derive(Clone, Debug)]
pub enum Expression {
    Int(i64, Span),
    Ident(String, Span),
    Binary { op: BinaryOperation, lhs: Box<Expression>, rhs: Box<Expression>, span: Span },
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperation {
    Add, Sub, Mul, Div,
}

#[derive(Clone, Debug)]
pub struct TypeReference {
    pub name: String,
    pub span: Span,
}