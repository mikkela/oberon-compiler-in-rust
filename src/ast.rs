use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Identifier {
    pub identifier: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierDef {
    pub identifier: Identifier,
    pub star: bool,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QualifiedIdentifier {
    pub first_ident: Option<Identifier>,
    pub second_ident: Identifier,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    pub first_expression: Box<Expression>,
    pub second_expression: Option<Box<Expression>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Selector {
    Qualify { name: Identifier, span: Span },
    Index { expressions: Vec<Box<Expression>>, span: Span },
    Dereference { span: Span },
    Subscription { parameters: Vec<QualifiedIdentifier>, span: Span },
}

impl Selector {
    pub fn span(&self) -> Span {
        match self {
            Selector::Qualify { span, .. } => *span,
            Selector::Index { span, .. } => *span,
            Selector::Dereference { span, .. } => *span,
            Selector::Subscription { span, .. } => *span,
        }
    }

    pub fn is_subscription(&self) -> bool {
        matches!(self, Selector::Subscription { .. })
    }

    pub fn as_subscription(&self) -> Option<&Vec<QualifiedIdentifier>> {
        match self {
            Selector::Subscription { parameters, .. } => Some(parameters),
            _ => None,
        }
    }

    pub fn is_qualified(&self) -> bool {
        matches!(self, Selector::Qualify { .. })
    }

    pub fn as_qualified(&self) -> Option<&Identifier> {
        match self {
            Selector::Qualify { name, .. } => Some(name),
            _ => None,
        }
    }

    pub fn is_index(&self) -> bool {
        matches!(self, Selector::Index { .. })
    }

    pub fn as_index(&self) -> Option<&Vec<Box<Expression>>> {
        match self {
            Selector::Index { expressions, .. } => Some(expressions),
            _ => None,
        }
    }

    pub fn is_dereference(&self) -> bool {
        matches!(self, Selector::Dereference { .. })
    }
    pub fn as_dereference(&self) -> Option<()> {
        match self {
            Selector::Dereference { .. } => Some(()),
            _ => None,
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub first_ident: Identifier,
    pub second_ident: Identifier,
    pub import_list: Vec<Import>,
    pub declaration_sequence: Vec<Declaration>,
    pub stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    pub first_ident: Identifier,
    pub second_ident: Option<Identifier>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Const { ident: IdentifierDef, expression: Expression, span: Span },
    Type { ident: IdentifierDef, ty: Type, span: Span },
    Var { name: String, ty: TypeReference, span: Span },
    Proc { name: String, params: Vec<Parameter>, ret: Option<TypeReference>, body: Vec<Statement>, span: Span },
}

impl Declaration {
    pub fn is_const(&self) -> bool {
        matches!(self, Declaration::Const { .. })
    }

    pub fn as_const(&self) -> Option<(&IdentifierDef, &Expression)> {
        match self {
            Declaration::Const { ident, expression, .. } =>
                Some((ident, expression)),
            _ => None,
        }
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Declaration::Type { .. })
    }

    pub fn as_type(&self) -> Option<(&IdentifierDef, &Type)> {
        match self {
            Declaration::Type { ident, ty, .. } =>
                Some((ident, ty)),
            _ => None,
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FieldList {
    pub fields: Vec<IdentifierDef>,
    pub ty: Type,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Named { name: QualifiedIdentifier, span: Span },
    Array { lengths: Vec<Expression>, element: Box<Type>, span: Span },
    Record { base: Option<QualifiedIdentifier>, field_lists: Vec<FieldList>, span: Span },
    Pointer { pointee: Box<Type>, span: Span },
}

impl Type {
    pub fn span(&self) -> Span {
        if self.is_array() {
            self.as_array().unwrap().1.span()
        } else if self.is_named() {
            self.as_named().unwrap().span
        }
        else {
            Span::new(0, 0)
        }
    }
    pub fn is_named(&self) -> bool {
        matches!(self, Type::Named { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }

    pub fn as_named(&self) -> Option<&QualifiedIdentifier> {
        match self {
            Type::Named { name, .. } => Some(name),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<(&[Expression], &Box<Type>)> {
        match self {
            Type::Array { lengths, element, .. } => Some((lengths, element)),
            _ => None,
        }
    }
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
pub struct ActualParameters {
    pub parameters: Vec<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Int { value: i64, span: Span},
    Real { value: f64, span: Span},
    String { value: String, span: Span},
    Bool { value: bool, span: Span},
    Nil { span: Span},
    Set { value: Vec<Box<Element>>, span: Span},
    Designator {
        target: QualifiedIdentifier,
        selector: Selector,
        parameters: Option<ActualParameters>,
        span: Span
    },
    //Ident(String, Span),
    Binary { op: BinaryOperation, lhs: Box<Expression>, rhs: Box<Expression>, span: Span },
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Int { span, .. } => *span,
            Expression::Real { span, .. } => *span,
            Expression::String { span, .. } => *span,
            Expression::Bool { span, .. } => *span,
            Expression::Nil { span } => *span,
            Expression::Set { span, .. } => *span,
            Expression::Binary { span, .. } => *span,
            Expression::Designator { span, .. } => *span,
        }
    }

    pub fn is_set(&self) -> bool {
        matches!(self, Expression::Set { .. })
    }

    pub fn as_set(&self) -> Option<&Vec<Box<Element>>> {
        match self {
            Expression::Set { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn is_designator(&self) -> bool {
        matches!(self, Expression::Designator { .. })
    }

    pub fn as_designator(&self) -> Option<(&QualifiedIdentifier, &Selector, &Option<ActualParameters>)> {
        match self {
            Expression::Designator { target, selector, parameters, .. } => Some((target, selector, parameters)),
            _ => None,
        }
    }

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