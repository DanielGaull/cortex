use crate::parsing::ast::expression::{BinaryOperator, UnaryOperator};

use super::{function::RInterpretedBody, statement::RConditionBody};

pub enum RExpression {
    Number(f64),
    Boolean(bool),
    Void,
    None,
    String(String),
    Identifier(String),
    Call(usize, Vec<RExpression>),
    Construction {
        assignments: Vec<(String, RExpression)>,
        is_heap_allocated: bool,
    },
    IfStatement {
        first: Box<RConditionBody>,
        conds: Vec<RConditionBody>,
        last: Option<Box<RInterpretedBody>>,
    },
    UnaryOperation {
        op: UnaryOperator,
        exp: Box<RExpression>,
    },
    ListLiteral(Vec<RExpression>),
    Bang(Box<RExpression>),
    MemberAccess(Box<RExpression>, String),
    BinaryOperation {
        left: Box<RExpression>,
        op: BinaryOperator,
        right: Box<RExpression>,
    },
}

pub struct RIdentExpression {
    pub(crate) base: String,
    pub(crate) chain: Vec<String>,
}
