use crate::parsing::ast::expression::{BinaryOperator, UnaryOperator};

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
        is_heap_allocated: Option<bool>,
    },
    // TODO: if statement
    // IfStatement {
    //     first: Box<ConditionBody>,
    //     conds: Vec<ConditionBody>,
    //     last: Option<Box<BasicBody>>,
    // },
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
