use crate::parsing::ast::expression::{BinaryOperator, IdentExpression, UnaryOperator};

use super::{function::RInterpretedBody, statement::RConditionBody};

pub enum RExpression {
    Number(f64),
    Boolean(bool),
    Void,
    None,
    String(String),
    Char(u8),
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
    Tuple(Vec<RExpression>),
}

pub struct RIdentExpression {
    pub(crate) base: String,
    pub(crate) chain: Vec<String>,
}
impl From<IdentExpression> for RIdentExpression {
    fn from(value: IdentExpression) -> Self {
        RIdentExpression {
            base: value.base,
            chain: value.chain,
        }
    }
}
impl RIdentExpression {
    pub fn is_simple(&self) -> bool {
        self.chain.is_empty()
    }
}
