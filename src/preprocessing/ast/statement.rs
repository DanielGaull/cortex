use super::{expression::{RExpression, RIdentExpression}, function::RDefinedBody};

#[derive(Clone)]
pub enum RStatement {
    Expression(RExpression),
    Throw(Option<RExpression>),
    VariableDeclaration {
        name: String,
        is_const: bool,
        initial_value: RExpression,
    },
    Assignment {
        name: RIdentExpression,
        value: RExpression,
    },
    WhileLoop(RConditionBody),
    Break,
    Continue,
}

#[derive(Clone)]
pub struct RConditionBody {
    pub(crate) condition: RExpression,
    pub(crate) body: RDefinedBody,
}
impl RConditionBody {
    pub fn new(condition: RExpression, body: RDefinedBody) -> Self {
        RConditionBody {
            condition,
            body,
        }
    }
}
