use super::{expression::{RExpression, RIdentExpression}, function::RInterpretedBody};

pub enum RStatement {
    Expression(RExpression),
    Throw(RExpression),
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
}

pub struct RConditionBody {
    pub(crate) condition: RExpression,
    pub(crate) body: RInterpretedBody,
}
impl RConditionBody {
    pub fn new(condition: RExpression, body: RInterpretedBody) -> Self {
        RConditionBody {
            condition,
            body,
        }
    }
}
