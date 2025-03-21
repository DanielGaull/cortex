use super::expression::RExpression;

pub enum RStatement {
    Expression(RExpression),
    Throw(RExpression),
    VariableDeclaration {
        name: String,
        is_const: bool,
        initial_value: RExpression,
    },
    // TODO: add these
    // Assignment {
    //     name: IdentExpression,
    //     value: RExpression,
    // },
    // WhileLoop(ConditionBody),
}
