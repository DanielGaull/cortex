use super::{expression::{Expression, OptionalIdentifier, Parameter}, statement::Statement, typ::CType};

pub enum TopLevel {
    Import {
        name: String, 
        is_string_import: bool,
    },
    Module {
        name: String,
        contents: Vec<TopLevel>,
    },
    Function {
        name: OptionalIdentifier,
        params: Vec<Parameter>,
        return_type: CType,
        body: Body,
    },
}

pub struct Body {
    statements: Vec<Statement>,
    result: Option<Expression>,
}
