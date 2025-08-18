use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::{expression::{RCodeGen, RExpression, RIdentExpression}, function::RDefinedBody};

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
impl RCodeGen for RStatement {
    fn codegen(&self, indent: usize, preprocessor: &crate::preprocessing::preprocessor::preprocessor::CortexPreprocessor) -> String {
        match self {
            RStatement::Expression(exp) => format!("{};", exp.codegen(indent, preprocessor)),
            RStatement::Throw(exp) => {
                if let Some(exp) = exp {
                    format!("throw {};", exp.codegen(indent, preprocessor))
                } else {
                    String::from("throw;")
                }
            },
            RStatement::VariableDeclaration { name, is_const, initial_value } => 
                format!("{} {} = {};", if *is_const { "const" } else { "let" }, name, initial_value.codegen(indent, preprocessor)),
            RStatement::Assignment { name, value } =>
                format!("{} = {};", name.codegen(indent), value.codegen(indent, preprocessor)),
            RStatement::WhileLoop(body) => {
                let mut s = String::new();
                s.push_str("while ");
                s.push_str(&body.condition.codegen(indent, preprocessor));
                s.push_str(" { \n");
                s.push_str(&body.body.codegen(indent + 1, preprocessor));
                s.push_str("}");
                s
            },
            RStatement::Break => String::from("break;"),
            RStatement::Continue => String::from("continue;"),
        }
    }
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
