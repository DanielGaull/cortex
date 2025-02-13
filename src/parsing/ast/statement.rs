use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::{expression::{BinaryOperator, ConditionBody, Expression, IdentExpression, OptionalIdentifier}, r#type::CortexType};

#[derive(Clone)]
pub enum Statement {
    Expression(Expression),
    Stop,
    VariableDeclaration {
        name: OptionalIdentifier,
        is_const: bool,
        typ: Option<CortexType>,
        initial_value: Expression,
    },
    VariableAssignment {
        name: IdentExpression,
        value: Expression,
        op: Option<BinaryOperator>,
    },
    WhileLoop(ConditionBody),
}
impl SimpleCodeGen for Statement {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);
        s.push_str(&indent_prefix);
        let mut semicolon = true;
        match self {
            Self::Expression(expr) => {
                s.push_str(&expr.codegen(indent));
            },
            Self::Stop => {
                s.push_str("stop");
            },
            Self::VariableDeclaration { name, is_const, typ, initial_value } => {
                if *is_const {
                    s.push_str("const ");
                } else {
                    s.push_str("let ");
                }
                s.push_str(&name.codegen(indent));
                if let Some(given_type) = typ {
                    s.push_str(": ");
                    s.push_str(&given_type.codegen(indent));
                }
                s.push_str(" = ");
                s.push_str(&initial_value.codegen(indent));
            },
            Self::VariableAssignment { name, value, op } => {
                s.push_str(&name.codegen(indent));
                s.push_str(" ");
                if let Some(binop) = op {
                    s.push_str(&binop.codegen(indent));
                }
                s.push_str("= ");
                s.push_str(&value.codegen(indent));
            },
            Self::WhileLoop(cond_body) => {
                semicolon = false;
                s.push_str("while ");
                s.push_str(&cond_body.condition.codegen(indent));
                s.push_str(" {\n");
                s.push_str(&cond_body.body.codegen(indent + 1));
                s.push_str("}");
            },
        }
        if semicolon {
            s.push_str(";");
        }
        s
    }
}
