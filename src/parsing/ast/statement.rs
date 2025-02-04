use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::{expression::{Expression, PathIdent}, typ::CType};

pub enum Statement {
    Expression(Expression),
    Stop,
    VariableDeclaration {
        name: String,
        is_const: bool,
        typ: Option<CType>,
        initial_value: Expression,
    },
    VariableAssignment {
        name: PathIdent,
        value: Expression,
    },
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
                s.push_str(name);
                if let Some(given_type) = typ {
                    s.push_str(": ");
                    s.push_str(&given_type.codegen(indent));
                }
                s.push_str(" = ");
                s.push_str(&initial_value.codegen(indent));
            },
            Self::VariableAssignment { name, value } => {
                s.push_str(&name.codegen(indent));
                s.push_str(" = ");
                s.push_str(&value.codegen(indent));
            },
        }
        if semicolon {
            s.push_str(";");
        }
        s
    }
}
