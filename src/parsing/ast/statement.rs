use crate::{parsing::codegen::r#trait::SimpleCodeGen, r#type::r#type::PType};

use super::expression::{PConditionBody, PExpression, IdentExpression, OptionalIdentifier};

#[derive(Clone)]
pub enum AssignmentName {
    Single(IdentExpression),
    Tuple(Vec<AssignmentName>),
    Ignore,
}
impl SimpleCodeGen for AssignmentName {
    fn codegen(&self, indent: usize) -> String {
        match self {
            AssignmentName::Single(ident_expression) => {
                ident_expression.codegen(indent)
            },
            AssignmentName::Tuple(items) => {
                if items.len() == 1 {
                    format!("({},)", items.get(0).unwrap().codegen(indent))
                } else {
                    format!("({})", items.iter().map(|i| i.codegen(indent)).collect::<Vec<_>>().join(", "))
                }
            },
            AssignmentName::Ignore => String::from("~"),
        }
    }
}

#[derive(Clone)]
pub enum DeclarationName {
    Single(OptionalIdentifier),
    Tuple(Vec<DeclarationName>),
}
impl SimpleCodeGen for DeclarationName {
    fn codegen(&self, indent: usize) -> String {
        match self {
            DeclarationName::Single(elem) => {
                elem.codegen(indent)
            },
            DeclarationName::Tuple(items) => {
                if items.len() == 1 {
                    format!("({},)", items.get(0).unwrap().codegen(indent))
                } else {
                    format!("({})", items.iter().map(|i| i.codegen(indent)).collect::<Vec<_>>().join(", "))
                }
            },
        }
    }
}

#[derive(Clone)]
pub enum PStatement {
    Expression(PExpression),
    Throw(Option<PExpression>),
    VariableDeclaration {
        name: DeclarationName,
        is_const: bool,
        typ: Option<PType>,
        initial_value: PExpression,
    },
    Assignment {
        name: AssignmentName,
        value: PExpression,
    },
    WhileLoop(PConditionBody),
    Break,
    Continue,
}
impl SimpleCodeGen for PStatement {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);
        s.push_str(&indent_prefix);
        let mut semicolon = true;
        match self {
            Self::Expression(expr) => {
                s.push_str(&expr.codegen(indent));
            },
            Self::Throw(expr) => {
                s.push_str("throw");
                if let Some(ex) = expr {
                    s.push_str(" ");
                    s.push_str(&ex.codegen(indent));
                }
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
            Self::Assignment { name, value } => {
                s.push_str(&name.codegen(indent));
                s.push_str(" = ");
                s.push_str(&value.codegen(indent));
            },
            Self::WhileLoop(cond_body) => {
                semicolon = false;
                s.push_str("while ");
                s.push_str(&cond_body.condition.codegen(indent));
                s.push_str(" {\n");
                s.push_str(&cond_body.body.codegen(indent + 1));
                s.push_str(&indent_prefix);
                s.push_str("}");
            },
            Self::Break => {
                s.push_str("break");
            },
            Self::Continue => {
                s.push_str("continue");
            },
        }
        if semicolon {
            s.push_str(";");
        }
        s
    }
}
