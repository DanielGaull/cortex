use crate::parsing::codegen::r#trait::SimpleCodeGen;

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
    Function(Function),
}
impl SimpleCodeGen for TopLevel {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Self::Import { name, is_string_import } => {
                if *is_string_import {
                    format!("import \"{}\";", name)
                } else {
                    format!("import {};", name.clone())
                }
            },
            Self::Module { name, contents } => {
                let mut s = String::new();
                let indent_prefix = &"    ".repeat(indent);
                s.push_str(indent_prefix);
                s.push_str("module ");
                s.push_str(name);
                s.push_str(" {\n");

                for top in contents {
                    s.push_str(&top.codegen(indent + 1));
                    s.push_str("\n");
                }
                
                s.push_str(indent_prefix);
                s.push_str("}");
                s
            },
            Self::Function(func) => func.codegen(indent),
        }
    }
}

pub struct Function {
    pub(crate) name: OptionalIdentifier,
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CType,
    pub(crate) body: Body,
}
impl SimpleCodeGen for Function {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = &"    ".repeat(indent);

        s.push_str(indent_prefix);
        s.push_str("fn ");
        s.push_str(&self.name.codegen(indent));
        s.push_str("(");
        for (i, param) in self.params.iter().enumerate() {
            s.push_str(&param.codegen(indent));
            if i + 1 < self.params.len() {
                s.push_str(", ");
            }
        }
        s.push_str("): ");
        s.push_str(&self.return_type.codegen(indent));
        s.push_str(" {\n");

        s.push_str(&self.body.codegen(indent + 1));

        s.push_str(indent_prefix);
        s.push_str("}");
        s
    }
}

pub struct Body {
    pub(crate) statements: Vec<Statement>,
    pub(crate) result: Option<Expression>,
}
impl SimpleCodeGen for Body {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        for st in &self.statements {
            s.push_str(&format!("{}\n", st.codegen(indent)));
        }
        if let Some(exp) = &self.result {
            let indent_prefix = "    ".repeat(indent);
            s.push_str(&format!("{}{}\n", indent_prefix, exp.codegen(indent)));
        }
        s
    }
}
