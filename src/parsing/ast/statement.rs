use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::expression::Expression;

pub enum Statement {
    Expression(Expression),
    Stop,
}
impl SimpleCodeGen for Statement {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);
        s.push_str(&indent_prefix);
        match self {
            Self::Expression(expr) => {
                s.push_str(expr.codegen(indent).as_str());
                s.push_str(";");
            },
            Self::Stop => {
                s.push_str("stop;");
            }
        }
        s
    }
}
