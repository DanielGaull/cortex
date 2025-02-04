use std::error::Error;
use crate::parsing::ast::expression::{Atom, Expression, ExpressionTail, PathIdent};
use super::value::CortexValue;

pub struct CortexInterpreter {

}

impl CortexInterpreter {
    pub fn evaluate_expression(&self, expr: &Expression) -> Result<CortexValue, Box<dyn Error>> {
        let atom_result = self.evaluate_atom(&expr.atom)?;
        let tail_result = self.handle_expr_tail(atom_result, &expr.tail)?;
        Ok(tail_result)
    }

    fn evaluate_atom(&self, atom: &Atom) -> Result<CortexValue, Box<dyn Error>> {
        match atom {
            Atom::Boolean(v) => Ok(CortexValue::Boolean(*v)),
            Atom::Number(v) => Ok(CortexValue::Number(*v)),
            Atom::String(v) => Ok(CortexValue::String(v.clone())),
            Atom::Void => Ok(CortexValue::Void),
            Atom::Null => Ok(CortexValue::Null),
            Atom::Expression(expr) => Ok(self.evaluate_expression(expr)?),
            Atom::PathIdent(path) => Ok(self.lookup_value(path)?),
            Atom::Call(path_ident, expressions) => todo!(),
        }
    }
    fn handle_expr_tail(&self, atom: CortexValue, tail: &ExpressionTail) -> Result<CortexValue, Box<dyn Error>> {
        match tail {
            ExpressionTail::None => Ok(atom),
        }
    }

    fn lookup_value(&self, path: &PathIdent) -> Result<CortexValue, Box<dyn Error>> {
        todo!()
    }
}
