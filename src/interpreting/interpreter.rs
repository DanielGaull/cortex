use std::error::Error;
use thiserror::Error;

use crate::parsing::ast::{expression::{Atom, Expression, ExpressionTail, PathError, PathIdent}, top_level::Function};
use super::{env::{EnvError, Environment}, module::{Module, ModuleError}, value::CortexValue};

type CortexError = Box<dyn Error>;

pub struct CortexInterpreter {
    base_module: Module,
    current_env: Environment,
}

impl CortexInterpreter {
    pub fn evaluate_expression(&self, expr: &Expression) -> Result<CortexValue, CortexError> {
        let atom_result = self.evaluate_atom(&expr.atom)?;
        let tail_result = self.handle_expr_tail(atom_result, &expr.tail)?;
        Ok(tail_result)
    }

    fn evaluate_atom(&self, atom: &Atom) -> Result<CortexValue, CortexError> {
        match atom {
            Atom::Boolean(v) => Ok(CortexValue::Boolean(*v)),
            Atom::Number(v) => Ok(CortexValue::Number(*v)),
            Atom::String(v) => Ok(CortexValue::String(v.clone())),
            Atom::Void => Ok(CortexValue::Void),
            Atom::Null => Ok(CortexValue::Null),
            Atom::Expression(expr) => Ok(self.evaluate_expression(expr)?),
            Atom::PathIdent(path) => Ok(self.lookup_value(path)?),
            Atom::Call(path_ident, expressions) => {
                let func = self.lookup_function(path_ident)?;
                todo!()
            },
        }
    }
    fn handle_expr_tail(&self, atom: CortexValue, tail: &ExpressionTail) -> Result<CortexValue, CortexError> {
        match tail {
            ExpressionTail::None => Ok(atom),
        }
    }

    fn lookup_value(&self, path: &PathIdent) -> Result<CortexValue, CortexError> {
        if path.is_final()? {
            // Search in our environment for it
            Ok(self.current_env.get_value(path.get_front()?)?.clone())
        } else {
            let last = path.get_back()?;
            Ok(self.base_module.get_module(path)?.env().get_value(last)?.clone())
        }
    }
    fn lookup_function(&self, path: &PathIdent) -> Result<&Function, CortexError> {
        if path.is_final()? {
            // Search in our environment for it
            Ok(self.current_env.get_function(path.get_front()?)?)
        } else {
            let last = path.get_back()?;
            Ok(self.base_module.get_module(path)?.env().get_function(last)?)
        }
    }
}
