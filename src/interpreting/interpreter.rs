use std::error::Error;

use thiserror::Error;

use crate::parsing::{ast::{expression::{Atom, Expression, ExpressionTail, OptionalIdentifier, PathIdent}, statement::Statement, top_level::Function, typ::CType}, codegen::r#trait::SimpleCodeGen};
use super::{env::Environment, module::Module, r#type::CortexType, value::CortexValue};

type CortexError = Box<dyn Error>;

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Cannot modify value \"\" if it comes from a module")]
    CannotModifyModuleEnvironment(String),
    #[error("Program execution was forcibly stopped using a \"stop\" statement")]
    ProgramStopped,
}

pub struct CortexInterpreter {
    base_module: Module,
    current_env: Environment,
}

impl CortexInterpreter {
    pub fn new() -> Self {
        CortexInterpreter {
            // NOTE: We will never use the base module's environment
            // since module Environments are immutable
            base_module: Module::new(Environment::base()),
            current_env: Environment::base(),
        }
    }

    pub fn register_module(&mut self, path: &PathIdent, module: Module) -> Result<(), CortexError> {
        self.base_module.add_module(path, module)?;
        Ok(())
    }

    pub fn run_statement(&mut self, statement: &Statement) -> Result<(), CortexError> {
        match statement {
            Statement::Expression(expression) => {
                self.evaluate_expression(expression)?;
                Ok(())
            },
            Statement::Stop => {
                Err(Box::new(InterpreterError::ProgramStopped))
            },
            Statement::VariableDeclaration { 
                name, is_const, typ, initial_value 
            } => {
                match name {
                    OptionalIdentifier::Ident(ident) => {
                        let value = self.evaluate_expression(initial_value)?;
                        let true_type = if let Some(the_type) = typ {
                            self.evaluate_type(the_type)?
                        } else {
                            self.determine_type(initial_value)?
                        };

                        if *is_const {
                            self.current_env.add_const(ident.clone(), true_type, value)?;
                        } else {
                            self.current_env.add_var(ident.clone(), true_type, value)?;
                        }
                        Ok(())
                    },
                    OptionalIdentifier::Ignore => {
                        self.evaluate_expression(initial_value)?;
                        Ok(())
                    },
                }
            },
            Statement::VariableAssignment { name, value } => {
                if !name.is_final()? {
                    Err(Box::new(InterpreterError::CannotModifyModuleEnvironment(name.codegen(0))))
                } else {
                    let var_name = name.get_front()?;
                    let value = self.evaluate_expression(value)?;
                    self.current_env.set_value(var_name, value)?;
                    Ok(())
                }
            },
        }
    }

    pub fn evaluate_expression(&self, expr: &Expression) -> Result<CortexValue, CortexError> {
        let atom_result = self.evaluate_atom(&expr.atom)?;
        let tail_result = self.handle_expr_tail(atom_result, &expr.tail)?;
        Ok(tail_result)
    }

    pub fn evaluate_type(&self, typ: &CType) -> Result<CortexType, CortexError> {
        todo!()
    }

    pub fn determine_type(&self, expr: &Expression) -> Result<CortexType, CortexError> {
        todo!()
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
