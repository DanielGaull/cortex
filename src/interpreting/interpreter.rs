use std::{error::Error, rc::Rc};

use thiserror::Error;

use crate::parsing::{ast::{expression::{Atom, BinaryOperator, Expression, ExpressionTail, OptionalIdentifier, PathIdent}, statement::Statement, top_level::{Body, Function}, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};
use super::{env::Environment, module::Module, value::CortexValue};

pub type CortexError = Box<dyn Error>;

#[derive(Error, Debug, PartialEq)]
pub enum InterpreterError {
    #[error("Cannot modify value \"{0}\" if it comes from a module")]
    CannotModifyModuleEnvironment(String),
    #[error("Program execution was forcibly stopped using a \"stop\" statement")]
    ProgramStopped,
    #[error("Mismatched argument count: Function {0} expects {1} arguments but received {2}")]
    MismatchedArgumentCount(String, usize, usize),
    #[error("Parent environment does not exist")]
    NoParentEnv,
    #[error("Expected type {0} but expression of type {1} was found")]
    MismatchedType(String, String),
    #[error("Invalid operator values: only the types ({0}) and ({1}) are allowed")]
    InvalidOperator(String, String),
}

pub struct CortexInterpreter {
    base_module: Module,
    current_env: Option<Box<Environment>>,
}

impl CortexInterpreter {
    pub fn new() -> Self {
        CortexInterpreter {
            // NOTE: We will never use the base module's environment
            // since module Environments are immutable
            base_module: Module::new(Environment::base()),
            current_env: Some(Box::new(Environment::base())),
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
                            // Check that the declared type and type of the result match
                            let value_type = self.determine_type(initial_value)?;
                            if !value_type.is_subtype_of(the_type) {
                                return Err(Box::new(InterpreterError::MismatchedType(the_type.codegen(0), value_type.codegen(0))));
                            }
                            the_type.clone()
                        } else {
                            self.determine_type(initial_value)?
                        };

                        if *is_const {
                            self.current_env.as_mut().unwrap().add_const(ident.clone(), true_type, value)?;
                        } else {
                            self.current_env.as_mut().unwrap().add_var(ident.clone(), true_type, value)?;
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
                    let assigned_type = self.determine_type(value)?;
                    let var_type = self.current_env.as_ref().unwrap().get_type(var_name)?;
                    if !assigned_type.is_subtype_of(var_type) {
                        return Err(Box::new(InterpreterError::MismatchedType(var_type.codegen(0), assigned_type.codegen(0))));
                    }
                    let value = self.evaluate_expression(value)?;
                    self.current_env.as_mut().unwrap().set_value(var_name, value)?;
                    Ok(())
                }
            },
        }
    }

    pub fn determine_type(&self, expr: &Expression) -> Result<CortexType, CortexError> {
        let atom_result = self.determine_type_atom(&expr.atom)?;
        let tail_result = self.determine_type_tail(atom_result, &expr.tail)?;
        Ok(tail_result)
    }
    fn determine_type_atom(&self, atom: &Atom) -> Result<CortexType, CortexError> {
        match atom {
            Atom::Number(_) => Ok(CortexType::number(false)),
            Atom::Boolean(_) => Ok(CortexType::boolean(false)),
            Atom::Void => Ok(CortexType::void(false)),
            Atom::Null => Ok(CortexType::any(true)),
            Atom::String(_) => Ok(CortexType::string(false)),
            Atom::PathIdent(path_ident) => Ok(self.lookup_type(path_ident)?),
            Atom::Call(path_ident, _) => {
                let func = self.lookup_function(path_ident)?;
                Ok(func.return_type.clone())
            },
            Atom::Expression(expression) => Ok(self.determine_type(expression)?),
        }
    }
    fn determine_type_tail(&self, atom: CortexType, tail: &ExpressionTail) -> Result<CortexType, CortexError> {
        match tail {
            ExpressionTail::None => Ok(atom),
            ExpressionTail::BinaryOperation { op, right, next } => {
                let left_req;
                let right_req;
                let pre = match op {
                    BinaryOperator::Add => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::number(false)
                    },
                    BinaryOperator::Subtract => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::number(false)
                    },
                    BinaryOperator::Multiply => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::number(false)
                    },
                    BinaryOperator::Divide => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::number(false)
                    },
                    BinaryOperator::Remainder => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::number(false)
                    },
                    BinaryOperator::LogicAnd => {
                        left_req = CortexType::boolean(false);
                        right_req = CortexType::boolean(false);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::LogicOr => {
                        left_req = CortexType::boolean(false);
                        right_req = CortexType::boolean(false);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::IsEqual => {
                        left_req = CortexType::any(true);
                        right_req = CortexType::any(true);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::IsNotEqual => {
                        left_req = CortexType::any(true);
                        right_req = CortexType::any(true);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::IsLessThan => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::IsGreaterThan => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::IsLessThanOrEqualTo => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::boolean(false)
                    },
                    BinaryOperator::IsGreaterThanOrEqualTo => {
                        left_req = CortexType::number(false);
                        right_req = CortexType::number(false);
                        CortexType::boolean(false)
                    },
                };
                if !atom.is_subtype_of(&left_req) {
                    return Err(Box::new(InterpreterError::InvalidOperator(left_req.codegen(0), right_req.codegen(0))));
                }
                let right_type = self.determine_type(right)?;
                if !right_type.is_subtype_of(&right_req) {
                    return Err(Box::new(InterpreterError::InvalidOperator(left_req.codegen(0), right_req.codegen(0))));
                }
                Ok(self.determine_type_tail(pre, next)?)
            },
        }
    }

    pub fn evaluate_expression(&mut self, expr: &Expression) -> Result<CortexValue, CortexError> {
        let atom_result = self.evaluate_atom(&expr.atom)?;
        let tail_result = self.handle_expr_tail(atom_result, &expr.tail)?;
        Ok(tail_result)
    }
    fn evaluate_atom(&mut self, atom: &Atom) -> Result<CortexValue, CortexError> {
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
                Ok(self.run_function(&func, expressions)?)
            },
        }
    }
    fn handle_expr_tail(&mut self, atom: CortexValue, tail: &ExpressionTail) -> Result<CortexValue, CortexError> {
        match tail {
            ExpressionTail::None => Ok(atom),
            ExpressionTail::BinaryOperation { op, right, next } => {
                let right_value = self.evaluate_expression(right)?;
                let pre = match op {
                    BinaryOperator::Add => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Number(n1 + n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::Subtract => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Number(n1 - n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::Multiply => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Number(n1 * n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::Divide => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Number(n1 / n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::Remainder => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Number(n1 % n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::LogicAnd => {
                        if let CortexValue::Boolean(b1) = atom {
                            if let CortexValue::Boolean(b2) = right_value {
                                Ok(CortexValue::Boolean(b1 && b2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("bool"), String::from("bool"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("bool"), String::from("bool"))))
                        }
                    },
                    BinaryOperator::LogicOr => {
                        if let CortexValue::Boolean(b1) = atom {
                            if let CortexValue::Boolean(b2) = right_value {
                                Ok(CortexValue::Boolean(b1 || b2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("bool"), String::from("bool"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("bool"), String::from("bool"))))
                        }
                    },
                    BinaryOperator::IsEqual => {
                        let result = atom == right_value;
                        Ok(CortexValue::Boolean(result))
                    },
                    BinaryOperator::IsNotEqual => {
                        let result = atom != right_value;
                        Ok(CortexValue::Boolean(result))
                    },
                    BinaryOperator::IsLessThan => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Boolean(n1 < n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::IsGreaterThan => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Boolean(n1 > n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::IsLessThanOrEqualTo => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Boolean(n1 <= n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                    BinaryOperator::IsGreaterThanOrEqualTo => {
                        if let CortexValue::Number(n1) = atom {
                            if let CortexValue::Number(n2) = right_value {
                                Ok(CortexValue::Boolean(n1 >= n2))
                            } else {
                                Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                            }
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperator(String::from("number"), String::from("number"))))
                        }
                    },
                }?;
                Ok(self.handle_expr_tail(pre, next)?)
            },
        }
    }

    pub fn run_function(&mut self, func: &Rc<Function>, args: &Vec<Expression>) -> Result<CortexValue, CortexError> {
        let body = &func.body;
        let mut param_names = Vec::<OptionalIdentifier>::with_capacity(func.params.len());
        let mut param_types = Vec::<CortexType>::with_capacity(func.params.len());
        for param in &func.params {
            param_names.push(param.name.clone());
            param_types.push(param.typ.clone());
        }

        if args.len() != func.params.len() {
            return Err(Box::new(
                InterpreterError::MismatchedArgumentCount(func.name.codegen(0), func.params.len(), args.len())
            ));
        }

        let mut arg_values = Vec::<CortexValue>::new();
        for (i, arg) in args.iter().enumerate() {
            arg_values.push(self.evaluate_expression(arg)?);
            let arg_type = self.determine_type(arg)?;
            let param_type = param_types.get(i).unwrap();
            if !arg_type.is_subtype_of(param_type) {
                return Err(Box::new(InterpreterError::MismatchedType(param_type.codegen(0), arg_type.codegen(0))));
            }
        }

        // Four steps:
        // 1. Construct a new environment for this function w/ all params in it
        // 2. Run the code of the function and store the return value
        // 3. Deconstruct the environment we just created
        // 4. Return the saved-off value
        
        // Create new env
        // Get ownership sorted first before adding values to the new environment
        let parent_env = self.current_env.take().ok_or(InterpreterError::NoParentEnv)?;
        let mut new_env = Environment::new(*parent_env);
        for i in 0..args.len() {
            let value = arg_values.remove(0);
            let param_name = param_names.get(i).unwrap();
            let param_type = param_types.remove(0);
            match &param_name {
                OptionalIdentifier::Ident(ident) => {
                    new_env
                        .add_var(
                            ident.clone(), 
                            param_type,
                            value
                        )?;
                },
                OptionalIdentifier::Ignore => {
                    // Do nothing
                    // We already evaluated the expression of the argument,
                    // so we now just need to not place it into our new environment
                },
            }
        }
        self.current_env = Some(Box::new(new_env));

        // Run body
        let result = self.evaluate_body(&body)?;

        // Deconstruct environment
        self.current_env = Some(Box::new(
            self.current_env
                .take()
                .ok_or(InterpreterError::NoParentEnv)?
                .exit()?));

        // Return result
        Ok(result)
    }

    fn evaluate_body(&mut self, body: &Body) -> Result<CortexValue, CortexError> {
        match body {
            Body::Basic { statements, result } => {
                for st in statements {
                    self.run_statement(st)?;
                }
        
                if let Some(return_expr) = result {
                    let res = self.evaluate_expression(return_expr)?;
                    Ok(res)
                } else {
                    Ok(CortexValue::Void)
                }
            },
            Body::Native(func) => {
                let res = func(self.current_env.as_ref().unwrap())?;
                Ok(res)
            },
        }
    }

    fn lookup_type(&self, path: &PathIdent) -> Result<CortexType, CortexError> {
        if path.is_final()? {
            // Search in our environment for it
            Ok(self.current_env.as_ref().unwrap().get_type(path.get_front()?)?.clone())
        } else {
            let last = path.get_back()?;
            Ok(self.base_module.get_module(path)?.env().get_type(last)?.clone())
        }
    }
    fn lookup_value(&self, path: &PathIdent) -> Result<CortexValue, CortexError> {
        if path.is_final()? {
            // Search in our environment for it
            Ok(self.current_env.as_ref().unwrap().get_value(path.get_front()?)?.clone())
        } else {
            let last = path.get_back()?;
            Ok(self.base_module.get_module(path)?.env().get_value(last)?.clone())
        }
    }
    fn lookup_function(&self, path: &PathIdent) -> Result<Rc<Function>, CortexError> {
        if path.is_final()? {
            // Search in our environment for it
            Ok(self.current_env.as_ref().unwrap().get_function(path.get_front()?)?)
        } else {
            let last = path.get_back()?;
            Ok(self.base_module.get_module(path)?.env().get_function(last)?)
        }
    }
}
