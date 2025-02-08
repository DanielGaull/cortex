use std::{collections::HashMap, error::Error, rc::Rc};

use thiserror::Error;
use paste::paste;

use crate::parsing::{ast::{expression::{Atom, BinaryOperator, EqResult, Expression, ExpressionTail, MulResult, OptionalIdentifier, PathIdent, Primary, SumResult}, statement::Statement, top_level::{Body, Function, TopLevel}, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};
use super::{env::Environment, module::Module, value::CortexValue};

macro_rules! determine_op_type_fn {
    ($name:ident, $typ:ty, $prev_name:ident) => {
        paste! {
            fn [<determine_type_ $name>](&self, this: &$typ) -> Result<CortexType, CortexError> {
                let first = &this.first;
                let rest = &this.rest;
                let mut typ = self.[<determine_type_ $prev_name>](first)?;
                for (op, item) in rest {
                    let second = self.[<determine_type_ $prev_name>](item)?;
                    typ = self.determine_type_operator(typ, op, second)?;
                }
                Ok(typ)
            }
        }
    }
}

macro_rules! evaluate_op_fn {
    ($name:ident, $typ:ty, $prev_name:ident) => {
        paste! {
            fn [<evaluate_ $name>](&mut self, item: &$typ) -> Result<CortexValue, CortexError> {
                let mut typ = self.[<evaluate_ $prev_name>](&item.first)?;
                for (op, exp) in &item.rest {
                    let second = self.[<evaluate_ $prev_name>](exp)?;
                    typ = self.evaluate_op(typ, op, second)?;
                }
                Ok(typ)
            }
        }
    }
}

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
    #[error("Invalid operator values: only the type(s) {0} and {1} are allowed")]
    InvalidOperator(&'static str, &'static str),
    #[error("Expected an integer value in this context; {0} is not an integer")]
    ExpectedInteger(f64),
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

    pub fn run_top_level(&mut self, top_level: TopLevel) -> Result<(), CortexError> {
        match top_level {
            TopLevel::Import { name: _, is_string_import: _ } => {
                todo!()
            },
            TopLevel::Module { name, contents } => {
                let module = Self::construct_module(contents)?;
                self.register_module(&PathIdent::simple(name), module)?;
                Ok(())
            },
            TopLevel::Function(function) => {
                self.current_env.as_mut().unwrap().add_function(function)?;
                Ok(())
            },
            TopLevel::Struct(struc) => {
                self.current_env.as_mut().unwrap().add_struct(struc)?;
                Ok(())
            },
        }
    }

    fn construct_module(contents: Vec<TopLevel>) -> Result<Module, CortexError> {
        let mut env = Environment::base();
        let mut children = HashMap::<String, Module>::new();
        for item in contents.into_iter() {
            match item {
                TopLevel::Import { name: _, is_string_import: _ } => todo!(),
                TopLevel::Module { name: submod_name, contents } => {
                    let new_module = Self::construct_module(contents)?;
                    children.insert(submod_name, new_module);
                },
                TopLevel::Function(function) => {
                    match &function.name {
                        OptionalIdentifier::Ident(_) => {
                            env.add_function(function)?;
                        },
                        OptionalIdentifier::Ignore => (),
                    }
                },
                TopLevel::Struct(item) => {
                    match &item.name {
                        OptionalIdentifier::Ident(_) => {
                            env.add_struct(item)?;
                        },
                        OptionalIdentifier::Ignore => (),
                    }
                },
            }
        }

        Ok(Module::with_children(env, children))
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
            Statement::VariableAssignment { name, value, op } => {
                if !name.is_final()? {
                    Err(Box::new(InterpreterError::CannotModifyModuleEnvironment(name.codegen(0))))
                } else {
                    let var_name = name.get_front()?;
                    let assigned_type = self.determine_type(value)?;
                    let var_type = self.current_env.as_ref().unwrap().get_type_of(var_name)?;
                    if let Some(binop) = op {
                        let type_op = self.determine_type_operator(var_type.clone(), binop, assigned_type)?;
                        if !type_op.is_subtype_of(var_type) {
                            return Err(Box::new(InterpreterError::MismatchedType(var_type.codegen(0), type_op.codegen(0))));
                        }
                        let second = self.evaluate_expression(value)?;
                        let first = self.current_env.as_ref().unwrap().get_value(var_name)?;
                        let value = self.evaluate_op(first.clone(), binop, second)?;
                        self.current_env.as_mut().unwrap().set_value(var_name, value)?;
                    } else {
                        if !assigned_type.is_subtype_of(var_type) {
                            return Err(Box::new(InterpreterError::MismatchedType(var_type.codegen(0), assigned_type.codegen(0))));
                        }
                        let value = self.evaluate_expression(value)?;
                        self.current_env.as_mut().unwrap().set_value(var_name, value)?;
                    }
                    Ok(())
                }
            },
        }
    }

    pub fn determine_type(&self, expr: &Expression) -> Result<CortexType, CortexError> {
        self.determine_type_expression(expr)
    }
    fn determine_type_primary(&self, primary: &Primary) -> Result<CortexType, CortexError> {
        let atom_result = self.determine_type_atom(&primary.atom)?;
        let tail_result = self.determine_type_tail(atom_result, &primary.tail)?;
        Ok(tail_result)
    }
    determine_op_type_fn!(mul_result, MulResult, primary);
    determine_op_type_fn!(sum_result, SumResult, mul_result);
    determine_op_type_fn!(eq_result, EqResult, sum_result);
    determine_op_type_fn!(expression, Expression, eq_result);
    fn determine_type_operator(&self, first: CortexType, op: &BinaryOperator, second: CortexType) -> Result<CortexType, CortexError> {
        let number = CortexType::number(false);
        let string = CortexType::string(false);
        let boolean = CortexType::boolean(false);
        match op {
            BinaryOperator::Add => {
                if first == number && second == number {
                    Ok(number)
                } else if first == string && second == string {
                    Ok(string)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number, string", "number, string")))
                }
            },
            BinaryOperator::Subtract => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::Multiply => {
                if first == number && second == number {
                    Ok(number)
                } else if first == number && second == string {
                    Ok(string)
                } else if first == string && second == number {
                    Ok(string)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "string")))
                }
            },
            BinaryOperator::Divide => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::Remainder => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::LogicAnd => {
                if first == boolean && second == boolean {
                    Ok(boolean)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("boolean", "boolean")))
                }
            },
            BinaryOperator::LogicOr => {
                if first == boolean && second == boolean {
                    Ok(boolean)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("boolean", "boolean")))
                }
            },
            BinaryOperator::IsEqual => {
                Ok(boolean)
            },
            BinaryOperator::IsNotEqual => {
                Ok(boolean)
            },
            BinaryOperator::IsLessThan => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsGreaterThan => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsLessThanOrEqualTo => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsGreaterThanOrEqualTo => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
        }
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
        }
    }

    fn evaluate_primary(&mut self, primary: &Primary) -> Result<CortexValue, CortexError> {
        let atom_result = self.evaluate_atom(&primary.atom)?;
        let tail_result = self.handle_expr_tail(atom_result, &primary.tail)?;
        Ok(tail_result)
    }
    evaluate_op_fn!(mul_result, MulResult, primary);
    evaluate_op_fn!(sum_result, SumResult, mul_result);
    evaluate_op_fn!(eq_result, EqResult, sum_result);
    evaluate_op_fn!(expression_priv, Expression, eq_result);
    pub fn evaluate_expression(&mut self, expr: &Expression) -> Result<CortexValue, CortexError> {
        self.evaluate_expression_priv(expr)
    }
    fn evaluate_op(&mut self, first: CortexValue, op: &BinaryOperator, second: CortexValue) -> Result<CortexValue, CortexError> {
        match op {
            BinaryOperator::Add => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Number(n1 + n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number, string", "number, string")))
                    }
                } else if let CortexValue::String(s1) = first {
                    if let CortexValue::String(s2) = second {
                        let mut s = String::new();
                        s.push_str(&s1);
                        s.push_str(&s2);
                        Ok(CortexValue::String(s))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number, string", "number, string")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number, string", "number, string")))
                }
            },
            BinaryOperator::Subtract => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Number(n1 - n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::Multiply => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Number(n1 * n2))
                    } else if let CortexValue::String(s2) = second {
                        if n1.fract() == 0.0 {
                            Ok(CortexValue::String(s2.repeat(n1 as usize)))
                        } else {
                            Err(Box::new(InterpreterError::ExpectedInteger(n1)))
                        }
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number, string")))
                    }
                } else if let CortexValue::String(s1) = first {
                    if let CortexValue::Number(n2) = second {
                        if n2.fract() == 0.0 {
                            Ok(CortexValue::String(s1.repeat(n2 as usize)))
                        } else {
                            Err(Box::new(InterpreterError::ExpectedInteger(n2)))
                        }
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number, string")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number, string")))
                }
            },
            BinaryOperator::Divide => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Number(n1 / n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::Remainder => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Number(n1 % n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::LogicAnd => {
                if let CortexValue::Boolean(b1) = first {
                    if let CortexValue::Boolean(b2) = second {
                        Ok(CortexValue::Boolean(b1 && b2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("bool", "bool")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("bool", "bool")))
                }
            },
            BinaryOperator::LogicOr => {
                if let CortexValue::Boolean(b1) = first {
                    if let CortexValue::Boolean(b2) = second {
                        Ok(CortexValue::Boolean(b1 || b2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("bool", "bool")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("bool", "bool")))
                }
            },
            BinaryOperator::IsEqual => {
                Ok(CortexValue::Boolean(first == second))
            },
            BinaryOperator::IsNotEqual => {
                Ok(CortexValue::Boolean(first != second))
            },
            BinaryOperator::IsLessThan => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Boolean(n1 < n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsGreaterThan => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Boolean(n1 > n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsLessThanOrEqualTo => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Boolean(n1 <= n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsGreaterThanOrEqualTo => {
                if let CortexValue::Number(n1) = first {
                    if let CortexValue::Number(n2) = second {
                        Ok(CortexValue::Boolean(n1 >= n2))
                    } else {
                        Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                    }
                } else {
                    Err(Box::new(InterpreterError::InvalidOperator("number", "number")))
                }
            },
        }
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
        }
    }

    pub fn run_function(&mut self, func: &Rc<Function>, args: &Vec<Expression>) -> Result<CortexValue, CortexError> {
        let body = &func.body;
        let mut param_names = Vec::<String>::with_capacity(func.params.len());
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
            new_env
                .add_var(
                    param_name.clone(),
                    param_type,
                    value
                )?;
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
            Ok(self.current_env.as_ref().unwrap().get_type_of(path.get_front()?)?.clone())
        } else {
            let last = path.get_back()?;
            Ok(self.base_module.get_module(path)?.env().get_type_of(last)?.clone())
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
