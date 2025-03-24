use std::{cell::RefCell, collections::{HashMap, HashSet}, rc::Rc};

use crate::{parsing::ast::{expression::{BinaryOperator, Expression, PathIdent, UnaryOperator}, statement::Statement, top_level::{BasicBody, TopLevel}}, preprocessing::{ast::{expression::RExpression, function::{RBody, RFunction, RInterpretedBody}, statement::RStatement}, module::Module, preprocessor::CortexPreprocessor, program::Program}};
use super::{env::Environment, error::{CortexError, InterpreterError}, heap::Heap, value::{CortexValue, ValueError}};

pub struct CortexInterpreter {
    preprocessor: CortexPreprocessor,
    current_env: Option<Box<Environment>>,
    heap: Heap,
}

impl CortexInterpreter {
    pub fn new() -> Result<Self, CortexError> {
        Ok(CortexInterpreter {
            preprocessor: CortexPreprocessor::new()?,
            current_env: Some(Box::new(Environment::base())),
            heap: Heap::new(),
        })
    }

    pub fn execute(&mut self, program: Program) -> Result<CortexValue, CortexError> {
        Ok(self.evaluate_interpreted_body(&program.code)?)
    }
    pub fn execute_expression(&mut self, expression: Expression) -> Result<CortexValue, CortexError> {
        let body = BasicBody::new(vec![], Some(expression));
        let program = self.preprocessor.preprocess(body)?;
        Ok(self.execute(program)?)
    }
    pub fn execute_statement(&mut self, statement: Statement) -> Result<(), CortexError> {
        let body = BasicBody::new(vec![statement], None);
        let program = self.preprocessor.preprocess(body)?;
        self.execute(program)?;
        Ok(())
    }

    pub fn gc(&mut self) {
        // Note: "roots" are the addresses of all pointers currently in memory
        // It also includes pointers that reside within struct fields currently in memory,
        // and those struct's fields, etc.
        let mut roots = HashSet::<usize>::new();
        if let Some(env) = &self.current_env {
            env.foreach(|_name, value| self.find_reachables(&mut roots, Rc::new(RefCell::new(value.clone()))));
        }
        self.heap.gc(roots);
    }
    fn find_reachables(&self, current: &mut HashSet<usize>, value: Rc<RefCell<CortexValue>>) {
        let value_ref = value.borrow();
        if let CortexValue::Composite { field_values } = &*value_ref {
            for (_, fvalue) in field_values {
                self.find_reachables(current, fvalue.clone());
            }
        } else if let CortexValue::Reference(addr) = *value_ref {
            current.insert(addr);
        }
    }
    pub fn hpsz(&self) -> usize {
        self.heap.sz()
    }

    pub fn register_module(&mut self, path: &PathIdent, module: Module) -> Result<(), CortexError> {
        self.preprocessor.register_module(path, module)
    }

    pub fn run_top_level(&mut self, top_level: TopLevel) -> Result<(), CortexError> {
        self.preprocessor.run_top_level(top_level)
    }

    fn run_statement(&mut self, statement: &RStatement) -> Result<(), CortexError> {
        match statement {
            RStatement::Expression(expression) => {
                self.evaluate_expression(expression)?;
                Ok(())
            },
            RStatement::Throw(expr) => {
                let val = self.evaluate_expression(expr)?;
                Err(Box::new(InterpreterError::ProgramThrow(val)))
            },
            RStatement::VariableDeclaration { 
                name, is_const, initial_value 
            } => {
                let value = self.evaluate_expression(initial_value)?;
                if *is_const {
                    self.current_env.as_mut().unwrap().add_const(name.clone(), value)?;
                } else {
                    self.current_env.as_mut().unwrap().add_var(name.clone(), value)?;
                }
                Ok(())
            },
            RStatement::Assignment { name, value } => {
                if name.is_simple() {
                    let var_name = &name.base;
                    let value = self.evaluate_expression(value)?;
                    self.current_env.as_mut().unwrap().set_value(var_name, value)?;
                    Ok(())
                } else {
                    let var_name = &name.base;
                    let chain = name.chain.clone();
                    let value = self.evaluate_expression(value)?;
                    let base = self.current_env.as_mut().unwrap().get_cell(var_name)?;
                    self.set_field_path(base, chain, value)?;
                    Ok(())
                }
            },
            RStatement::WhileLoop(condition_body) => {
                loop {
                    let cond = self.evaluate_expression(&condition_body.condition)?;
                    if let CortexValue::Boolean(b) = cond {
                        if b {
                            for st in &condition_body.body.statements {
                                self.run_statement(st)?;
                            }
                        } else {
                            break;
                        }
                    }
                }
                Ok(())
            },
        }
    }

    fn evaluate_op(&mut self, first: CortexValue, op: &BinaryOperator, second: CortexValue) -> Result<CortexValue, CortexError> {
        match op {
            BinaryOperator::Add => {
                match (first, second) {
                    (CortexValue::Number(n1), CortexValue::Number(n2)) => Ok(CortexValue::Number(n1 + n2)),
                    (CortexValue::String(s1), CortexValue::String(s2)) => {
                        let mut s = String::new();
                        s.push_str(&s1);
                        s.push_str(&s2);
                        Ok(CortexValue::String(s))
                    },
                    _ => Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess)),
                }
            },
            BinaryOperator::Subtract => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Number(n1 - n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
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
                        Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                    }
                } else if let (CortexValue::String(s1), CortexValue::Number(n2)) = (first, second) {
                    if n2.fract() == 0.0 {
                        Ok(CortexValue::String(s1.repeat(n2 as usize)))
                    } else {
                        Err(Box::new(InterpreterError::ExpectedInteger(n2)))
                    }
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::Divide => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Number(n1 / n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::Remainder => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Number(n1 % n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::LogicAnd => {
                if let (CortexValue::Boolean(b1), CortexValue::Boolean(b2)) = (first, second) {
                    Ok(CortexValue::Boolean(b1 && b2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::LogicOr => {
                if let (CortexValue::Boolean(b1), CortexValue::Boolean(b2)) = (first, second) {
                    Ok(CortexValue::Boolean(b1 || b2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::IsEqual => {
                Ok(CortexValue::Boolean(first == second))
            },
            BinaryOperator::IsNotEqual => {
                Ok(CortexValue::Boolean(first != second))
            },
            BinaryOperator::IsLessThan => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Boolean(n1 < n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::IsGreaterThan => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Boolean(n1 > n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::IsLessThanOrEqualTo => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Boolean(n1 <= n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            BinaryOperator::IsGreaterThanOrEqualTo => {
                if let (CortexValue::Number(n1), CortexValue::Number(n2)) = (first, second) {
                    Ok(CortexValue::Boolean(n1 >= n2))
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
        }
    }
    fn evaluate_expression(&mut self, exp: &RExpression) -> Result<CortexValue, CortexError> {
        match exp {
            RExpression::Boolean(v) => Ok(CortexValue::Boolean(*v)),
            RExpression::Number(v) => Ok(CortexValue::Number(*v)),
            RExpression::String(v) => Ok(CortexValue::String(v.clone())),
            RExpression::Void => Ok(CortexValue::Void),
            RExpression::None => Ok(CortexValue::None),
            RExpression::Identifier(path) => Ok(self.lookup_value(path)?),
            RExpression::Call(id, expressions) => {
                let func = self.lookup_function(*id)?.clone();
                let func_result = self.run_function(&func, expressions.iter().collect());
                Ok(func_result?)
            },
            RExpression::Construction { assignments, is_heap_allocated } => {
                if !*is_heap_allocated {
                    Ok(self.construct_struct(assignments)?)
                } else {
                    let value = self.construct_struct(assignments)?;
                    let addr = self.allocate(value);
                    Ok(CortexValue::Reference(addr))
                }
            },
            RExpression::IfStatement { first, conds, last } => {
                let cond = self.evaluate_expression(&first.condition)?;
                if let CortexValue::Boolean(b) = cond {
                    if b {
                        self.evaluate_interpreted_body(&first.body)
                    } else {
                        for c in conds {
                            let cond = self.evaluate_expression(&c.condition)?;
                            if let CortexValue::Boolean(b) = cond {
                                if b {
                                    return Ok(self.evaluate_interpreted_body(&c.body)?);
                                }
                            } else {
                                return Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess));
                            }
                        }
                        if let Some(c) = last {
                            Ok(self.evaluate_interpreted_body(&*c)?)
                        } else {
                            Ok(CortexValue::Void)
                        }
                    }
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            RExpression::UnaryOperation { op, exp } => {
                let val = self.evaluate_expression(exp)?;
                match op {
                    UnaryOperator::Negate => {
                        if let CortexValue::Number(n) = val {
                            Ok(CortexValue::Number(-n))
                        } else {
                            Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                        }
                    },
                    UnaryOperator::Invert => {
                        if let CortexValue::Boolean(b) = val {
                            Ok(CortexValue::Boolean(!b))
                        } else {
                            Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                        }
                    },
                }
            },
            RExpression::ListLiteral(items) => {
                let values = items
                    .iter()
                    .map(|e| self.evaluate_expression(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let addr = self.heap.allocate(CortexValue::List(values));
                Ok(CortexValue::Reference(addr))
            },
            RExpression::Bang(inner) => {
                let inner = self.evaluate_expression(inner)?;
                if let CortexValue::None = inner {
                    Err(Box::new(InterpreterError::BangCalledOnNoneValue))
                } else {
                    Ok(inner)
                }
            },
            RExpression::MemberAccess(inner, member) => {
                let inner = self.evaluate_expression(inner)?;
                if let CortexValue::Reference(addr) = inner {
                    let val = self.heap
                        .get(addr)
                        .borrow()
                        .get_field(member)?
                        .borrow()
                        .clone();
                    Ok(val)
                } else {
                    Ok(inner.get_field(member)?.borrow().clone())
                }
            },
            RExpression::BinaryOperation { left, op, right } => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;
                let result = self.evaluate_op(left, op, right)?;
                Ok(result)
            },
        }
    }

    fn allocate(&mut self, value: CortexValue) -> usize {
        let ptr = self.heap.allocate(value);
        if self.heap.is_at_gc_threshold() {
            self.gc();
        }
        ptr
    }
    fn construct_struct(
        &mut self,
        assignments: &Vec<(String, RExpression)>,
    ) -> Result<CortexValue, CortexError> {
        let mut values = HashMap::<String, Rc<RefCell<CortexValue>>>::new();
        for (fname, value) in assignments {
            let assigned_value = self.evaluate_expression(value)?;
            values.insert(fname.clone(), Rc::new(RefCell::new(assigned_value)));
        }
        Ok(
            CortexValue::Composite {
                field_values: values,
            }
        )
    }
    
    fn set_field_path(&mut self, mut base: Rc<RefCell<CortexValue>>, mut path: Vec<String>, value: CortexValue) -> Result<(), ValueError> {
        let first_option = path.get(0);
        if let CortexValue::Reference(addr) = &*base.clone().borrow() {
            base = self.heap.get(*addr);
        }
        if let Some(first) = first_option {
            if path.len() == 1{
                base.borrow_mut().set_field(first, value)
            } else {
                let fname = first.clone();
                path.remove(0);
                let field = base.borrow().get_field(&fname)?.clone();
                self.set_field_path(field, path, value)
            }
        } else {
            Err(ValueError::MemberPathCannotBeEmpty)
        }
    }

    fn run_function(&mut self, func: &Rc<RFunction>, args: Vec<&RExpression>) -> Result<CortexValue, CortexError> {
        let mut arg_values = Vec::<CortexValue>::new();
        for arg in args {
            arg_values.push(self.evaluate_expression(arg)?);
        }

        self.call_function(func, arg_values)
    }

    pub fn call_function(&mut self, func: &RFunction, mut args: Vec<CortexValue>) -> Result<CortexValue, CortexError> {
        let body = &func.body;
        let mut param_names = Vec::<String>::with_capacity(func.params.len());
        for param in &func.params {
            param_names.push(param.clone());
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
            let value = args.remove(0);
            let param_name = param_names.get(i).unwrap();
            new_env.add_var(param_name.clone(), value)?;
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

    fn evaluate_body(&mut self, body: &RBody) -> Result<CortexValue, CortexError> {
        match body {
            RBody::Interpreted(b) => {
                Ok(self.evaluate_interpreted_body(b)?)
            },
            RBody::Native(func) => {
                let res = func(self.current_env.as_ref().unwrap(), &mut self.heap)?;
                Ok(res)
            },
        }
    }
    fn evaluate_interpreted_body(&mut self, b: &RInterpretedBody) -> Result<CortexValue, CortexError> {
        for st in &b.statements {
            self.run_statement(st)?;
        }

        if let Some(return_expr) = &b.result {
            let res = self.evaluate_expression(return_expr)?;
            Ok(res)
        } else {
            Ok(CortexValue::Void)
        }
    }

    fn lookup_value(&self, path: &String) -> Result<CortexValue, CortexError> {
        Ok(self.current_env.as_ref().unwrap().get_value(path)?.clone())
    }

    fn lookup_function(&self, id: usize) -> Result<&Rc<RFunction>, CortexError> {
        Ok(self.preprocessor.get_function(id).unwrap())
    }
}
