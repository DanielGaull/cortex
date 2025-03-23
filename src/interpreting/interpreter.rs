use std::{cell::RefCell, collections::{HashMap, HashSet}, rc::Rc};

use crate::parsing::{ast::{expression::{Expression, BinaryOperator, OptionalIdentifier, PathIdent, UnaryOperator}, statement::Statement, top_level::{Body, Function, TopLevel}}, codegen::r#trait::SimpleCodeGen};
use super::{env::Environment, error::{CortexError, InterpreterError}, heap::Heap, module::{Module, ModuleError}, value::{CortexValue, ValueError}};

pub struct CortexInterpreter {
    base_module: Module,
    current_env: Option<Box<Environment>>,
    heap: Rc<RefCell<Heap>>,
    global_module: Module,
}

impl CortexInterpreter {
    pub fn new() -> Result<Self, CortexError> {
        let mut this = CortexInterpreter {
            base_module: Module::new(),
            current_env: Some(Box::new(Environment::base())),
            heap: Rc::new(RefCell::new(Heap::new())),
            global_module: Module::new(),
        };

        Self::add_list_funcs(&mut this.global_module, this.heap.clone())?;
        Self::add_string_funcs(&mut this.global_module, this.heap.clone())?;

        Ok(this)
    }

    pub fn gc(&mut self) {
        // Note: "roots" are the addresses of all pointers currently in memory
        // It also includes pointers that reside within struct fields currently in memory,
        // and those struct's fields, etc.
        let mut roots = HashSet::<usize>::new();
        if let Some(env) = &self.current_env {
            env.foreach(|_name, value| self.find_reachables(&mut roots, Rc::new(RefCell::new(value.clone()))));
        }
        self.heap.borrow_mut().gc(roots);
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
        self.heap.borrow().sz()
    }

    pub fn register_module(&mut self, path: &PathIdent, module: Module) -> Result<(), CortexError> {
        self.base_module.add_module(path, module)?;
        Ok(())
    }

    pub fn run_top_level(&mut self, top_level: TopLevel) -> Result<(), CortexError> {
        match top_level {
            TopLevel::Import { name: _, is_string_import: _ } => {
                todo!("Imports are currently not supported!")
            },
            TopLevel::Module { name, contents } => {
                let module = Self::construct_module(contents)?;
                self.register_module(&PathIdent::simple(name), module)?;
                Ok(())
            },
            TopLevel::Function(function) => {
                self.base_module.add_function(function)?;
                Ok(())
            },
            TopLevel::Struct(struc) => {
                self.base_module.add_struct(struc)?;
                Ok(())
            },
            TopLevel::Bundle(bundle) => {
                self.base_module.add_bundle(bundle)?;
                Ok(())
            },
        }
    }

    fn construct_module(contents: Vec<TopLevel>) -> Result<Module, CortexError> {
        let mut module = Module::new();
        for item in contents.into_iter() {
            match item {
                TopLevel::Import { name: _, is_string_import: _ } => todo!("Imports are currently not supported!"),
                TopLevel::Module { name: submod_name, contents } => {
                    let new_module = Self::construct_module(contents)?;
                    module.add_child(submod_name, new_module)?;
                },
                TopLevel::Function(function) => {
                    module.add_function(function)?;
                },
                TopLevel::Struct(item) => {
                    module.add_struct(item)?;
                },
                TopLevel::Bundle(item) => {
                    module.add_bundle(item)?;
                },
            }
        }
        Ok(module)
    }

    pub fn run_statement(&mut self, statement: &Statement) -> Result<(), CortexError> {
        match statement {
            Statement::Expression(expression) => {
                self.evaluate_expression(expression)?;
                Ok(())
            },
            Statement::Throw(expr) => {
                let val = self.evaluate_expression(expr)?;
                Err(Box::new(InterpreterError::ProgramThrow(val)))
            },
            Statement::VariableDeclaration { 
                name, is_const, typ: _, initial_value 
            } => {
                match name {
                    OptionalIdentifier::Ident(ident) => {
                        let value = self.evaluate_expression(initial_value)?;
                        if *is_const {
                            self.current_env.as_mut().unwrap().add_const(ident.clone(), value)?;
                        } else {
                            self.current_env.as_mut().unwrap().add_var(ident.clone(), value)?;
                        }
                        Ok(())
                    },
                    OptionalIdentifier::Ignore => {
                        self.evaluate_expression(initial_value)?;
                        Ok(())
                    },
                }
            },
            Statement::Assignment { name, value } => {
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
            Statement::WhileLoop(condition_body) => {
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
    pub fn evaluate_expression(&mut self, exp: &Expression) -> Result<CortexValue, CortexError> {
        match exp {
            Expression::Boolean(v) => Ok(CortexValue::Boolean(*v)),
            Expression::Number(v) => Ok(CortexValue::Number(*v)),
            Expression::String(v) => Ok(CortexValue::String(v.clone())),
            Expression::Void => Ok(CortexValue::Void),
            Expression::None => Ok(CortexValue::None),
            Expression::PathIdent(path) => Ok(self.lookup_value(path)?),
            Expression::Call(path_ident, expressions) => {
                let func = self.lookup_function(path_ident)?;
                let func_result = self.run_function(&func, expressions.iter().collect());
                Ok(func_result?)
            },
            Expression::Construction { name: _, type_args: _, assignments } => {
                let is_heap_allocated = &false;
                if !*is_heap_allocated {
                    Ok(self.construct_struct(assignments)?)
                } else {
                    let value = self.construct_struct(assignments)?;
                    let addr = self.allocate(value);
                    Ok(CortexValue::Reference(addr))
                }
            },
            Expression::IfStatement { first, conds, last } => {
                let cond = self.evaluate_expression(&first.condition)?;
                if let CortexValue::Boolean(b) = cond {
                    if b {
                        self.evaluate_body(&Body::Basic(first.body.clone()))
                    } else {
                        for c in conds {
                            let cond = self.evaluate_expression(&c.condition)?;
                            if let CortexValue::Boolean(b) = cond {
                                if b {
                                    return Ok(self.evaluate_body(&Body::Basic(c.body.clone()))?);
                                }
                            } else {
                                return Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess));
                            }
                        }
                        if let Some(c) = last {
                            Ok(self.evaluate_body(&Body::Basic(*c.clone()))?)
                        } else {
                            Ok(CortexValue::Void)
                        }
                    }
                } else {
                    Err(Box::new(InterpreterError::MismatchedTypeNoPreprocess))
                }
            },
            Expression::UnaryOperation { op, exp } => {
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
            Expression::ListLiteral(items) => {
                let values = items
                    .iter()
                    .map(|e| self.evaluate_expression(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let addr = self.heap.borrow_mut().allocate(CortexValue::List(values));
                Ok(CortexValue::Reference(addr))
            },
            Expression::Bang(inner) => {
                let inner = self.evaluate_expression(inner)?;
                if let CortexValue::None = inner {
                    Err(Box::new(InterpreterError::BangCalledOnNoneValue))
                } else {
                    Ok(inner)
                }
            },
            Expression::MemberAccess(inner, member) => {
                let inner = self.evaluate_expression(inner)?;
                if let CortexValue::Reference(addr) = inner {
                    let val = self.heap
                        .borrow()
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
            Expression::MemberCall { callee: _, member: _, args: _ } => {
                Err(Box::new(InterpreterError::InvalidObject("expression")))
            },
            Expression::BinaryOperation { left, op, right } => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;
                let result = self.evaluate_op(left, op, right)?;
                Ok(result)
            },
        }
    }

    fn allocate(&mut self, value: CortexValue) -> usize {
        let ptr = self.heap.borrow_mut().allocate(value);
        if self.heap.borrow().is_at_gc_threshold() {
            self.gc();
        }
        ptr
    }
    fn construct_struct(
        &mut self,
        assignments: &Vec<(String, Expression)>,
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
            base = self.heap.borrow().get(*addr);
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

    fn run_function(&mut self, func: &Rc<Function>, args: Vec<&Expression>) -> Result<CortexValue, CortexError> {
        let mut arg_values = Vec::<CortexValue>::new();
        for arg in args {
            arg_values.push(self.evaluate_expression(arg)?);
        }

        self.call_function(func, arg_values)
    }

    pub fn call_function(&mut self, func: &Rc<Function>, mut args: Vec<CortexValue>) -> Result<CortexValue, CortexError> {
        let body = &func.body;
        let mut param_names = Vec::<String>::with_capacity(func.params.len());
        for param in &func.params {
            param_names.push(param.name.clone());
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

    fn evaluate_body(&mut self, body: &Body) -> Result<CortexValue, CortexError> {
        match body {
            Body::Basic(b) => {
                for st in &b.statements {
                    self.run_statement(st)?;
                }
        
                if let Some(return_expr) = &b.result {
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

    fn lookup_value(&self, path: &PathIdent) -> Result<CortexValue, CortexError> {
        if path.is_final() {
            // Search in our environment for it
            Ok(self.current_env.as_ref().unwrap().get_value(path.get_front()?)?.clone())
        } else {
            Err(Box::new(InterpreterError::ValueNotFound(path.codegen(0))))
        }
    }

    fn lookup_function(&self, path: &PathIdent) -> Result<Rc<Function>, CortexError> {
        let last = path.get_back()?;
        let module = self.base_module.get_module_for(path)?;
        let result = module.get_function(last);
        match result {
            Ok(f) => Ok(f),
            Err(ModuleError::FunctionDoesNotExist(_)) => Ok(self.global_module.get_function(last)?),
            Err(e) => Err(Box::new(e)),
        }
    }
}
