use std::{cell::RefCell, collections::{HashMap, HashSet}, rc::Rc};

use crate::parsing::{ast::{expression::{Atom, BinaryOperator, Expression, ExpressionTail, OptionalIdentifier, PathIdent, UnaryOperator}, statement::Statement, top_level::{BasicBody, Body, Bundle, Function, TopLevel}, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};
use super::{env::Environment, error::{CortexError, InterpreterError}, heap::Heap, module::{CompositeType, Module, ModuleError}, type_env::TypeEnvironment, value::{CortexValue, ValueError}};

pub struct CortexInterpreter {
    base_module: Module,
    current_env: Option<Box<Environment>>,
    current_context: PathIdent,
    heap: Rc<RefCell<Heap>>,
    current_type_env: Option<Box<TypeEnvironment>>,
    global_module: Module,
}

impl CortexInterpreter {
    pub fn new() -> Result<Self, CortexError> {
        let mut this = CortexInterpreter {
            base_module: Module::new(),
            current_env: Some(Box::new(Environment::base())),
            current_context: PathIdent::empty(),
            heap: Rc::new(RefCell::new(Heap::new())),
            current_type_env: Some(Box::new(TypeEnvironment::base())),
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
        if let CortexValue::Composite { struct_name: _, field_values, type_args: _, type_arg_names: _ } = &*value_ref {
            for (_, fvalue) in field_values {
                self.find_reachables(current, fvalue.clone());
            }
        } else if let CortexValue::Reference(addr, _, _) = *value_ref {
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
                name, is_const, typ, initial_value 
            } => {
                match name {
                    OptionalIdentifier::Ident(ident) => {
                        let mut value = self.evaluate_expression(initial_value)?;
                        let true_type = if let Some(the_type) = typ {
                            // Check that the declared type and type of the result match
                            let value_type = self.determine_type(initial_value)?;
                            if !value_type.is_subtype_of(the_type) {
                                return Err(
                                    Box::new(
                                        InterpreterError::MismatchedType(
                                            the_type.codegen(0),
                                            value_type.codegen(0),
                                            ident.clone(),
                                        )
                                    )
                                );
                            }
                            the_type.clone()
                        } else {
                            self.determine_type(initial_value)?
                        };

                        if let CortexType::RefType { contained: _, mutable } = &true_type {
                            value = value.forward_mutability(*mutable);
                        }

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
            Statement::Assignment { name, value } => {
                if name.is_simple() {
                    let var_name = &name.base;
                    let assigned_type = self.determine_type(value)?;
                    let var_type = &self.current_env.as_ref().unwrap().get_type_of(var_name)?.clone();
                    if !assigned_type.is_subtype_of(var_type) {
                        return Err(
                            Box::new(
                                InterpreterError::MismatchedType(
                                    var_type.codegen(0),
                                    assigned_type.codegen(0),
                                    var_name.clone(),
                                )
                            )
                        );
                    }
                    let value = self.evaluate_expression(value)?;
                    self.current_env.as_mut().unwrap().set_value(var_name, value)?;
                    Ok(())
                } else {
                    let name_expr = name.clone().to_member_access_expr();
                    let var_type = self.determine_type(&name_expr)?;
                    let var_name = &name.base;
                    let chain = name.chain.clone();
                    let assigned_type = self.determine_type(value)?;
                    if !assigned_type.is_subtype_of(&var_type) {
                        return Err(
                            Box::new(
                                InterpreterError::MismatchedType(
                                    var_type.codegen(0),
                                    assigned_type.codegen(0),
                                    chain.last().unwrap().clone(),
                                )
                            )
                        );
                    }
                    let mut value = self.evaluate_expression(value)?;
                    if let CortexType::RefType { contained: _, mutable } = &var_type {
                        value = value.forward_mutability(*mutable);
                    }
                    let base = self.current_env.as_mut().unwrap().get_cell(var_name)?;
                    self.set_field_path(base, chain, value)?;
                    Ok(())
                }
            },
            Statement::WhileLoop(condition_body) => {
                if condition_body.body.has_result() {
                    return Err(Box::new(InterpreterError::LoopCannotHaveReturnValue));
                }
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
                    } else {
                        return Err(
                            Box::new(
                                InterpreterError::MismatchedType(
                                    String::from("bool"),
                                    cond.get_type().codegen(0),
                                    String::from("if condition")
                                )
                            )
                        );
                    }
                }
                Ok(())
            },
        }
    }

    pub fn determine_type(&self, expr: &Expression) -> Result<CortexType, CortexError> {
        Ok(self.clean_type(self.determine_type_expression(expr)?))
    }
    fn determine_type_expression(&self, primary: &Expression) -> Result<CortexType, CortexError> {
        let atom_result = self.determine_type_atom(&primary.atom)?;
        let tail_result = self.determine_type_tail(atom_result, &primary.tail)?;
        Ok(tail_result)
    }
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
            Atom::None => Ok(CortexType::none()),
            Atom::String(_) => Ok(CortexType::string(false)),
            Atom::PathIdent(path_ident) => Ok(self.lookup_type(path_ident)?),
            Atom::Call(path_ident, arg_exps) => {
                let func = self.lookup_function(path_ident)?;
                let mut return_type = func
                    .return_type
                    .clone()
                    .with_prefix_if_not_core(&self.current_context)
                    .with_prefix_if_not_core(&path_ident.without_last());
                let bindings = self.infer_type_args(&func, &arg_exps.iter().map(|a| self.determine_type(a)).collect::<Result<Vec<_>, _>>()?)?;
                return_type = TypeEnvironment::fill(return_type, &bindings);
                Ok(return_type)
            },
            Atom::Expression(expression) => Ok(self.determine_type(expression)?),
            Atom::Construction { name, type_args, assignments: _ } => {
                let composite = self.lookup_composite(name)?;
                let base_type = CortexType::basic(name.clone(), false, type_args.clone()).with_prefix_if_not_core(&self.current_context);
                if composite.is_heap_allocated {
                    Ok(CortexType::reference(base_type, true))
                } else {
                    Ok(base_type)
                }
            },
            Atom::IfStatement { first, conds, last } => {
                let cond_typ = self.determine_type(&first.condition)?;
                if cond_typ != CortexType::boolean(false) {
                    return Err(
                        Box::new(
                            InterpreterError::MismatchedType(
                                String::from("bool"),
                                cond_typ.codegen(0),
                                String::from("if condition"),
                            )
                        )
                    );
                }
                let mut the_type = self.determine_type_body(&first.body)?;
                
                for c in conds {
                    let cond_typ = self.determine_type(&c.condition)?;
                    if cond_typ != CortexType::boolean(false) {
                        return Err(
                            Box::new(
                                InterpreterError::MismatchedType(
                                    String::from("bool"),
                                    cond_typ.codegen(0),
                                    String::from("while condition"),
                                )
                            )
                        );
                    }
                    let typ = self.determine_type_body(&c.body)?;
                    let the_type_str = the_type.codegen(0);
                    let typ_str = typ.codegen(0);
                    let next = the_type.combine_with(typ);
                    if let Some(t) = next {
                        the_type = t;
                    } else {
                        return Err(Box::new(InterpreterError::IfArmsDoNotMatch(the_type_str, typ_str)));
                    }
                }
                if let Some(fin) = last {
                    let typ = self.determine_type_body(fin)?;
                    let the_type_str = the_type.codegen(0);
                    let typ_str = typ.codegen(0);
                    let next = the_type.combine_with(typ);
                    if let Some(t) = next {
                        the_type = t;
                    } else {
                        return Err(Box::new(InterpreterError::IfArmsDoNotMatch(the_type_str, typ_str)));
                    }
                } else if the_type != CortexType::void(false) {
                    return Err(Box::new(InterpreterError::IfRequiresElseBlock));
                }

                Ok(the_type)
            },
            Atom::UnaryOperation { op, exp } => {
                let typ = self.determine_type(exp)?;
                match op {
                    UnaryOperator::Negate => {
                        if typ == CortexType::number(false) {
                            Ok(CortexType::number(false))
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperatorUnary("number")))
                        }
                    },
                    UnaryOperator::Invert => {
                        if typ == CortexType::boolean(false) {
                            Ok(CortexType::boolean(false))
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperatorUnary("bool")))
                        }
                    },
                }
            },
            Atom::ListLiteral(items) => {
                let mut typ = CortexType::Unknown(false);
                for item in items {
                    let item_type = self.determine_type(item)?;
                    let item_type_str = item_type.codegen(0);
                    let typ_str = typ.codegen(0);
                    typ = typ
                        .combine_with(item_type)
                        .ok_or(InterpreterError::CannotDetermineListLiteralType(typ_str, item_type_str))?;
                }
                let true_type = CortexType::reference(CortexType::list(typ, false), true);
                Ok(true_type)
            },
        }
    }
    fn determine_type_tail(&self, atom: CortexType, tail: &ExpressionTail) -> Result<CortexType, CortexError> {
        match tail {
            ExpressionTail::None => Ok(atom),
            ExpressionTail::PostfixBang { next } => {
                let new_type = atom.to_non_optional();
                Ok(self.determine_type_tail(new_type, next)?)
            },
            ExpressionTail::MemberAccess { member, next } => {
                let composite = self.lookup_composite(atom.name()?)?;
                if !composite.fields.contains_key(member) {
                    Err(Box::new(ValueError::FieldDoesNotExist(member.clone(), atom.codegen(0))))
                } else {
                    let mut member_type = composite.fields.get(member).unwrap().clone();
                    let bindings = Self::get_bindings(&composite.type_param_names, &atom)?;
                    member_type = TypeEnvironment::fill(member_type, &bindings);
                    member_type = member_type.with_prefix_if_not_core(&atom.prefix());
                    member_type = self.determine_type_tail(member_type, next)?;
                    Ok(member_type)
                }
            },
            ExpressionTail::MemberCall { member, args, next } => {
                let caller_type = atom.name()?;
                let caller_func_prefix = caller_type.without_last();
                let caller_func_base = caller_type.get_back()?;
                let member_func_name = Bundle::get_bundle_func_name(caller_func_base, member);
                let member_func_path = PathIdent::continued(caller_func_prefix.clone(), member_func_name)
                    .subtract(&self.current_context)?;
                let func = self.lookup_function(&member_func_path)?;
                let mut return_type = func.return_type.clone();

                let mut arg_types = args.iter().map(|a| self.determine_type(a)).collect::<Result<Vec<_>, _>>()?;
                arg_types.insert(0, atom.clone());
                let bindings = self.infer_type_args(&func, &arg_types)?;
                return_type = TypeEnvironment::fill(return_type, &bindings);

                return_type = self.determine_type_tail(return_type, next)?;
                return_type = return_type
                    .with_prefix_if_not_core(&self.current_context)
                    .with_prefix_if_not_core(&caller_func_prefix);
                Ok(return_type)
            },
            ExpressionTail::BinOp { op, right, next } => {
                let op_type = self.determine_type_operator(atom, op, self.determine_type(right)?)?;
                let next_type = self.determine_type_tail(op_type, next)?;
                Ok(next_type)
            },
        }
    }
    fn determine_type_body(&self, body: &BasicBody) -> Result<CortexType, CortexError> {
        if let Some(expr) = &body.result {
            self.determine_type(expr)
        } else {
            Ok(CortexType::void(false))
        }
    }

    pub fn evaluate_expression(&mut self, primary: &Expression) -> Result<CortexValue, CortexError> {
        let atom_result = self.evaluate_atom(&primary.atom)?;
        let tail_result = self.handle_expr_tail(atom_result, &primary.tail)?;
        Ok(tail_result)
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
        let _atom_type = self.determine_type_atom(atom)?;
        match atom {
            Atom::Boolean(v) => Ok(CortexValue::Boolean(*v)),
            Atom::Number(v) => Ok(CortexValue::Number(*v)),
            Atom::String(v) => Ok(CortexValue::String(v.clone())),
            Atom::Void => Ok(CortexValue::Void),
            Atom::None => Ok(CortexValue::None),
            Atom::Expression(expr) => Ok(self.evaluate_expression(expr)?),
            Atom::PathIdent(path) => Ok(self.lookup_value(path)?),
            Atom::Call(path_ident, expressions) => {
                let func = self.lookup_function(path_ident)?;
                let context_to_return_to = std::mem::replace(&mut self.current_context, path_ident.without_last());
                let func_result = self.run_function(&func, expressions.iter().collect());
                self.current_context = context_to_return_to;
                Ok(func_result?)
            },
            Atom::Construction { name, type_args, assignments } => {
                let composite = self.lookup_composite(name)?;
                if !composite.is_heap_allocated {
                    Ok(self.construct_struct(name, assignments, &composite.fields, &composite.type_param_names, type_args)?)
                } else {
                    let value = self.construct_struct(name, assignments, &composite.fields, &composite.type_param_names, type_args)?;
                    let typ = value.get_type();
                    let addr = self.allocate(value);
                    Ok(CortexValue::Reference(addr, typ, true))
                }
            },
            Atom::IfStatement { first, conds, last } => {
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
                                return Err(
                                    Box::new(
                                        InterpreterError::MismatchedType(
                                            String::from("bool"),
                                            self.determine_type(&first.condition)?.codegen(0),
                                            String::from("if condition")
                                        )
                                    )
                                );
                            }
                        }
                        if let Some(c) = last {
                            Ok(self.evaluate_body(&Body::Basic(*c.clone()))?)
                        } else {
                            // If all arms return void, then we're fine to return void here
                            // Otherwise, need to throw an error
                            if self.determine_type_body(&first.body)? != CortexType::void(false) {
                                Err(Box::new(InterpreterError::IfRequiresElseBlock))
                            } else {
                                for c in conds {
                                    if self.determine_type_body(&c.body)? != CortexType::void(false) {
                                        return Err(Box::new(InterpreterError::IfRequiresElseBlock));
                                    }
                                }
                                // If we've made it here, all arms return void
                                Ok(CortexValue::Void)
                            }
                        }
                    }
                } else {
                    Err(
                        Box::new(
                            InterpreterError::MismatchedType(
                                String::from("bool"),
                                self.determine_type(&first.condition)?.codegen(0),
                                String::from("if condition")
                            )
                        )
                    )
                }
            },
            Atom::UnaryOperation { op, exp } => {
                let val = self.evaluate_expression(exp)?;
                match op {
                    UnaryOperator::Negate => {
                        if let CortexValue::Number(n) = val {
                            Ok(CortexValue::Number(-n))
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperatorUnary("number")))
                        }
                    },
                    UnaryOperator::Invert => {
                        if let CortexValue::Boolean(b) = val {
                            Ok(CortexValue::Boolean(!b))
                        } else {
                            Err(Box::new(InterpreterError::InvalidOperatorUnary("bool")))
                        }
                    },
                }
            },
            Atom::ListLiteral(items) => {
                let values = items
                    .iter()
                    .map(|e| self.evaluate_expression(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut typ = CortexType::Unknown(false);
                for item in items {
                    let item_type = self.determine_type(item)?;
                    let item_type_str = item_type.codegen(0);
                    let typ_str = typ.codegen(0);
                    typ = typ
                        .combine_with(item_type)
                        .ok_or(InterpreterError::CannotDetermineListLiteralType(typ_str, item_type_str))?;
                }
                let addr = self.heap.borrow_mut().allocate(CortexValue::List(values, typ.clone()));
                Ok(CortexValue::Reference(addr, CortexType::list(typ, false), true))
            },
        }
    }
    fn handle_expr_tail(&mut self, atom: CortexValue, tail: &ExpressionTail) -> Result<CortexValue, CortexError> {
        match tail {
            ExpressionTail::None => Ok(atom),
            ExpressionTail::PostfixBang { next } => {
                if let CortexValue::None = atom {
                    Err(Box::new(InterpreterError::BangCalledOnNoneValue))
                } else {
                    Ok(self.handle_expr_tail(atom, next)?)
                }
            },
            ExpressionTail::MemberAccess { member, next } => {
                if let CortexValue::Reference(addr, _, mutable) = atom {
                    let val = self.heap
                        .borrow()
                        .get(addr)
                        .borrow()
                        .get_field(member)?
                        .borrow()
                        .clone()
                        .forward_mutability(mutable);
                    Ok(self.handle_expr_tail(val, next)?)
                } else {
                    Ok(self.handle_expr_tail(atom.get_field(member)?.borrow().clone(), next)?)
                }
            },
            ExpressionTail::MemberCall { member, args, next } => {
                let atom_type = atom.get_type();
                let caller_type = atom_type.name()?;
                let caller_func_prefix = caller_type.without_last();
                let caller_func_base = caller_type.get_back()?;
                let member_func_name = Bundle::get_bundle_func_name(caller_func_base, member);
                let member_func_path = PathIdent::continued(caller_func_prefix.clone(), member_func_name)
                    .subtract(&self.current_context)?;
                let func = self.lookup_function(&member_func_path)?;
                let mut args = args
                    .iter()
                    .map(|e| self.evaluate_expression(e))
                    .collect::<Result<Vec<CortexValue>, _>>()?;
                args.insert(0, atom);
                
                let context_to_return_to = std::mem::replace(&mut self.current_context, caller_func_prefix);
                let result = self.call_function(&func, args);
                self.current_context = context_to_return_to;

                Ok(self.handle_expr_tail(result?, next)?)
            },
            ExpressionTail::BinOp { op, right, next } => {
                let right = self.evaluate_expression(right)?;
                let result = self.evaluate_op(atom, op, right)?;
                Ok(self.handle_expr_tail(result, next)?)
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
        name: &PathIdent,
        assignments: &Vec<(String, Expression)>,
        fields: &HashMap<String, CortexType>,
        type_param_names: &Vec<String>,
        type_args: &Vec<CortexType>,
    ) -> Result<CortexValue, CortexError> {
        if type_args.len() != type_param_names.len() {
            return Err(Box::new(InterpreterError::MismatchedTypeArgCount(name.codegen(0), type_param_names.len(), type_args.len())));
        }
        let mut fields_to_assign = Vec::new();
        for k in fields.keys() {
            fields_to_assign.push(k.clone());
        }
        let mut values = HashMap::<String, Rc<RefCell<CortexValue>>>::new();
        let bindings = TypeEnvironment::create_bindings(type_param_names, type_args);
        for (fname, value) in assignments {
            let opt_typ = fields
                .get(fname)
                .map(|t| t.clone());
            if let Some(typ) = opt_typ {
                let typ = TypeEnvironment::fill(typ, &bindings)
                    .with_prefix_if_not_core(&self.current_context)
                    .with_prefix_if_not_core(&name.without_last());
                let assigned_type = self.determine_type(value)?;
                if !assigned_type.is_subtype_of(&typ) {
                    return Err(
                        Box::new(
                            InterpreterError::MismatchedType(
                                typ.codegen(0),
                                assigned_type.codegen(0),
                                fname.clone(),
                            )
                        )
                    );
                }
                let assigned_value = self.evaluate_expression(value)?;
                values.insert(fname.clone(), Rc::new(RefCell::new(assigned_value)));
                let index_opt = fields_to_assign.iter().position(|x| *x == *fname);
                if let Some(index) = index_opt {
                    fields_to_assign.remove(index);
                } else {
                    return Err(Box::new(InterpreterError::MultipleFieldAssignment(fname.clone())));
                }
            } else {
                return Err(Box::new(ValueError::FieldDoesNotExist(fname.clone(), name.codegen(0))));
            }
        }
        if fields_to_assign.is_empty() {
            Ok(
                CortexValue::Composite {
                    struct_name: PathIdent::concat(&self.current_context, name),
                    field_values: values,
                    type_arg_names: type_param_names.clone(),
                    type_args: type_args.clone(),
                }
            )
        } else {
            Err(Box::new(InterpreterError::NotAllFieldsAssigned(name.codegen(0), fields_to_assign.join(","))))
        }
    }
    
    fn set_field_path(&mut self, mut base: Rc<RefCell<CortexValue>>, mut path: Vec<String>, value: CortexValue) -> Result<(), ValueError> {
        let first_option = path.get(0);
        if let CortexValue::Reference(addr, _, mutable) = &*base.clone().borrow() {
            if !mutable {
                return Err(ValueError::CannotModifyNonMutableReference);
            }
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

    // Used to get bindings for a type (give param names and the concrete type)
    fn get_bindings(type_param_names: &Vec<String>, typ: &CortexType) -> Result<HashMap<String, CortexType>, CortexError> {
        let mut type_args_handled = false;
        let mut typ = typ.clone();
        let mut bindings = HashMap::new();
        while !type_args_handled {
            if let CortexType::BasicType { optional: _, name: _, type_args } = &typ {
                bindings = TypeEnvironment::create_bindings(type_param_names, type_args);
                typ = TypeEnvironment::fill(typ, &bindings);
                type_args_handled = true;
            } else if let CortexType::RefType { contained, mutable: _} = typ {
                typ = *contained;
            }
        }
        Ok(bindings)
    }
    fn infer_type_args(&self, func: &Rc<Function>, args: &Vec<CortexType>) -> Result<HashMap<String, CortexType>, CortexError> {
        let mut bindings = HashMap::<String, CortexType>::new();
        for (arg, param) in args.iter().zip(&func.params) {
            self.infer_arg(&param.typ, &arg, &func.type_param_names, &mut bindings, param.name())?;
        }

        if bindings.len() != func.type_param_names.len() {
            Err(Box::new(InterpreterError::CouldNotInferTypeBinding(func.name.codegen(0))))
        } else {
            Ok(bindings)
        }
    }
    fn infer_arg(&self, param_type: &CortexType, arg_type: &CortexType, type_param_names: &Vec<String>, bindings: &mut HashMap<String, CortexType>, param_name: &String) -> Result<(), CortexError> {
        let correct;
        match (&param_type, arg_type) {
            (CortexType::BasicType { optional, name: _, type_args }, arg_type) => {
                if let Some(name) = TypeEnvironment::does_arg_list_contain(type_param_names, &param_type) {
                    // If we take in a T? and passing a number?, then we want T = number, not T = number?
                    let mut bound_type = arg_type.clone();
                    if *optional {
                        bound_type = bound_type.to_non_optional();
                    }
                    if type_args.len() > 0 {
                        return Err(Box::new(InterpreterError::CannotHaveTypeArgsOnGeneric(param_type.codegen(0))));
                    }
                    if let Some(existing_binding) = bindings.get(name) {
                        let combined = bound_type.combine_with(existing_binding.clone());
                        if let Some(result) = combined {
                            bindings.insert(name.clone(), result);
                            correct = true;
                        } else {
                            correct = false;
                        }
                    } else {
                        bindings.insert(name.clone(), bound_type);
                        correct = true;
                    }
                } else {
                    // Try to match up type args (ex. list<T> to list<number>)
                    // If both are not BasicType, then we just ignore this
                    if let CortexType::BasicType { optional: _, name: _, type_args: type_args2 } = arg_type {
                        if type_args.len() == type_args2.len() {
                            for (type_param, type_arg) in type_args.iter().zip(type_args2) {
                                self.infer_arg(type_param, type_arg, type_param_names, bindings, param_name)?;
                            }
                            correct = true;
                        } else {
                            correct = false;
                        }
                    } else {
                        correct = true;
                    }
                }
            },
            (CortexType::RefType { contained, mutable: _ }, CortexType::RefType { contained: contained2, mutable: _ }) => {
                self.infer_arg(contained, contained2, type_param_names, bindings, param_name)?;
                correct = true;
            },
            (CortexType::RefType { contained: _, mutable: _ }, _) => {
                // parameter is reference but arg is not a reference
                correct = false;
            },
            (_, _) => {
                correct = false;
            },
        }
        if correct {
            Ok(())
        } else {
            Err(Box::new(InterpreterError::MismatchedType(param_type.codegen(0), arg_type.codegen(0), param_name.clone())))
        }
    }

    // "Cleans" type, for example replacing type arguments
    fn clean_type(&self, typ: CortexType) -> CortexType {
        self.current_type_env.as_ref().unwrap().fill_in(typ)
    }

    pub fn call_function(&mut self, func: &Rc<Function>, mut args: Vec<CortexValue>) -> Result<CortexValue, CortexError> {
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

        let type_bindings = self.infer_type_args(func, &args.iter().map(|a| a.get_type()).collect())?;
        let parent_type_env = self.current_type_env.take().ok_or(InterpreterError::NoParentEnv)?;
        let mut new_type_env = TypeEnvironment::new(*parent_type_env);
        for (name, typ) in type_bindings {
            new_type_env.add(name, typ);
        }
        self.current_type_env = Some(Box::new(new_type_env));

        for (i, arg) in args.iter().enumerate() {
            let arg_type = self.clean_type(arg.get_type());
            let param_type = self.clean_type(param_types.get(i).unwrap().clone().with_prefix_if_not_core(&self.current_context));
            if !arg_type.is_subtype_of(&param_type) {
                return Err(
                    Box::new(
                        InterpreterError::MismatchedType(
                            param_type.codegen(0),
                            arg_type.codegen(0),
                            param_names.get(i).unwrap().clone(),
                        )
                    )
                );
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
            let value = args.remove(0);
            let param_name = param_names.get(i).unwrap();
            let param_type = self.clean_type(param_types.remove(0));
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
        self.current_type_env = Some(Box::new(
            self.current_type_env
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

    fn lookup_type(&self, path: &PathIdent) -> Result<CortexType, CortexError> {
        if path.is_final() {
            // Search in our environment for it
            Ok(self.current_env.as_ref().unwrap().get_type_of(path.get_front()?)?.clone())
        } else {
            Err(Box::new(InterpreterError::ValueNotFound(path.codegen(0))))
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
        let module = self.base_module.get_module_for(&PathIdent::concat(&self.current_context, path))?;
        let result = module.get_function(last);
        match result {
            Ok(f) => Ok(f),
            Err(ModuleError::FunctionDoesNotExist(_)) => Ok(self.global_module.get_function(last)?),
            Err(e) => Err(Box::new(e)),
        }
    }
    fn lookup_composite(&self, path: &PathIdent) -> Result<Rc<CompositeType>, CortexError> {
        let last = path.get_back()?;
        let module = self.base_module.get_module_for(&PathIdent::concat(&self.current_context, path))?;
        let result = module.get_composite(last);
        match result {
            Ok(f) => Ok(f),
            Err(ModuleError::TypeDoesNotExist(_)) => Ok(self.global_module.get_composite(last)?),
            Err(e) => Err(Box::new(e)),
        }
    }
}
