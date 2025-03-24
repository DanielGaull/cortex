use std::{collections::HashMap, error::Error, rc::Rc};

use crate::parsing::{ast::{expression::{BinaryOperator, ConditionBody, Expression, OptionalIdentifier, PathIdent, UnaryOperator}, statement::Statement, top_level::{BasicBody, Body, Bundle, Function, TopLevel}, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};

use super::{ast::{expression::RExpression, function::{FunctionDict, RBody, RFunction, RInterpretedBody}, statement::{RConditionBody, RStatement}}, error::PreprocessingError, module::{CompositeType, Module, ModuleError}, program::Program, type_checking_env::TypeCheckingEnvironment, type_env::TypeEnvironment};

type CortexError = Box<dyn Error>;
pub type CheckResult<T> = Result<(T, CortexType), CortexError>;

pub struct CortexPreprocessor {
    current_env: Option<Box<TypeCheckingEnvironment>>,
    base_module: Module,
    current_context: PathIdent,
    current_type_env: Option<Box<TypeEnvironment>>,
    global_module: Module,
    function_dict: FunctionDict,
}

impl CortexPreprocessor {
    pub fn new() -> Result<Self, CortexError> {
        let mut this = CortexPreprocessor {
            base_module: Module::new(),
            current_env: Some(Box::new(TypeCheckingEnvironment::base())),
            current_context: PathIdent::empty(),
            current_type_env: Some(Box::new(TypeEnvironment::base())),
            global_module: Module::new(),
            function_dict: FunctionDict::new(),
        };

        Self::add_list_funcs(&mut this.global_module)?;
        Self::add_string_funcs(&mut this.global_module)?;

        Ok(this)
    }

    pub fn register_module(&mut self, path: &PathIdent, module: Module) -> Result<(), CortexError> {
        self.base_module.add_module(path, module)?;
        Ok(())
    }

    pub(crate) fn get_function(&self, id: usize) -> Option<&Rc<RFunction>> {
        self.function_dict.get(id)
    }

    pub(crate) fn determine_type(&mut self, expr: Expression) -> Result<CortexType, CortexError> {
        let (_, typ) = self.check_exp(expr)?;
        Ok(typ)
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

    pub fn preprocess(&mut self, body: BasicBody) -> Result<Program, CortexError> {
        let (body, _) = self.check_body(body)?;

        for p in self.function_dict.referenced_functions() {
            let f = self.take_function(&p)?;
            let processed = self.check_function(f)?;
            self.function_dict.add_function(p, processed);
        }

        Ok(Program { code: body })
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

    fn check_function(&mut self, function: Function) -> Result<RFunction, CortexError> {
        let parent_env = self.current_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        let mut new_env = TypeCheckingEnvironment::new(*parent_env);
        let mut params = Vec::new();
        for p in function.params {
            let param_type = self.clean_type(p.typ.clone().with_prefix_if_not_core(&self.current_context));
            new_env.add(p.name.clone(), param_type, false)?;
            params.push(p.name);
        }
        self.current_env = Some(Box::new(new_env));

        // Do check body
        let final_fn_body;
        match function.body {
            Body::Basic(body) => {
                let (new_body, body_type) = self.check_body(body)?;
                if !body_type.is_subtype_of(&function.return_type) {
                    return Err(Box::new(PreprocessingError::ReturnTypeMismatch(function.return_type.codegen(0), body_type.codegen(0))));
                }
                final_fn_body = RBody::Interpreted(new_body);
            },
            Body::Native(native_body) => {
                final_fn_body = RBody::Native(native_body);
            },
        }

        self.current_env = Some(Box::new(self.current_env.take().unwrap().exit()?));

        Ok(RFunction::new(params, final_fn_body))
    }

    fn check_statement(&mut self, statement: Statement) -> Result<RStatement, CortexError> {
        match statement {
            Statement::Expression(expression) => {
                let (exp, _) = self.check_exp(expression)?;
                Ok(RStatement::Expression(exp))
            },
            Statement::Throw(expression) => {
                let (exp, _) = self.check_exp(expression)?;
                Ok(RStatement::Throw(exp))
            },
            Statement::VariableDeclaration { name, is_const, typ, initial_value } => {
                match name {
                    OptionalIdentifier::Ident(ident) => {
                        let (assigned_exp, assigned_type) = self.check_exp(initial_value)?;
                        let type_of_var = if let Some(declared_type) = typ {
                            if !assigned_type.is_subtype_of(&declared_type) {
                                return Err(
                                    Box::new(
                                        PreprocessingError::MismatchedType(
                                            declared_type.codegen(0),
                                            assigned_type.codegen(0),
                                            ident.clone(),
                                        )
                                    )
                                );
                            }
                            declared_type.clone()
                        } else {
                            assigned_type
                        };

                        self.current_env.as_mut().unwrap().add(ident.clone(), type_of_var, is_const)?;

                        Ok(RStatement::VariableDeclaration { name: ident, is_const: is_const, initial_value: assigned_exp })
                    },
                    OptionalIdentifier::Ignore => {
                        let (exp, _) = self.check_exp(initial_value)?;
                        Ok(RStatement::Expression(exp))
                    },
                }
            },
            Statement::Assignment { name, value } => {
                let (assigned_exp, assigned_type) = self.check_exp(value)?;
                if name.is_simple() {
                    let var_name = &name.base;
                    let var_type = &self.current_env.as_ref().unwrap().get(var_name)?.clone();
                    if !assigned_type.is_subtype_of(var_type) {
                        return Err(
                            Box::new(
                                PreprocessingError::MismatchedType(
                                    var_type.codegen(0),
                                    assigned_type.codegen(0),
                                    var_name.clone(),
                                )
                            )
                        );
                    }
                    
                    if self.current_env.as_ref().unwrap().is_const(var_name)? {
                        return Err(
                            Box::new(
                                PreprocessingError::CannotModifyConst(var_name.clone())
                            )
                        )
                    }
                } else {
                    let name_expr = name.clone().to_member_access_expr();
                    let (_, var_type) = self.check_exp(name_expr)?;
                    let chain = name.chain.clone();
                    if !assigned_type.is_subtype_of(&var_type) {
                        return Err(
                            Box::new(
                                PreprocessingError::MismatchedType(
                                    var_type.codegen(0),
                                    assigned_type.codegen(0),
                                    chain.last().unwrap().clone(),
                                )
                            )
                        );
                    }
                }

                Ok(RStatement::Assignment { name: name.into(), value: assigned_exp })
            },
            Statement::WhileLoop(condition_body) => {
                let (cond, cond_type) = self.check_exp(condition_body.condition)?;
                if !cond_type.is_subtype_of(&CortexType::boolean(false)) {
                    return Err(
                        Box::new(
                            PreprocessingError::MismatchedType(
                                String::from("bool"),
                                cond_type.codegen(0),
                                String::from("while condition"),
                            )
                        )
                    );
                }

                let (body, body_type) = self.check_body(condition_body.body)?;
                if !body_type.is_subtype_of(&CortexType::void(false)) {
                    return Err(
                        Box::new(
                            PreprocessingError::LoopCannotHaveReturnValue
                        )
                    );
                }

                Ok(RStatement::WhileLoop(RConditionBody::new(cond, body)))
            },
        }
    }

    fn check_exp(&mut self, exp: Expression) -> CheckResult<RExpression> {
        match exp {
            Expression::Number(v) => Ok((RExpression::Number(v), CortexType::number(false))),
            Expression::Boolean(v) => Ok((RExpression::Boolean(v), CortexType::boolean(false))),
            Expression::Void => Ok((RExpression::Void, CortexType::void(false))),
            Expression::None => Ok((RExpression::None, CortexType::none())),
            Expression::String(v) => Ok((RExpression::String(v), CortexType::string(false))),
            Expression::PathIdent(path_ident) => Ok((RExpression::Identifier(path_ident.get_back()?.clone()), self.lookup_type(&path_ident)?)),
            Expression::Call(path_ident, arg_exps) => {
                self.check_call(path_ident, arg_exps)
            },
            Expression::Construction { name, type_args, assignments } => {
                self.check_construction(name, type_args, assignments)
            },
            Expression::IfStatement { first, conds, last } => {
                self.check_if_statement(*first, conds, last.map(|b| *b))
            },
            Expression::UnaryOperation { op, exp } => {
                let (exp, typ) = self.check_exp(*exp)?;
                match op {
                    UnaryOperator::Negate => {
                        if typ == CortexType::number(false) {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Negate, exp: Box::new(exp) }, CortexType::number(false)))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("number")))
                        }
                    },
                    UnaryOperator::Invert => {
                        if typ == CortexType::boolean(false) {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Invert, exp: Box::new(exp) }, CortexType::boolean(false)))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("bool")))
                        }
                    },
                }
            },
            Expression::ListLiteral(items) => {
                let mut typ = CortexType::Unknown(false);
                let mut new_items = Vec::new();
                for item in items {
                    let (item_exp, item_type) = self.check_exp(item)?;
                    let item_type_str = item_type.codegen(0);
                    let typ_str = typ.codegen(0);
                    typ = typ
                        .combine_with(item_type)
                        .ok_or(PreprocessingError::CannotDetermineListLiteralType(typ_str, item_type_str))?;
                    new_items.push(item_exp);
                }
                let true_type = CortexType::reference(CortexType::list(typ, false), true);
                Ok((RExpression::ListLiteral(new_items), true_type))
            },
            Expression::Bang(inner) => {
                let (exp, typ) = self.check_exp(*inner)?;
                Ok((exp, typ.to_non_optional()))
            },
            Expression::MemberAccess(inner, member) => {
                let (atom_exp, atom_type) = self.check_exp(*inner)?;
                let composite = self.lookup_composite(atom_type.name()?)?;
                if !composite.fields.contains_key(&member) {
                    Err(Box::new(PreprocessingError::FieldDoesNotExist(member.clone(), atom_type.codegen(0))))
                } else {
                    let mut member_type = composite.fields.get(&member).unwrap().clone();
                    let bindings = Self::get_bindings(&composite.type_param_names, &atom_type)?;
                    member_type = TypeEnvironment::fill(member_type, &bindings);
                    member_type = member_type.with_prefix_if_not_core(&atom_type.prefix());
                    Ok((RExpression::MemberAccess(Box::new(atom_exp), member), member_type))
                }
            },
            Expression::MemberCall { callee, member, args } => {
                let (atom_exp, atom_type) = self.check_exp(*callee)?;
                let caller_type = atom_type.name()?;
                let caller_func_prefix = caller_type.without_last();
                let caller_func_base = caller_type.get_back()?;
                let member_func_name = Bundle::get_bundle_func_name(caller_func_base, &member);
                let member_func_path = PathIdent::continued(caller_func_prefix.clone(), member_func_name)
                    .subtract(&self.current_context)?;
                let mut args: Vec<RExpression> = args
                    .into_iter()
                    .map(|a| self.check_exp(a))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .map(|a| a.0)
                    .collect();
                let func = self.lookup_function(&member_func_path)?;
                args.push(atom_exp);

                let return_type = self.clean_type(func.return_type().clone());

                let id = self.function_dict.add_call(member_func_path);

                Ok((RExpression::Call(id, args), return_type))
            },
            Expression::BinaryOperation { left, op, right } => {
                let (left_exp, left_type) = self.check_exp(*left)?;
                let (right_exp, right_type) = self.check_exp(*right)?;
                let op_type = self.check_operator(left_type, &op, right_type)?;
                Ok((RExpression::BinaryOperation { left: Box::new(left_exp), op: op, right: Box::new(right_exp) }, op_type))
            },
        }
    }
    fn check_call(&mut self, path_ident: PathIdent, arg_exps: Vec<Expression>) -> CheckResult<RExpression> {
        let provided_arg_count = arg_exps.len();
        let mut processed_args = Vec::new();
        let mut arg_types = Vec::new();
        for a in arg_exps.into_iter() {
            let (arg, typ) = self.check_exp(a)?;
            arg_types.push(typ);
            processed_args.push(arg);
        }
        
        let func = self.lookup_function(&path_ident)?;

        if provided_arg_count != func.params.len() {
            return Err(Box::new(
                PreprocessingError::MismatchedArgumentCount(func.name.codegen(0), func.params.len(), provided_arg_count)
            ));
        }

        let mut return_type = func
            .return_type
            .clone()
            .with_prefix_if_not_core(&self.current_context)
            .with_prefix_if_not_core(&path_ident.without_last());

        let mut param_names = Vec::<String>::with_capacity(func.params.len());
        let mut param_types = Vec::<CortexType>::with_capacity(func.params.len());
        for param in &func.params {
            param_names.push(param.name.clone());
            param_types.push(param.typ.clone());
        }

        let bindings = self.infer_type_args(func, &arg_types)?;
        let parent_type_env = self.current_type_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        let mut new_type_env = TypeEnvironment::new(*parent_type_env);
        for (name, typ) in &bindings {
            new_type_env.add(name.clone(), typ.clone());
        }
        self.current_type_env = Some(Box::new(new_type_env));

        for (i, arg_type) in arg_types.into_iter().enumerate() {
            let arg_type = self.clean_type(arg_type);
            let param_type = self.clean_type(param_types.get(i).unwrap().clone().with_prefix_if_not_core(&self.current_context));
            if !arg_type.is_subtype_of(&param_type) {
                return Err(
                    Box::new(
                        PreprocessingError::MismatchedType(
                            param_type.codegen(0),
                            arg_type.codegen(0),
                            param_names.get(i).unwrap().clone(),
                        )
                    )
                );
            }
        }

        return_type = self.clean_type(return_type);

        self.current_type_env = Some(Box::new(self.current_type_env.take().unwrap().exit()?));

        let func_id = self.function_dict.add_call(path_ident);

        Ok((RExpression::Call(func_id, processed_args), return_type))
    }
    fn check_construction(&mut self, name: PathIdent, type_args: Vec<CortexType>, assignments: Vec<(String, Expression)>) -> CheckResult<RExpression> {
        let composite = self.lookup_composite(&name)?;
        let base_type = CortexType::basic(name.clone(), false, type_args.clone()).with_prefix_if_not_core(&self.current_context);

        if type_args.len() != composite.type_param_names.len() {
            return Err(Box::new(PreprocessingError::MismatchedTypeArgCount(name.codegen(0), composite.type_param_names.len(), type_args.len())));
        }
        let mut fields_to_assign = Vec::new();
        for k in composite.fields.keys() {
            fields_to_assign.push(k.clone());
        }

        let bindings = TypeEnvironment::create_bindings(&composite.type_param_names, &type_args);
        let mut new_assignments = Vec::new();
        for (fname, fvalue) in assignments {
            let opt_typ = composite.fields
                .get(&fname)
                .map(|t| t.clone());
            if let Some(typ) = opt_typ {
                let typ = TypeEnvironment::fill(typ, &bindings)
                    .with_prefix_if_not_core(&self.current_context)
                    .with_prefix_if_not_core(&name.without_last());
                let (exp, assigned_type) = self.check_exp(fvalue)?;
                if !assigned_type.is_subtype_of(&typ) {
                    return Err(
                        Box::new(
                            PreprocessingError::MismatchedType(
                                typ.codegen(0),
                                assigned_type.codegen(0),
                                fname.clone(),
                            )
                        )
                    );
                }

                new_assignments.push((fname.clone(), exp));

                let index_opt = fields_to_assign.iter().position(|x| *x == *fname);
                if let Some(index) = index_opt {
                    fields_to_assign.remove(index);
                } else {
                    return Err(Box::new(PreprocessingError::MultipleFieldAssignment(fname.clone())));
                }
            } else {
                return Err(Box::new(PreprocessingError::FieldDoesNotExist(fname.clone(), name.codegen(0))));
            }
        }

        if fields_to_assign.is_empty() {
            if composite.is_heap_allocated {
                Ok((RExpression::Construction { assignments: new_assignments, is_heap_allocated: true }, CortexType::reference(base_type, true)))
            } else {
                Ok((RExpression::Construction { assignments: new_assignments, is_heap_allocated: false }, base_type))
            }
        } else {
            Err(Box::new(PreprocessingError::NotAllFieldsAssigned(name.codegen(0), fields_to_assign.join(","))))
        }
    }
    fn check_if_statement(&mut self, first: ConditionBody, conds: Vec<ConditionBody>, last: Option<BasicBody>) -> CheckResult<RExpression> {
        let (cond_exp, cond_typ) = self.check_exp(first.condition)?;
        if cond_typ != CortexType::boolean(false) {
            return Err(
                Box::new(
                    PreprocessingError::MismatchedType(
                        String::from("bool"),
                        cond_typ.codegen(0),
                        String::from("if condition"),
                    )
                )
            );
        }
        let (first_body, mut the_type) = self.check_body(first.body)?;
        
        let mut condition_bodies = Vec::<RConditionBody>::new();
        for c in conds {
            let (cond, cond_typ) = self.check_exp(c.condition)?;
            if !cond_typ.is_subtype_of(&CortexType::boolean(false)) {
                return Err(
                    Box::new(
                        PreprocessingError::MismatchedType(
                            String::from("bool"),
                            cond_typ.codegen(0),
                            String::from("else-if condition"),
                        )
                    )
                );
            }
            let (body, typ) = self.check_body(c.body)?;
            let the_type_str = the_type.codegen(0);
            let typ_str = typ.codegen(0);
            let next = the_type.combine_with(typ);
            if let Some(t) = next {
                the_type = t;
                condition_bodies.push(RConditionBody::new(cond, body));
            } else {
                return Err(Box::new(PreprocessingError::IfArmsDoNotMatch(the_type_str, typ_str)));
            }
        }
        let mut final_body = None;
        if let Some(fin) = last {
            let (body, typ) = self.check_body(fin)?;
            final_body = Some(Box::new(body));
            let the_type_str = the_type.codegen(0);
            let typ_str = typ.codegen(0);
            let next = the_type.combine_with(typ);
            if let Some(t) = next {
                the_type = t;
            } else {
                return Err(Box::new(PreprocessingError::IfArmsDoNotMatch(the_type_str, typ_str)));
            }
        } else if the_type != CortexType::void(false) {
            return Err(Box::new(PreprocessingError::IfRequiresElseBlock));
        }

        Ok((RExpression::IfStatement { 
            first: Box::new(RConditionBody::new(cond_exp, first_body)),
            conds: condition_bodies,
            last: final_body,
        }, the_type))
    }

    fn check_body(&mut self, body: BasicBody) -> CheckResult<RInterpretedBody> {
        let mut statements = Vec::new();
        for st in body.statements {
            let s = self.check_statement(st)?;
            statements.push(s);
        }
        if let Some(exp) = body.result {
            let (exp, typ) = self.check_exp(exp)?;
            Ok((RInterpretedBody::new(statements, Some(exp)), typ))
        } else {
            Ok((RInterpretedBody::new(statements, None), CortexType::void(false)))
        }
    }

    fn check_operator(&self, first: CortexType, op: &BinaryOperator, second: CortexType) -> Result<CortexType, CortexError> {
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
                    Err(Box::new(PreprocessingError::InvalidOperator("number, string", "number, string")))
                }
            },
            BinaryOperator::Subtract => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
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
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "string")))
                }
            },
            BinaryOperator::Divide => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::Remainder => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::LogicAnd => {
                if first == boolean && second == boolean {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("boolean", "boolean")))
                }
            },
            BinaryOperator::LogicOr => {
                if first == boolean && second == boolean {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("boolean", "boolean")))
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
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsGreaterThan => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsLessThanOrEqualTo => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
                }
            },
            BinaryOperator::IsGreaterThanOrEqualTo => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number")))
                }
            },
        }
    }

    // "Cleans" type, for example replacing type arguments
    fn clean_type(&self, typ: CortexType) -> CortexType {
        self.current_type_env.as_ref().unwrap().fill_in(typ)
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
    fn infer_type_args(&self, func: &Function, args: &Vec<CortexType>) -> Result<HashMap<String, CortexType>, CortexError> {
        let mut bindings = HashMap::<String, CortexType>::new();
        for (arg, param) in args.iter().zip(&func.params) {
            self.infer_arg(&param.typ, &arg, &func.type_param_names, &mut bindings, param.name())?;
        }

        if bindings.len() != func.type_param_names.len() {
            Err(Box::new(PreprocessingError::CouldNotInferTypeBinding(func.name.codegen(0))))
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
                        return Err(Box::new(PreprocessingError::CannotHaveTypeArgsOnGeneric(param_type.codegen(0))));
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
            Err(Box::new(PreprocessingError::MismatchedType(param_type.codegen(0), arg_type.codegen(0), param_name.clone())))
        }
    }

    fn lookup_type(&self, path: &PathIdent) -> Result<CortexType, CortexError> {
        if path.is_final() {
            // Search in our environment for it
            let front = path.get_front()?;
            Ok(self.current_env.as_ref().unwrap().get(front)?.clone())
        } else {
            Err(Box::new(PreprocessingError::ValueNotFound(path.codegen(0))))
        }
    }
    
    fn lookup_function(&self, path: &PathIdent) -> Result<&Function, CortexError> {
        let last = path.get_back()?;
        let module = self.base_module.get_module_for(&PathIdent::concat(&self.current_context, path))?;
        let result = module.get_function(last);
        match result {
            Ok(f) => Ok(f),
            Err(ModuleError::FunctionDoesNotExist(_)) => Ok(self.global_module.get_function(last)?),
            Err(e) => Err(Box::new(e)),
        }
    }
    fn take_function(&mut self, path: &PathIdent) -> Result<Function, CortexError> {
        let last = path.get_back()?;
        let module = self.base_module.get_module_for_mut(&PathIdent::concat(&self.current_context, path))?;
        let result = module.take_function(last);
        match result {
            Ok(f) => Ok(f),
            Err(ModuleError::FunctionDoesNotExist(_)) => Ok(self.global_module.take_function(last)?),
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
