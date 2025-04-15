use std::{collections::{HashMap, HashSet, VecDeque}, error::Error, rc::Rc};

use crate::parsing::{ast::{expression::{BinaryOperator, IdentExpression, OptionalIdentifier, PConditionBody, PExpression, Parameter, PathIdent, UnaryOperator}, statement::{AssignmentName, DeclarationName, PStatement}, top_level::{BasicBody, Body, Bundle, Extension, FunctionSignature, MemberFunction, PFunction, Struct, ThisArg, TopLevel}, r#type::{forwarded_type_args, CortexType, TupleType, TypeError}}, codegen::r#trait::SimpleCodeGen};

use super::{ast::{expression::RExpression, function::{FunctionDict, RBody, RFunction, RInterpretedBody}, function_address::FunctionAddress, statement::{RConditionBody, RStatement}}, error::PreprocessingError, module::{Module, ModuleError, TypeDefinition}, program::Program, type_checking_env::TypeCheckingEnvironment, type_env::TypeEnvironment};

type CortexError = Box<dyn Error>;
pub type CheckResult<T> = Result<(T, CortexType), CortexError>;

pub struct CortexPreprocessor {
    current_env: Option<Box<TypeCheckingEnvironment>>,
    current_context: PathIdent,
    current_type_env: Option<Box<TypeEnvironment>>,
    function_dict: FunctionDict,
    function_signature_map: HashMap<FunctionAddress, FunctionSignature>,
    type_map: HashMap<PathIdent, TypeDefinition>,
    loop_depth: u32,
    temp_num: usize,
}

impl CortexPreprocessor {
    pub fn new() -> Result<Self, CortexError> {
        let mut this = CortexPreprocessor {
            current_env: Some(Box::new(TypeCheckingEnvironment::base())),
            current_context: PathIdent::empty(),
            current_type_env: Some(Box::new(TypeEnvironment::base())),
            function_dict: FunctionDict::new(),
            function_signature_map: HashMap::new(),
            type_map: HashMap::new(),
            loop_depth: 0,
            temp_num: 0,
        };

        macro_rules! add_core_type {
            ($name:literal) => {
                this.type_map.insert(PathIdent::simple(String::from($name)), TypeDefinition { fields: HashMap::new(), type_param_names: Vec::new(), is_heap_allocated: false });
            }
        }

        add_core_type!("number");
        add_core_type!("bool");
        add_core_type!("string");
        add_core_type!("void");
        add_core_type!("none");

        let mut global_module = Module::new();
        Self::add_list_funcs(&mut global_module)?;
        Self::add_string_funcs(&mut global_module)?;
        Self::add_range_funcs(&mut global_module)?;

        this.register_module(&PathIdent::empty(), global_module)?;

        Ok(this)
    }

    pub(crate) fn get_function(&self, id: usize) -> Option<&Rc<RFunction>> {
        self.function_dict.get(id)
    }

    pub(crate) fn determine_type(&mut self, expr: PExpression) -> Result<CortexType, CortexError> {
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
                match &function.name {
                    OptionalIdentifier::Ident(func_name) => {
                        let addr = FunctionAddress {
                            own_module_path: PathIdent::simple(func_name.clone()),
                            target: None,
                        };
                        self.add_signature(&addr, &function)?;
                        self.add_function(addr, function)?;
                        Ok(())
                    },
                    OptionalIdentifier::Ignore => Ok(()),
                }
            },
            TopLevel::Struct(struc) => {
                let mut funcs = Vec::new();
                self.add_struct(PathIdent::empty(), struc, &mut funcs)?;
                for (addr, f) in &funcs {
                    self.add_signature(addr, &f)?;
                }
                for (addr, f) in funcs {
                    self.add_function(addr, f)?;
                }
                Ok(())
            },
            TopLevel::Bundle(bundle) => {
                let mut funcs = Vec::new();
                self.add_bundle(PathIdent::empty(), bundle, &mut funcs)?;
                for (addr, f) in &funcs {
                    self.add_signature(addr, &f)?;
                }
                for (addr, f) in funcs {
                    self.add_function(addr, f)?;
                }
                Ok(())
            },
            TopLevel::Extension(extension) => {
                let mut funcs = Vec::new();
                self.add_extension(PathIdent::empty(), extension, &mut funcs)?;
                for (addr, f) in &funcs {
                    self.add_signature(addr, &f)?;
                }
                for (addr, f) in funcs {
                    self.add_function(addr, f)?;
                }
                Ok(())
            },
            TopLevel::Contract(item) => todo!(),
        }
    }

    pub fn register_module(&mut self, path: &PathIdent, mut module: Module) -> Result<(), CortexError> {
        for (path_end, m) in module.children_iter() {
            let this_path = PathIdent::continued(path.clone(), path_end);
            self.register_module(&this_path, m)?;
        }

        let mut functions = module
            .take_functions()?
            .into_iter()
            .map(|f| {
                match &f.name {
                    OptionalIdentifier::Ident(func_name) => {
                        let addr = FunctionAddress {
                            own_module_path: PathIdent::continued(path.clone(), func_name.clone()),
                            target: None,
                        };
                        Some((addr, f))
                    },
                    OptionalIdentifier::Ignore => None,
                }
            })
            .filter_map(|x| x)
            .collect::<Vec<(FunctionAddress, PFunction)>>();
        let structs = module.take_structs()?;
        let bundles = module.take_bundles()?;
        let extensions = module.take_extensions()?;

        let context_to_return_to = std::mem::replace(&mut self.current_context, path.clone());

        for item in structs {
            self.add_struct(path.clone(), item, &mut functions)?;
        }

        for item in bundles {
            self.add_bundle(path.clone(), item, &mut functions)?;
        }

        for item in extensions {
            self.add_extension(path.clone(), item, &mut functions)?;
        }

        for (addr, f) in &functions {
            self.add_signature(addr, &f)?;
        }

        for (addr, f) in functions {
            self.add_function(addr, f)?;
        }

        self.current_context = context_to_return_to;

        Ok(())
    }

    fn add_signature(&mut self, addr: &FunctionAddress, f: &PFunction) -> Result<(), CortexError> {
        let sig = f.signature();
        if self.function_signature_map.contains_key(&addr) {
            return Err(Box::new(ModuleError::FunctionAlreadyExists(addr.own_module_path.codegen(0))));
        }
        let mut seen_type_param_names = HashSet::new();
        for t in &sig.type_param_names {
            if seen_type_param_names.contains(t) {
                return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.clone())));
            }
            seen_type_param_names.insert(t);
        }
        self.function_signature_map.insert(addr.clone(), sig);
        Ok(())
    }
    fn add_function(&mut self, addr: FunctionAddress, f: PFunction) -> Result<(), CortexError> {
        let name = f.name().clone();
        let processed = self.preprocess_function(f)?;
        match name {
            OptionalIdentifier::Ident(_) => {
                self.function_dict.add_function(addr, processed);
            },
            OptionalIdentifier::Ignore => {},
        }
        Ok(())
    }
    fn add_struct(&mut self, n: PathIdent, item: Struct, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        match &item.name {
            OptionalIdentifier::Ident(item_name) => {
                let full_path = PathIdent::continued(n.clone(), item_name.clone());
                if self.has_type(&full_path) {
                    Err(Box::new(ModuleError::TypeAlreadyExists(full_path.codegen(0))))
                } else {
                    let has_loop = self.search_struct_for_loops(&item)?;
                    if has_loop {
                        return Err(Box::new(PreprocessingError::StructContainsCircularFields(full_path.codegen(0))));
                    }
                    
                    Self::handle_member_functions(item.functions, n, &item.type_param_names, item_name, funcs_to_add)?;

                    let mut seen_type_param_names = HashSet::new();
                    for t in &item.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.clone())));
                        }
                        seen_type_param_names.insert(t);
                    }

                    self.type_map.insert(full_path, TypeDefinition {
                        fields: item.fields,
                        is_heap_allocated: false,
                        type_param_names: item.type_param_names,
                    });
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }
    fn add_bundle(&mut self, n: PathIdent, item: Bundle, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        match &item.name {
            OptionalIdentifier::Ident(item_name) => {
                let full_path = PathIdent::continued(n.clone(), item_name.clone());
                if self.has_type(&full_path) {
                    Err(Box::new(ModuleError::TypeAlreadyExists(full_path.codegen(0))))
                } else {
                    Self::handle_member_functions(item.functions, n, &item.type_param_names, item_name, funcs_to_add)?;

                    let mut seen_type_param_names = HashSet::new();
                    for t in &item.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.clone())));
                        }
                        seen_type_param_names.insert(t);
                    }

                    self.type_map.insert(full_path, TypeDefinition {
                        fields: item.fields,
                        is_heap_allocated: true,
                        type_param_names: item.type_param_names,
                    });
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }
    fn handle_member_functions(functions: Vec<MemberFunction>, n: PathIdent, item_type_param_names: &Vec<String>, item_name: &String, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        for func in functions {
            let new_param = Parameter::named("this", Self::this_arg_to_type(func.signature.this_arg, item_name, item_type_param_names));
            let mut param_list = vec![new_param];
            param_list.extend(func.signature.params);
            let mut type_param_names = func.signature.type_param_names;
            let intersecting_type_param = item_type_param_names.iter().find(|t| type_param_names.contains(t));
            if let Some(name) = intersecting_type_param {
                return Err(Box::new(ModuleError::DuplicateTypeArgumentName(name.clone())));
            }
            type_param_names.extend(item_type_param_names.clone());
            let new_func = PFunction::new(
                OptionalIdentifier::Ident(func.signature.name.clone()),
                param_list,
                func.signature.return_type,
                func.body,
                type_param_names,
            );
            let addr = FunctionAddress {
                own_module_path: PathIdent::continued(n.clone(), func.signature.name),
                target: Some(PathIdent::continued(n.clone(), item_name.clone())),
            };
            funcs_to_add.push((addr, new_func));
        }
        Ok(())
    }

    fn add_extension(&mut self, n: PathIdent, item: Extension, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        let item_name = item.name.get_back()?;
        let item_prefix = item.name.without_last();
        for func in item.functions {
            let new_param = Parameter::named("this", Self::this_arg_to_type(func.signature.this_arg, item_name, &item.type_param_names).with_prefix(&item_prefix));
            let mut param_list = vec![new_param];
            param_list.extend(func.signature.params);
            let mut type_param_names = func.signature.type_param_names;
            let intersecting_type_param = item.type_param_names.iter().find(|t| type_param_names.contains(t));
            if let Some(name) = intersecting_type_param {
                return Err(Box::new(ModuleError::DuplicateTypeArgumentName(name.clone())));
            }
            type_param_names.extend(item.type_param_names.clone());
            let new_func = PFunction::new(
                OptionalIdentifier::Ident(func.signature.name.clone()),
                param_list,
                func.signature.return_type,
                func.body,
                type_param_names,
            );
            let addr = FunctionAddress {
                own_module_path: PathIdent::continued(n.clone(), func.signature.name),
                target: Some(PathIdent::concat(&n, &item.name)),
            };
            funcs_to_add.push((addr, new_func));
        }
        Ok(())
    }

    fn this_arg_to_type(this_arg: ThisArg, item_name: &String, type_param_names: &Vec<String>) -> CortexType {
        match this_arg {
            ThisArg::RefThis => 
                CortexType::reference(
                    CortexType::basic(PathIdent::simple(item_name.clone()), false, forwarded_type_args(type_param_names)),
                    false,
                ),
            ThisArg::RefMutThis => 
                CortexType::reference(
                    CortexType::basic(PathIdent::simple(item_name.clone()), false, forwarded_type_args(type_param_names)),
                    true,
                ),
            ThisArg::DirectThis => CortexType::basic(PathIdent::simple(item_name.clone()), false, forwarded_type_args(type_param_names)),
        }
    }

    pub fn preprocess(&mut self, body: BasicBody) -> Result<Program, CortexError> {
        let (body, _) = self.check_body(body)?;
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
                TopLevel::Extension(item) => {
                    module.add_extension(item)?;
                },
                TopLevel::Contract(item) => todo!(),
            }
        }
        Ok(module)
    }

    pub fn preprocess_function(&mut self, function: PFunction) -> Result<RFunction, CortexError> {
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
                let return_type = self.clean_type(function.return_type.clone().with_prefix_if_not_core(&self.current_context));
                if !body_type.is_subtype_of(&return_type) {
                    return Err(Box::new(PreprocessingError::ReturnTypeMismatch(return_type.codegen(0), body_type.codegen(0))));
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

    fn check_statement(&mut self, statement: PStatement) -> Result<Vec<RStatement>, CortexError> {
        let st_str = statement.codegen(0);
        match statement {
            PStatement::Expression(expression) => {
                let (exp, _) = self.check_exp(expression)?;
                Ok(vec![RStatement::Expression(exp)])
            },
            PStatement::Throw(expression) => {
                let (exp, _) = self.check_exp(expression)?;
                Ok(vec![RStatement::Throw(exp)])
            },
            PStatement::VariableDeclaration { name, is_const, typ, initial_value } => {
                Ok(self.check_declaration_recursive(name, typ, is_const, initial_value, &st_str)?)
            },
            PStatement::Assignment { name, value } => {
                Ok(self.check_assignment_recursive(name, value, &st_str)?)
            },
            PStatement::WhileLoop(condition_body) => {
                let (cond, cond_type) = self.check_exp(condition_body.condition)?;
                if !cond_type.is_subtype_of(&CortexType::boolean(false)) {
                    return Err(
                        Box::new(
                            PreprocessingError::MismatchedType(
                                String::from("bool"),
                                cond_type.codegen(0),
                                String::from("while condition"),
                                st_str,
                            )
                        )
                    );
                }

                self.loop_depth += 1;
                let (body, body_type) = self.check_body_and_handle_env(condition_body.body)?;
                if !body_type.is_subtype_of(&CortexType::void(false)) {
                    return Err(
                        Box::new(
                            PreprocessingError::LoopCannotHaveReturnValue
                        )
                    );
                }
                self.loop_depth -= 1;

                Ok(vec![RStatement::WhileLoop(RConditionBody::new(cond, body))])
            },
            PStatement::Break => {
                if self.loop_depth <= 0 {
                    Err(Box::new(PreprocessingError::BreakUsedInNonLoopContext))
                } else {
                    Ok(vec![RStatement::Break])
                }
            },
            PStatement::Continue => {
                if self.loop_depth <= 0 {
                    Err(Box::new(PreprocessingError::ContinueUsedInNonLoopContext))
                } else {
                    Ok(vec![RStatement::Continue])
                }
            },
        }
    }

    fn check_declaration(&mut self, name: OptionalIdentifier, typ: Option<CortexType>, is_const: bool, initial_value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            OptionalIdentifier::Ident(ident) => {
                let (assigned_exp, assigned_type) = self.check_exp(initial_value)?;
                let type_of_var = if let Some(mut declared_type) = typ {
                    declared_type = self.clean_type(declared_type.with_prefix_if_not_core(&self.current_context));
                    if !assigned_type.is_subtype_of(&declared_type) {
                        return Err(
                            Box::new(
                                PreprocessingError::MismatchedType(
                                    declared_type.codegen(0),
                                    assigned_type.codegen(0),
                                    ident.clone(),
                                    st_str.clone(),
                                )
                            )
                        );
                    }
                    declared_type.clone()
                } else {
                    assigned_type
                };

                self.current_env.as_mut().unwrap().add(ident.clone(), type_of_var, is_const)?;

                Ok(vec![RStatement::VariableDeclaration { name: ident, is_const: is_const, initial_value: assigned_exp }])
            },
            OptionalIdentifier::Ignore => {
                let (exp, _) = self.check_exp(initial_value)?;
                Ok(vec![RStatement::Expression(exp)])
            },
        }
    }
    fn check_declaration_recursive(&mut self, name: DeclarationName, typ: Option<CortexType>, is_const: bool, initial_value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            DeclarationName::Single(name) => {
                Ok(self.check_declaration(name, typ, is_const, initial_value, &st_str)?)
            },
            DeclarationName::Tuple(names) => {
                let mut result = Vec::new();
                // We want to save off the initial expression in case it is an expensive calculation
                let temp_name = self.next_temp();
                // We need to run through this so that the preprocessor registers that the temp var was created
                let var_dec = self.check_statement(PStatement::VariableDeclaration {
                    name: DeclarationName::Single(OptionalIdentifier::Ident(temp_name.clone())),
                    is_const: true,
                    typ,
                    initial_value,
                })?;
                result.extend(var_dec);
                let temp_expr = PExpression::PathIdent(PathIdent::simple(temp_name));
                for (i, name) in names.into_iter().enumerate() {
                    let new_value = PExpression::MemberAccess(Box::new(temp_expr.clone()), format!("t{}", i));
                    result.extend(self.check_declaration_recursive(name, None, is_const, new_value, &st_str)?);
                }
                Ok(result)
            },
        }
    }

    fn check_assignment(&mut self, name: IdentExpression, value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
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
                            st_str.clone(),
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
            let source_expr = name.clone().without_last()?.to_member_access_expr();
            let (_, source_type) = self.check_exp(source_expr)?;
            if let CortexType::RefType(r) = source_type {
                if !r.mutable {
                    return Err(
                        Box::new(
                            PreprocessingError::CannotModifyFieldOnImmutableReference(r.contained.codegen(0))
                        )
                    );
                }
            }

            let name_expr = name.clone().to_member_access_expr();
            let (_, var_type) = self.check_exp(name_expr)?;
            if !assigned_type.is_subtype_of(&var_type) {
                return Err(
                    Box::new(
                        PreprocessingError::MismatchedType(
                            var_type.codegen(0),
                            assigned_type.codegen(0),
                            name.codegen(0),
                            st_str.clone(),
                        )
                    )
                );
            }
        }

        Ok(vec![RStatement::Assignment { name: name.into(), value: assigned_exp }])
    }
    fn check_assignment_recursive(&mut self, name: AssignmentName, value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            AssignmentName::Single(name) => {
                Ok(self.check_assignment(name, value, &st_str)?)
            },
            AssignmentName::Ignore => {
                Ok(self.check_statement(PStatement::Expression(value))?)
            },
            AssignmentName::Tuple(names) => {
                let mut result = Vec::new();
                // We want to save off the initial expression in case it is an expensive calculation
                let temp_name = self.next_temp();
                // We need to run through this so that the preprocessor registers that the temp var was created
                let var_dec = self.check_statement(PStatement::VariableDeclaration {
                    name: DeclarationName::Single(OptionalIdentifier::Ident(temp_name.clone())),
                    is_const: true,
                    typ: None,
                    initial_value: value,
                })?;
                result.extend(var_dec);
                let temp_expr = PExpression::PathIdent(PathIdent::simple(temp_name));
                for (i, name) in names.into_iter().enumerate() {
                    let new_value = PExpression::MemberAccess(Box::new(temp_expr.clone()), format!("t{}", i));
                    result.extend(self.check_assignment_recursive(name, new_value, &st_str)?);
                }
                Ok(result)
            },
        }
    }

    fn check_exp(&mut self, exp: PExpression) -> CheckResult<RExpression> {
        let st_str = exp.codegen(0);
        match exp {
            PExpression::Number(v) => Ok((RExpression::Number(v), CortexType::number(false))),
            PExpression::Boolean(v) => Ok((RExpression::Boolean(v), CortexType::boolean(false))),
            PExpression::Void => Ok((RExpression::Void, CortexType::void(false))),
            PExpression::None => Ok((RExpression::None, CortexType::none())),
            PExpression::String(v) => Ok((RExpression::String(v), CortexType::string(false))),
            PExpression::Char(v) => Ok((RExpression::Char(v), CortexType::char(false))),
            PExpression::PathIdent(path_ident) => Ok((RExpression::Identifier(path_ident.get_back()?.clone()), self.get_variable_type(&path_ident)?)),
            PExpression::Call { name: addr, args: arg_exps, type_args } => {
                let extended = PathIdent::concat(&self.current_context, &addr.without_last());
                let context_to_return_to = std::mem::replace(&mut self.current_context, extended);
                let result = self.check_call(
                    addr.get_back()?,
                    arg_exps, 
                    type_args, 
                    &st_str
                );
                self.current_context = context_to_return_to;
                result
            },
            PExpression::Construction { name, type_args, assignments } => {
                self.check_construction(name, type_args, assignments, &st_str)
            },
            PExpression::IfStatement { first, conds, last } => {
                self.check_if_statement(*first, conds, last.map(|b| *b), &st_str)
            },
            PExpression::UnaryOperation { op, exp } => {
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
            PExpression::ListLiteral(items) => {
                let mut contained_type = CortexType::Unknown(false);
                let mut new_items = Vec::new();
                for item in items {
                    let (item_exp, item_type) = self.check_exp(item)?;
                    let item_type_str = item_type.codegen(0);
                    let typ_str = contained_type.codegen(0);
                    contained_type = contained_type
                        .combine_with(item_type)
                        .ok_or(PreprocessingError::CannotDetermineListLiteralType(typ_str, item_type_str))?;
                    new_items.push(item_exp);
                }
                let true_type = CortexType::reference(CortexType::list(contained_type, false), true);
                Ok((RExpression::ListLiteral(new_items), true_type))
            },
            PExpression::Bang(inner) => {
                let (exp, typ) = self.check_exp(*inner)?;
                Ok((RExpression::Bang(Box::new(exp)), typ.to_non_optional()))
            },
            PExpression::MemberAccess(inner, member) => {
                let inner_as_string = inner.codegen(0);
                let (atom_exp, atom_type) = self.check_exp(*inner)?;
                match &atom_type {
                    CortexType::BasicType(_) |
                    CortexType::RefType(_) => {
                        if atom_type.is_non_composite() {
                            return Err(Box::new(PreprocessingError::CannotAccessMemberOfNonComposite));
                        }
                        if atom_type.optional() {
                            return Err(Box::new(PreprocessingError::CannotAccessMemberOfOptional(inner_as_string)));
                        }
                        Ok(self.check_composite_member_access(atom_exp, atom_type, member)?)
                    },
                    CortexType::Unknown(_) => Err(Box::new(TypeError::UnknownTypeNotValid)),
                    CortexType::TupleType(t) => {
                        if t.optional {
                            return Err(Box::new(PreprocessingError::CannotAccessMemberOfOptional(inner_as_string)));
                        }
                        Ok(self.check_tuple_member_access(atom_exp, t, member)?)
                    },
                }
            },
            PExpression::MemberCall { callee, member, mut args, type_args } => {
                let (_, atom_type) = self.check_exp(*callee.clone())?;
                
                let caller_type = atom_type.name()?;
                let caller_type_prefix = caller_type.without_last();
                let non_extension_func_addr = FunctionAddress::member_func(
                    PathIdent::continued(caller_type_prefix.clone().subtract(&self.current_context)?, member.clone()), 
                    caller_type.clone().subtract(&self.current_context)?);
                
                args.insert(0, *callee);
                let true_type_args;
                if let Some(mut type_args) = type_args {
                    let typedef = self.lookup_type(caller_type)?;
                    let mut bindings = HashMap::new();
                    self.infer_arg(&CortexType::reference(
                        CortexType::basic(caller_type.clone(), false, forwarded_type_args(&typedef.type_param_names)),
                        true,
                    ), &atom_type, &typedef.type_param_names, &mut bindings, &String::from("this"), &st_str)?;
                    let mut beginning_type_args = Vec::new();
                    for a in &typedef.type_param_names {
                        beginning_type_args.push(bindings.remove(a).unwrap());
                    }
                    type_args.extend(beginning_type_args);
                    true_type_args = Some(type_args);
                } else {
                    true_type_args = None;
                }

                let actual_func_addr;
                if self.has_function(&non_extension_func_addr) {
                    actual_func_addr = non_extension_func_addr;
                } else {
                    let attempted_extension_path = self.search_for_extension(&caller_type, &member)?;
                    if let Some(extension_func_path) = attempted_extension_path {
                        actual_func_addr = extension_func_path.clone();
                    } else {
                        return Err(Box::new(PreprocessingError::FunctionDoesNotExist(non_extension_func_addr.codegen(0))));
                    }
                }

                let call_exp = PExpression::Call {
                    name: actual_func_addr, 
                    args,
                    type_args: true_type_args,
                };
                let result = self.check_exp(call_exp)?;
                Ok(result)
            },
            PExpression::BinaryOperation { left, op, right } => {
                let (left_exp, left_type) = self.check_exp(*left)?;
                let (right_exp, right_type) = self.check_exp(*right)?;
                let op_type = self.check_operator(left_type, &op, right_type)?;
                Ok((RExpression::BinaryOperation { left: Box::new(left_exp), op: op, right: Box::new(right_exp) }, op_type))
            },
            PExpression::Tuple(items) => {
                let results = items
                    .into_iter()
                    .map(|e| self.check_exp(e))
                    .collect::<Result<Vec<_>, _>>()?;
                let (exps, types): (Vec<RExpression>, Vec<CortexType>) = results.into_iter().unzip();
                Ok((RExpression::Tuple(exps), CortexType::tuple(types, false)))
            },
            PExpression::Range { start, end, step } => {
                fn otov(o: Option<f64>) -> RExpression {
                    match o {
                        Some(v) => RExpression::Number(v),
                        None => RExpression::None,
                    }
                }
                let construction = RExpression::Construction {
                    assignments: vec![
                        (String::from("start"), otov(start)),
                        (String::from("end"), otov(end)),
                        (String::from("step"), otov(step)),
                    ],
                    is_heap_allocated: false,
                };
                Ok((construction, CortexType::range(false)))
            },
        }
    }
    fn check_composite_member_access(&mut self, atom_exp: RExpression, atom_type: CortexType, member: String) -> CheckResult<RExpression> {
        let is_mutable;
        match &atom_type {
            CortexType::BasicType(_) | 
            CortexType::TupleType(_) => {
                is_mutable = true;
            },
            CortexType::RefType(r) => {
                is_mutable = r.mutable;
            },
            CortexType::Unknown(_) => {
                return Err(Box::new(PreprocessingError::UnknownTypeFound));
            },
        }
        let typedef = self.lookup_type(&atom_type.name()?.clone().subtract(&self.current_context)?)?;
        if !typedef.fields.contains_key(&member) {
            Err(Box::new(PreprocessingError::FieldDoesNotExist(member.clone(), atom_type.codegen(0))))
        } else {
            let mut member_type = typedef.fields.get(&member).unwrap().clone();
            let bindings = Self::get_bindings(&typedef.type_param_names, &atom_type)?;
            let prefix = atom_type.prefix();
            member_type = TypeEnvironment::fill(member_type, 
                &bindings
                    .into_iter()
                    .map(|(k, v)| (k, v.subtract_if_possible(&prefix)))
                    .collect::<HashMap<_, _>>()
                );
            member_type = member_type.with_prefix_if_not_core(&prefix);
            member_type = member_type.forward_immutability(is_mutable);
            Ok((RExpression::MemberAccess(Box::new(atom_exp), member), member_type))
        }
    }
    fn check_tuple_member_access(&mut self, atom_exp: RExpression, atom_type: &TupleType, member: String) -> CheckResult<RExpression> {
        fn strip_t(s: &str) -> Option<usize> {
            s.strip_prefix('t')?.parse().ok()
        }

        let index = strip_t(&member).ok_or(PreprocessingError::TupleMemberSyntaxInvalid(member))?;
        if index > atom_type.types.len() {
            return Err(Box::new(PreprocessingError::TupleIndexValueInvalid(atom_type.types.len(), index)));
        }

        let member_type = atom_type.types.get(index).unwrap().clone();
        
        Ok((RExpression::MemberAccess(Box::new(atom_exp), format!("t{}", index)), member_type))
    }

    fn search_for_extension(&self, typ: &PathIdent, member: &String) -> Result<Option<&FunctionAddress>, CortexError> {
        // Search through *ALL* functions
        // If they are prefixed by the current_context, and have a target type = to `typ`,
        // and a function name .getBack() == member, then return that address
        let candidates = self.function_signature_map.keys().filter_map(|p| {
            // Must be prefixed by current_context to be in scope at this point
            if p.own_module_path.is_prefixed_by(&self.current_context) {
                // Must have the same target type to be able to be called
                if let Some(target) = &p.target {
                    if target == typ {
                        // Finally, must have same method name (getBack() == member)
                        let back = p.own_module_path.get_back();
                        match back {
                            Ok(back_name) => {
                                if back_name == member {
                                    return Some(Ok::<_, CortexError>(p));
                                }
                            },
                            Err(err) => {
                                return Some(Err(Box::new(err)));
                            },
                        }
                    }
                }
            }
            None
        }).collect::<Result<Vec<_>, _>>()?;

        if candidates.len() > 1 {
            Err(Box::new(PreprocessingError::AmbiguousExtensionCall(member.clone(), typ.codegen(0))))
        } else {
            Ok(candidates.get(0).cloned())
        }
    }

    fn check_call(&mut self, addr: FunctionAddress, arg_exps: Vec<PExpression>, type_args: Option<Vec<CortexType>>, st_str: &String) -> CheckResult<RExpression> {
        let provided_arg_count = arg_exps.len();
        let mut processed_args = Vec::new();
        let mut arg_types = Vec::new();
        for a in arg_exps.into_iter() {
            let (arg, typ) = self.check_exp(a)?;
            arg_types.push(typ);
            processed_args.push(arg);
        }
        
        let sig = self.lookup_signature(&addr)?.clone();

        let full_path = FunctionAddress::concat(&self.current_context, &addr);
        if provided_arg_count != sig.params.len() {
            return Err(Box::new(
                PreprocessingError::MismatchedArgumentCount(full_path.codegen(0), sig.params.len(), provided_arg_count)
            ));
        }

        let mut return_type = sig
            .return_type
            .clone();

        let mut param_names = Vec::<String>::with_capacity(sig.params.len());
        let mut param_types = Vec::<CortexType>::with_capacity(sig.params.len());
        for param in &sig.params {
            param_names.push(param.name.clone());
            param_types.push(param.typ.clone());
        }

        let bindings;
        if let Some(type_args) = type_args {
            bindings = sig.type_param_names.iter().cloned().zip(type_args).collect();
        } else {
            bindings = self.infer_type_args(&sig, &arg_types, &full_path, st_str)?;
        }
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
                            st_str.clone(),
                        )
                    )
                );
            }
        }

        return_type = self.clean_type(return_type)
            .with_prefix_if_not_core(&self.current_context);

        self.current_type_env = Some(Box::new(self.current_type_env.take().unwrap().exit()?));

        let func_id = self.function_dict.add_call(full_path)?;

        Ok((RExpression::Call(func_id, processed_args), return_type))
    }
    fn check_construction(&mut self, name: PathIdent, type_args: Vec<CortexType>, assignments: Vec<(String, PExpression)>, st_str: &String) -> CheckResult<RExpression> {
        let typedef = self.lookup_type(&name)?;
        let base_type = CortexType::basic(name.clone(), false, type_args.clone()).with_prefix_if_not_core(&self.current_context);

        if type_args.len() != typedef.type_param_names.len() {
            return Err(Box::new(PreprocessingError::MismatchedTypeArgCount(name.codegen(0), typedef.type_param_names.len(), type_args.len())));
        }
        let mut fields_to_assign = Vec::new();
        for k in typedef.fields.keys() {
            fields_to_assign.push(k.clone());
        }

        let is_heap_allocated = typedef.is_heap_allocated;
        let fields = typedef.fields.clone();

        let bindings = TypeEnvironment::create_bindings(&typedef.type_param_names, &type_args);
        let bindings = bindings
            .iter()
            .map(|(k, v)| (k.clone(), v.clone().subtract_if_possible(&name.without_last())))
            .collect::<HashMap<_, _>>();
        let mut new_assignments = Vec::new();
        for (fname, fvalue) in assignments {
            let opt_typ = fields
                .get(&fname)
                .map(|t| t.clone());
            if let Some(typ) = opt_typ {
                let field_type = TypeEnvironment::fill(typ, &bindings)
                    .with_prefix_if_not_core(&self.current_context)
                    .with_prefix_if_not_core(&name.without_last());
                let (exp, assigned_type) = self.check_exp(fvalue)?;
                if !assigned_type.is_subtype_of(&field_type) {
                    return Err(
                        Box::new(
                            PreprocessingError::MismatchedType(
                                field_type.codegen(0),
                                assigned_type.codegen(0),
                                fname.clone(),
                                st_str.clone(),
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
            if is_heap_allocated {
                Ok((RExpression::Construction { assignments: new_assignments, is_heap_allocated: true }, CortexType::reference(base_type, true)))
            } else {
                Ok((RExpression::Construction { assignments: new_assignments, is_heap_allocated: false }, base_type))
            }
        } else {
            Err(Box::new(PreprocessingError::NotAllFieldsAssigned(name.codegen(0), fields_to_assign.join(","))))
        }
    }
    fn check_if_statement(&mut self, first: PConditionBody, conds: Vec<PConditionBody>, last: Option<BasicBody>, st_str: &String) -> CheckResult<RExpression> {
        let (cond_exp, cond_typ) = self.check_exp(first.condition)?;
        if cond_typ != CortexType::boolean(false) {
            return Err(
                Box::new(
                    PreprocessingError::MismatchedType(
                        String::from("bool"),
                        cond_typ.codegen(0),
                        String::from("if condition"),
                        st_str.clone(),
                    )
                )
            );
        }
        let (first_body, mut the_type) = self.check_body_and_handle_env(first.body)?;
        
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
                            st_str.clone(),
                        )
                    )
                );
            }
            let (body, typ) = self.check_body_and_handle_env(c.body)?;
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
            let (body, typ) = self.check_body_and_handle_env(fin)?;
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

    fn check_body_and_handle_env(&mut self, body: BasicBody) -> CheckResult<RInterpretedBody> {
        let parent_env = self.current_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        self.current_env = Some(Box::new(TypeCheckingEnvironment::new(*parent_env)));

        let result = self.check_body(body);

        self.current_env = Some(Box::new(self.current_env.take().unwrap().exit()?));
        result
    }

    fn check_body(&mut self, body: BasicBody) -> CheckResult<RInterpretedBody> {
        let mut statements = Vec::new();
        for st in body.statements {
            let s = self.check_statement(st)?;
            statements.extend(s);
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
            if let CortexType::BasicType(b) = &typ {
                bindings = TypeEnvironment::create_bindings(type_param_names, &b.type_args);
                typ = TypeEnvironment::fill(typ, &bindings);
                type_args_handled = true;
            } else if let CortexType::RefType(r) = typ {
                typ = *r.contained;
            }
        }
        Ok(bindings)
    }
    fn infer_type_args(&self, sig: &FunctionSignature, args: &Vec<CortexType>, name: &FunctionAddress, st_str: &String) -> Result<HashMap<String, CortexType>, CortexError> {
        let mut bindings = HashMap::<String, CortexType>::new();
        for (arg, param) in args.iter().zip(&sig.params) {
            self.infer_arg(&param.typ, &arg, &sig.type_param_names, &mut bindings, param.name(), st_str)?;
        }

        if bindings.len() != sig.type_param_names.len() {
            Err(Box::new(PreprocessingError::CouldNotInferTypeBinding(name.codegen(0))))
        } else {
            Ok(bindings)
        }
    }
    fn infer_arg(&self, param_type: &CortexType, arg_type: &CortexType, type_param_names: &Vec<String>, bindings: &mut HashMap<String, CortexType>, param_name: &String, st_str: &String) -> Result<(), CortexError> {
        let correct;
        match (&param_type, arg_type) {
            (CortexType::BasicType(b), arg_type) => {
                if let Some(name) = TypeEnvironment::does_arg_list_contain(type_param_names, &param_type) {
                    // If we take in a T? and passing a number?, then we want T = number, not T = number?
                    let mut bound_type = arg_type.clone();
                    if b.optional {
                        bound_type = bound_type.to_non_optional();
                    }
                    if b.type_args.len() > 0 {
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
                    if let CortexType::BasicType(b2) = arg_type {
                        if b.type_args.len() == b2.type_args.len() {
                            for (type_param, type_arg) in b.type_args.iter().zip(&b2.type_args) {
                                self.infer_arg(type_param, type_arg, type_param_names, bindings, param_name, st_str)?;
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
            (CortexType::RefType(r), CortexType::RefType(r2)) => {
                self.infer_arg(&*r.contained, &*r2.contained, type_param_names, bindings, param_name, st_str)?;
                correct = true;
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    for (type1, type2) in t1.types.iter().zip(&t2.types) {
                        self.infer_arg(type1, type2, type_param_names, bindings, param_name, st_str)?;
                    }
                    correct = true;
                } else {
                    correct = false;
                }
            },
            (_, _) => {
                correct = false;
            },
        }
        if correct {
            Ok(())
        } else {
            Err(Box::new(PreprocessingError::MismatchedType(param_type.codegen(0), arg_type.codegen(0), param_name.clone(), st_str.clone())))
        }
    }

    fn search_struct_for_loops(&self, s: &Struct) -> Result<bool, CortexError> {
        match &s.name {
            OptionalIdentifier::Ident(name) => {
                let stype = CortexType::basic(PathIdent::simple(name.clone()), false, forwarded_type_args(&s.type_param_names));
                let mut q = VecDeque::new();
                for field in &s.fields {
                    q.push_back(field.1.clone());
                }
                // Only need to search for references to this struct, everything else should be fine
                while !q.is_empty() {
                    let typ = q.pop_front().unwrap();
                    if typ == stype {
                        return Ok(true);
                    }
                    if !typ.is_core() {
                        // Enqueue all fields of this type
                        let typ_name = typ.name()?;

                        // It's ok if the struct doesn't exist yet
                        // If it has loops, then they will be caught when we visit this function upon registering it
                        // Unfortunately, the order in which structs are added is not deterministic
                        if self.has_type(typ_name) {
                            let struc = self.lookup_type(typ_name)?;
                            for field in &struc.fields {
                                q.push_back(field.1.clone());
                            }
                        }
                    }
                }
                Ok(false)
            },
            OptionalIdentifier::Ignore => Ok(false),
        }
    }

    fn get_variable_type(&self, path: &PathIdent) -> Result<CortexType, CortexError> {
        if path.is_final() {
            // Search in our environment for it
            let front = path.get_front()?;
            Ok(self.current_env.as_ref().unwrap().get(front)?.clone())
        } else {
            Err(Box::new(PreprocessingError::ValueNotFound(path.codegen(0))))
        }
    }

    fn has_function(&self, path: &FunctionAddress) -> bool {
        let full_path: FunctionAddress = FunctionAddress::concat(&self.current_context, &path);
        self.function_signature_map.contains_key(&full_path)
    }
    fn lookup_signature(&self, path: &FunctionAddress) -> Result<&FunctionSignature, CortexError> {
        let full_path: FunctionAddress = FunctionAddress::concat(&self.current_context, &path);
        if let Some(sig) = self.function_signature_map.get(&full_path) {
            Ok(sig)
        } else {
            Err(Box::new(PreprocessingError::FunctionDoesNotExist(full_path.codegen(0))))
        }
    }

    fn lookup_type(&self, path: &PathIdent) -> Result<&TypeDefinition, CortexError> {
        let full_path = PathIdent::concat(&self.current_context, &path);
        if let Some(c) = self.type_map.get(&full_path) {
            Ok(c)
        } else {
            Err(Box::new(PreprocessingError::TypeDoesNotExist(full_path.codegen(0))))
        }
    }
    fn has_type(&self, path: &PathIdent) -> bool {
        let full_path = PathIdent::concat(&self.current_context, &path);
        self.type_map.contains_key(&full_path)
    }

    fn next_temp(&mut self) -> String {
        let res = format!("$temp{}", self.temp_num);
        self.temp_num += 1;
        res
    }
}
