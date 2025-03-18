use std::{collections::HashMap, rc::Rc};

use crate::parsing::{ast::{expression::{Atom, Expression, ExpressionTail, PathIdent}, top_level::{Function, TopLevel}, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};

use super::{env::EnvError, error::{CortexError, PreprocessingError}, module::Module, type_env::TypeEnvironment};

pub type CheckResult = Result<CortexType, CortexError>;

pub struct CortexPreprocessor {
    current_var_env: Option<Box<TypeEnvironment>>,
    base_module: Module,
    current_context: PathIdent,
    current_type_env: Option<Box<TypeEnvironment>>,
    global_module: Module,
}

impl CortexPreprocessor {
    pub fn new() -> Result<Self, CortexError> {
        let mut this = CortexPreprocessor {
            base_module: Module::new(),
            current_var_env: Some(Box::new(TypeEnvironment::base())),
            current_context: PathIdent::empty(),
            current_type_env: Some(Box::new(TypeEnvironment::base())),
            global_module: Module::new(),
        };

        // Self::add_list_funcs(&mut this.global_module, this.heap.clone())?;
        // Self::add_string_funcs(&mut this.global_module, this.heap.clone())?;

        Ok(this)
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

    fn check_exp(&mut self, exp: &mut Expression) -> CheckResult {
        let atom_type = self.check_atom(&mut exp.atom)?;
        let tail_type = self.check_tail(atom_type, &mut exp.tail)?;
        Ok(tail_type)
    }
    fn check_atom(&mut self, atom: &mut Atom) -> CheckResult {
        match atom {
            Atom::Number(_) => Ok(CortexType::number(false)),
            Atom::Boolean(_) => Ok(CortexType::boolean(false)),
            Atom::Void => Ok(CortexType::void(false)),
            Atom::None => Ok(CortexType::none()),
            Atom::String(_) => Ok(CortexType::string(false)),
            Atom::PathIdent(path_ident) => todo!(),
            Atom::Call(path_ident, expressions) => todo!(),
            Atom::Construction { name, type_args, assignments } => todo!(),
            Atom::IfStatement { first, conds, last } => todo!(),
            Atom::UnaryOperation { op, exp } => todo!(),
            Atom::ListLiteral(items) => {
                let mut typ = CortexType::Unknown(false);
                for item in items {
                    let item_type = self.check_exp(item)?;
                    let item_type_str = item_type.codegen(0);
                    let typ_str = typ.codegen(0);
                    typ = typ
                        .combine_with(item_type)
                        .ok_or(PreprocessingError::CannotDetermineListLiteralType(typ_str, item_type_str))?;
                }
                let true_type = CortexType::reference(CortexType::list(typ, false), true);
                Ok(true_type)
            },
            Atom::Expression(expression) => Ok(self.check_exp(expression)?),
        }
    }
    fn check_tail(&mut self, atom_type: CortexType, tail: &mut ExpressionTail) -> CheckResult {
        match tail {
            ExpressionTail::None => Ok(atom_type),
            ExpressionTail::PostfixBang { next } => Ok(self.check_tail(atom_type.to_non_optional(), next)?),
            ExpressionTail::MemberAccess { member, next } => todo!(),
            ExpressionTail::MemberCall { member, args, next } => todo!(),
            ExpressionTail::BinOp { op, right, next } => todo!(),
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
    fn infer_type_args(&self, func: &Rc<Function>, args: &Vec<CortexType>) -> Result<HashMap<String, CortexType>, CortexError> {
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
            Ok(self.current_var_env.as_ref().unwrap().get(front).ok_or(EnvError::VariableDoesNotExist(front.clone()))?.clone())
        } else {
            Err(Box::new(PreprocessingError::ValueNotFound(path.codegen(0))))
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
