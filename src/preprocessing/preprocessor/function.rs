use std::collections::HashMap;

use crate::{interpreting::error::CortexError, parsing::{ast::{expression::{OptionalIdentifier, PExpression, Parameter, PathIdent}, top_level::FunctionSignature, r#type::{forwarded_type_args, CortexType, FollowsType}}, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::{expression::RExpression, function_address::FunctionAddress}, error::PreprocessingError, type_env::TypeEnvironment}};

use super::preprocessor::{CheckResult, CortexPreprocessor};

struct ProcessedCall {
    args: Vec<RExpression>,
    return_type: CortexType,
}

impl CortexPreprocessor {
    pub(super) fn check_fat_member_call(&mut self, atom_type: FollowsType, callee: Box<PExpression>, member: String, mut args: Vec<PExpression>, type_args: Option<Vec<CortexType>>, st_str: String) -> CheckResult<RExpression> {
        let mut function_sig = None;
        let mut contract_to_use = None;
        let mut full_prefix = None;
        for entry in &atom_type.clause.contracts {
            let contract = self.lookup_contract(&entry.name)?;
            for sig in &contract.function_sigs {
                if sig.name == OptionalIdentifier::Ident(member.clone()) {
                    function_sig = Some(sig);
                    contract_to_use = Some(contract);
                    full_prefix = Some(PathIdent::concat(&self.current_context, &entry.name).without_last());
                    break;
                }
            }
        }
        if let None = function_sig {
            return Err(Box::new(PreprocessingError::FunctionDoesNotExist(member)));
        }
        let mut function_sig = function_sig.unwrap().clone();
        let contract_to_use = contract_to_use.unwrap();
        let full_prefix = full_prefix.unwrap();

        let index = contract_to_use.function_sigs
            .iter()
            .position(|a| a.name == function_sig.name);

        if let None = index {
            return Err(Box::new(PreprocessingError::FunctionDoesNotExist(member)));
        }
        let index = index.unwrap();

        let (callee_processed, this_type) = self.check_exp(*callee.clone())?;
        args.insert(0, *callee.clone());
        
        function_sig.params.insert(0, Parameter::named("this", this_type));
        let pure_sig = FunctionSignature {
            params: function_sig.params,
            return_type: function_sig.return_type,
            type_param_names: function_sig.type_param_names,
        };

        let call = self.check_call_base(pure_sig, String::from("##temp##"), args, type_args, full_prefix, &st_str)?;

        Ok((RExpression::FatCall {
            callee: Box::new(callee_processed),
            index_in_vtable: index,
            args: call.args,
        }, call.return_type))
    }
    pub(super) fn check_direct_member_call(&mut self, atom_type: CortexType, mut args: Vec<PExpression>, callee: Box<PExpression>, member: String, type_args: Option<Vec<CortexType>>, st_str: String) -> CheckResult<RExpression> {
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
    }

    pub(super) fn check_call(&mut self, addr: FunctionAddress, arg_exps: Vec<PExpression>, type_args: Option<Vec<CortexType>>, prefix: PathIdent, st_str: &String) -> CheckResult<RExpression> {
        let sig = self.lookup_signature(&FunctionAddress::concat(&prefix, &addr))?.clone();
        let extended_prefix = PathIdent::concat(&self.current_context, &prefix);
        let full_path = FunctionAddress::concat(&extended_prefix, &addr);
        let call = self.check_call_base(sig, full_path.codegen(0), arg_exps, type_args, prefix, st_str)?;
        let func_id = self.function_dict.add_call(full_path)?;
        Ok((RExpression::Call(func_id, call.args), call.return_type))
    }

    fn check_call_base(&mut self, sig: FunctionSignature, name: String, arg_exps: Vec<PExpression>, type_args: Option<Vec<CortexType>>, prefix: PathIdent, st_str: &String) -> Result<ProcessedCall, CortexError> {
        let provided_arg_count = arg_exps.len();
        let mut processed_args = Vec::new();
        let mut arg_types = Vec::new();
        for a in arg_exps.into_iter() {
            let (arg, typ) = self.check_exp(a)?;
            arg_types.push(typ);
            processed_args.push(arg);
        }
        
        let extended_prefix = PathIdent::concat(&self.current_context, &prefix);

        // let full_path = FunctionAddress::concat(&extended_prefix, &addr);
        if provided_arg_count != sig.params.len() {
            return Err(Box::new(
                PreprocessingError::MismatchedArgumentCount(name, sig.params.len(), provided_arg_count)
            ));
        }

        let mut return_type = sig
            .return_type
            .clone();

        let mut param_names = Vec::<String>::with_capacity(sig.params.len());
        let mut param_types = Vec::<CortexType>::with_capacity(sig.params.len());
        for param in &sig.params {
            param_names.push(param.name.clone());
            param_types.push(param.typ.clone().with_prefix_if_not_core(&extended_prefix));
        }

        let bindings;
        if let Some(type_args) = type_args {
            bindings = sig.type_param_names.iter().cloned().zip(type_args).collect();
        } else {
            bindings = self.infer_type_args(&sig, &arg_types, name, st_str)?;
        }
        let parent_type_env = self.current_type_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        let mut new_type_env = TypeEnvironment::new(*parent_type_env);
        for (name, typ) in &bindings {
            new_type_env.add(name.clone(), typ.clone());
        }
        self.current_type_env = Some(Box::new(new_type_env));

        for (i, arg_type) in arg_types.into_iter().enumerate() {
            let arg_type = self.clean_type(arg_type);
            let param_type = self.clean_type(param_types.get(i).unwrap().clone());
            if !arg_type.is_subtype_of(&param_type, &self.type_map) {
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
            .with_prefix_if_not_core(&extended_prefix);

        self.current_type_env = Some(Box::new(self.current_type_env.take().unwrap().exit()?));

        // let func_id = self.function_dict.add_call(full_path)?;
        Ok(ProcessedCall {
            args: processed_args,
            return_type,
        })
    }

    // Used to get bindings for a type (give param names and the concrete type)
    pub(super) fn get_bindings(type_param_names: &Vec<String>, typ: &CortexType) -> Result<HashMap<String, CortexType>, CortexError> {
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
    fn infer_type_args(&self, sig: &FunctionSignature, args: &Vec<CortexType>, name: String, st_str: &String) -> Result<HashMap<String, CortexType>, CortexError> {
        let mut bindings = HashMap::<String, CortexType>::new();
        for (arg, param) in args.iter().zip(&sig.params) {
            self.infer_arg(&param.typ, &arg, &sig.type_param_names, &mut bindings, param.name(), st_str)?;
        }

        if bindings.len() != sig.type_param_names.len() {
            Err(Box::new(PreprocessingError::CouldNotInferTypeBinding(name)))
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
                        let combined = bound_type.combine_with(existing_binding.clone(), &self.type_map);
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

    pub(super) fn lookup_signature(&self, path: &FunctionAddress) -> Result<&FunctionSignature, CortexError> {
        let full_path: FunctionAddress = FunctionAddress::concat(&self.current_context, &path);
        if let Some(sig) = self.function_signature_map.get(&full_path) {
            Ok(sig)
        } else {
            Err(Box::new(PreprocessingError::FunctionDoesNotExist(full_path.codegen(0))))
        }
    }
}
