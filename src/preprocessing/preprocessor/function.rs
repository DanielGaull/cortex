use std::collections::HashMap;

use crate::{interpreting::error::CortexError, parsing::{ast::{expression::{OptionalIdentifier, PExpression, Parameter, PathIdent}, top_level::{FunctionSignature, ThisArg}, r#type::{forwarded_type_args, CortexType, FollowsClause, FollowsEntry, FollowsType, TypeArg, TypeParam}}, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::{expression::RExpression, function_address::FunctionAddress, statement::RStatement}, error::PreprocessingError, type_env::TypeEnvironment}};

use super::preprocessor::{CheckResult, CortexPreprocessor};

struct ProcessedCall {
    args: Vec<RExpression>,
    return_type: CortexType,
    statements: Vec<RStatement>,
}

impl CortexPreprocessor {
    pub(super) fn check_fat_member_call(&mut self, atom_type: FollowsType, callee: Box<PExpression>, member: String, mut args: Vec<PExpression>, type_args: Option<Vec<TypeArg>>, st_str: String) -> CheckResult<RExpression> {
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

        function_sig.type_params.extend(contract_to_use.type_params.clone());
        let this_type = CortexType::FollowsType(FollowsType {
            clause: FollowsClause {
                contracts: vec![
                    FollowsEntry {
                        name: PathIdent::simple(contract_to_use.name.clone()),
                        type_args: contract_to_use.type_params
                            .iter()
                            .map(|t| TypeArg::Ident(t.name.clone()))
                            .collect(),
                    }
                ]
            }
        });
        function_sig.params.insert(0, Parameter::named("this", this_type));

        let (callee_processed, _, callee_st) = self.check_exp(*callee.clone(), None)?;
        let wrap_this_in_deref = function_sig.this_arg == ThisArg::DirectThis;
        if wrap_this_in_deref {
            args.insert(0, PExpression::DerefFat(Box::new(*callee.clone())));
        } else {
            args.insert(0, *callee.clone());
        }

        let pure_sig = FunctionSignature {
            params: function_sig.params,
            return_type: function_sig.return_type,
            type_params: function_sig.type_params,
        };

        let call = self.check_call_base(pure_sig, member.clone(), args, type_args, full_prefix, &st_str)?;
        let mut statements = Vec::new();
        statements.extend(callee_st);
        statements.extend(call.statements);

        Ok((RExpression::FatCall {
            callee: Box::new(callee_processed),
            index_in_vtable: index,
            args: call.args,
        }, call.return_type, statements))
    }
    pub(super) fn check_direct_member_call(&mut self, atom_type: CortexType, mut args: Vec<PExpression>, callee: Box<PExpression>, member: String, type_args: Option<Vec<TypeArg>>, st_str: String, expected_type: Option<CortexType>) -> CheckResult<RExpression> {
        let caller_type = atom_type.name()?;
        let actual_func_addr = self.get_member_function_address(&atom_type, &member)?;

        args.insert(0, *callee);
        let true_type_args;
        if let Some(mut type_args) = type_args {
            let typedef = self.lookup_type(caller_type)?;
            let mut bindings = HashMap::new();
            self.infer_arg_type(&CortexType::reference(
                CortexType::basic(caller_type.clone(), forwarded_type_args(&typedef.type_params)),
                true,
            ), &atom_type, &typedef.type_params, &mut bindings, &String::from("this"), &st_str)?;
            let mut beginning_type_args = Vec::new();
            for a in &typedef.type_params {
                beginning_type_args.push(bindings.remove(a).unwrap());
            }
            type_args.extend(beginning_type_args);
            true_type_args = Some(type_args);
        } else {
            true_type_args = None;
        }

        let call_exp = PExpression::Call {
            name: actual_func_addr, 
            args,
            type_args: true_type_args,
        };
        let result = self.check_exp(call_exp, expected_type)?;
        Ok(result)
    }

    pub(super) fn get_member_function_address(&self, callee_type: &CortexType, member: &String) -> Result<FunctionAddress, CortexError> {
        let caller_type = callee_type.name()?;
        let caller_type_prefix = caller_type.without_last();
        let non_extension_func_addr = FunctionAddress::member_func(
            PathIdent::continued(caller_type_prefix.clone().subtract(&self.current_context)?, member.clone()), 
            caller_type.clone().subtract(&self.current_context)?);

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

        Ok(actual_func_addr)
    }

    pub(super) fn check_call(&mut self, addr: FunctionAddress, arg_exps: Vec<PExpression>, type_args: Option<Vec<TypeArg>>, prefix: PathIdent, st_str: &String) -> CheckResult<RExpression> {
        let sig = self.lookup_signature(&FunctionAddress::concat(&prefix, &addr))?.clone();
        let extended_prefix = PathIdent::concat(&self.current_context, &prefix);
        let full_path = FunctionAddress::concat(&extended_prefix, &addr);
        let call = self.check_call_base(sig, full_path.codegen(0), arg_exps, type_args, prefix, st_str)?;
        let func_id = self.function_dict.add_call(full_path)?;
        Ok((RExpression::Call(func_id, call.args), call.return_type, call.statements))
    }

    fn check_call_base(&mut self, sig: FunctionSignature, name: String, arg_exps: Vec<PExpression>, type_args: Option<Vec<TypeArg>>, prefix: PathIdent, st_str: &String) -> Result<ProcessedCall, CortexError> {
        let provided_arg_count = arg_exps.len();
        if provided_arg_count != sig.params.len() {
            return Err(Box::new(
                PreprocessingError::MismatchedArgumentCount(name, sig.params.len(), provided_arg_count)
            ));
        }

        let mut processed_args = Vec::new();
        let mut arg_types = Vec::new();
        let mut statements = Vec::new();
        for (i, a) in arg_exps.into_iter().enumerate() {
            let (arg, typ, st) = self.check_exp(a, Some(sig.params.get(i).unwrap().param_type().clone()))?;
            statements.extend(st);
            arg_types.push(typ);
            processed_args.push(arg);
        }
        
        let extended_prefix = PathIdent::concat(&self.current_context, &prefix);

        let mut return_type = sig
            .return_type
            .clone();

        let mut param_names = Vec::<String>::with_capacity(sig.params.len());
        let mut param_types = Vec::<CortexType>::with_capacity(sig.params.len());
        for param in &sig.params {
            param_names.push(param.name.clone());
            if let Some(_) = TypeEnvironment::does_arg_list_contain(&sig.type_params, &param.typ) {
                param_types.push(param.typ.clone());
            } else {
                param_types.push(param.typ.clone().with_prefix_if_not_core(&extended_prefix));
            }
        }

        let bindings;
        if let Some(type_args) = type_args {
            bindings = sig.type_params.iter().cloned().zip(type_args).collect();
        } else {
            bindings = self.infer_type_args(&param_names, &param_types, &sig.type_params,
                &arg_types, name, st_str)?;
        }
        let parent_type_env = self.current_type_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        let mut new_type_env = TypeEnvironment::new(*parent_type_env);
        for (name, typ) in &bindings {
            new_type_env.add(name.clone(), typ.clone());
        }
        self.current_type_env = Some(Box::new(new_type_env));

        let mut final_args = Vec::new();
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

            let arg = processed_args.remove(0);
            let (arg, st) = self.assign_to(arg, arg_type, param_type)?;
            final_args.push(arg);
            statements.extend(st);
        }
        
        return_type = self.clean_type(return_type);
        if let None = TypeEnvironment::does_arg_list_contain(&sig.type_params, &return_type) {
            return_type = return_type.with_prefix_if_not_core(&extended_prefix);
        }

        self.current_type_env = Some(Box::new(self.current_type_env.take().unwrap().exit()?));

        Ok(ProcessedCall {
            args: final_args,
            return_type,
            statements,
        })
    }

    // Used to get bindings for a type (give param names and the concrete type)
    pub(super) fn get_bindings(type_params: &Vec<TypeParam>, typ: &CortexType) -> Result<HashMap<TypeParam, TypeArg>, CortexError> {
        let mut type_args_handled = false;
        let mut typ = typ.clone();
        let mut bindings = HashMap::new();
        while !type_args_handled {
            if let CortexType::BasicType(b) = &typ {
                bindings = TypeEnvironment::create_bindings(type_params, &b.type_args);
                typ = TypeEnvironment::fill_type(typ, &bindings);
                type_args_handled = true;
            } else if let CortexType::RefType(r) = typ {
                typ = *r.contained;
            }
        }
        Ok(bindings)
    }
    fn infer_type_args(&self, param_names: &Vec<String>, param_types: &Vec<CortexType>, 
            type_params: &Vec<TypeParam>, args: &Vec<CortexType>, 
            name: String, st_str: &String) -> Result<HashMap<TypeParam, TypeArg>, CortexError> {
        let mut bindings = HashMap::new();
        for (arg, param) in args.iter().zip(param_names.iter().zip(param_types)) {
            self.infer_arg_type(&param.1, &arg, type_params, &mut bindings, param.0, st_str)?;
        }

        if bindings.len() != type_params.len() {
            Err(Box::new(PreprocessingError::CouldNotInferTypeBinding(name)))
        } else {
            Ok(bindings)
        }
    }
    fn infer_arg(&self, param_type: &TypeArg, arg_type: &TypeArg, type_params: &Vec<TypeParam>, 
        bindings: &mut HashMap<TypeParam, TypeArg>, param_name: &String, st_str: &String)
         -> Result<(), CortexError> {
        match (param_type, arg_type) {
            (TypeArg::Ty(t1), TypeArg::Ty(t2)) => {
                self.infer_arg_type(
                    t1,
                    t2,
                    type_params,
                    bindings,
                    param_name,
                    st_str,
                )?;
                Ok(())
            },
            (TypeArg::Ty(t1), TypeArg::Ident(name)) => {
                self.infer_arg_type(
                    t1,
                    &CortexType::basic_simple(&name, vec![]),
                    type_params,
                    bindings,
                    param_name,
                    st_str,
                )?;
                Ok(())
            },
            (TypeArg::Ident(name), TypeArg::Int(_)) => {
                bindings.insert(TypeParam::int(name), arg_type.clone());
                Ok(())
            },
            (_, _) => {
                Err(Box::new(PreprocessingError::CouldNotInferTypeBinding(param_type.codegen(0))))
            },
        }
    }
    fn infer_arg_type(&self, param_type: &CortexType, arg_type: &CortexType, type_params: &Vec<TypeParam>, 
        bindings: &mut HashMap<TypeParam, TypeArg>, param_name: &String, st_str: &String) 
        -> Result<(), CortexError> {
        let correct;
        match (&param_type, arg_type) {
            (CortexType::BasicType(b), arg_type) => {
                if let Some(name) = TypeEnvironment::does_arg_list_contain(type_params, &param_type) {
                    let bound_type = arg_type.clone();
                    if b.type_args.len() > 0 {
                        return Err(Box::new(PreprocessingError::CannotHaveTypeArgsOnGeneric(param_type.codegen(0))));
                    }
                    if let Some(existing_binding) = bindings.get(name) {
                        if let TypeArg::Ty(ty) = existing_binding {
                            let combined = bound_type.combine_with(ty.clone(), &self.type_map);
                            if let Some(result) = combined {
                                bindings.insert(name.clone(), TypeArg::Ty(result));
                                correct = true;
                            } else {
                                correct = false;
                            }
                        } else {
                            correct = false;
                        }
                    } else {
                        bindings.insert(name.clone(), TypeArg::Ty(bound_type));
                        correct = true;
                    }
                } else {
                    // Try to match up type args (ex. list<T> to list<number>)
                    // If both are not BasicType, then we just ignore this
                    if let CortexType::BasicType(b2) = arg_type {
                        if b.type_args.len() == b2.type_args.len() {
                            for (type_param, type_arg) in b.type_args.iter().zip(&b2.type_args) {
                                self.infer_arg(type_param, type_arg, type_params, bindings, param_name, st_str)?;
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
                self.infer_arg_type(&*r.contained, &*r2.contained, type_params, bindings, param_name, st_str)?;
                correct = true;
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    for (type1, type2) in t1.types.iter().zip(&t2.types) {
                        self.infer_arg_type(type1, type2, type_params, bindings, param_name, st_str)?;
                    }
                    correct = true;
                } else {
                    correct = false;
                }
            },
            (CortexType::FollowsType(f1), CortexType::FollowsType(f2)) => {
                // As an example, we might have something like `follows X` and `follows X+Y`
                let mut true_correct: bool = true;
                for entry in &f1.clause.contracts {
                    let matching = f2.clause.contracts
                        .iter()
                        .find(|x| x.name == entry.name);
                    if let Some(matching) = matching {
                        if matching.type_args.len() != entry.type_args.len() {
                            true_correct = false;
                            break;
                        }
                        for (t1, t2) in entry.type_args.iter().zip(&matching.type_args) {
                            self.infer_arg(t1, t2, type_params, bindings, param_name, st_str)?;
                        }
                    } else {
                        true_correct = false;
                        break;
                    }
                }
                correct = true_correct;
            },
            (CortexType::FollowsType(follows), CortexType::BasicType(basic)) => {
                let mut true_correct = true;
                if let Some(typedef) = self.type_map.get(&basic.name) {
                    let follows_entries = 
                        TypeEnvironment::fill_in_follows_entry_from_typedef(basic.clone(), typedef.type_params.clone(), typedef.followed_contracts.clone());
                    'top: for entry in &follows.clause.contracts {
                        let mut found = false;
                        for def_entry in &follows_entries {
                            if def_entry.name == entry.name {
                                if def_entry.type_args.len() == entry.type_args.len() {
                                    for (ta1, ta2) in entry.type_args.iter().zip(&def_entry.type_args) {
                                        self.infer_arg(ta1, ta2, type_params, bindings, param_name, st_str)?;
                                    }
                                } else {
                                    true_correct = false;
                                    break 'top;
                                }
                                found = true;
                            }
                        }
                        if !found {
                            true_correct = false;
                            break;
                        }
                    }
                } else {
                    return Err(Box::new(PreprocessingError::TypeDoesNotExist(basic.name.codegen(0))));
                }
                correct = true_correct;
            },
            (CortexType::FollowsType(_), CortexType::RefType(r2)) => {
                self.infer_arg_type(param_type, &*r2.contained, type_params, bindings, param_name, st_str)?;
                correct = true;
            },
            (CortexType::OptionalType(_), CortexType::NoneType) |
            (CortexType::NoneType, CortexType::NoneType) => {
                correct = true;
            },
            (CortexType::OptionalType(o), other) => {
                self.infer_arg_type(o, other, type_params, bindings, param_name, st_str)?;
                correct = true;
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
}
