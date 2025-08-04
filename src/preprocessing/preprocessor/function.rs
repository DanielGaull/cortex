use std::collections::HashMap;

use crate::{interpreting::error::CortexError, parsing::{ast::{expression::{PExpression, PathIdent}, top_level::ThisArg}, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::{expression::RExpression, function::RFunctionSignature, function_address::FunctionAddress, statement::RStatement, top_level::RParameter, r#type::{RFollowsClause, RFollowsEntry, RType, RTypeArg}}, error::PreprocessingError}, r#type::{r#type::{forwarded_type_args, TypeArg, TypeParam}, type_env::TypeEnvironment}};

use super::preprocessor::{CheckResult, CortexPreprocessor};

struct ProcessedCall {
    args: Vec<RExpression>,
    return_type: RType,
    statements: Vec<RStatement>,
}

impl CortexPreprocessor {
    pub(super) fn check_fat_member_call(&mut self, atom_type: RFollowsClause, callee: Box<PExpression>, member: String, mut args: Vec<PExpression>, type_args: Option<Vec<TypeArg>>, st_str: String) -> CheckResult<RExpression> {
        let mut function_sig = None;
        let mut contract_to_use = None;
        let mut full_name_of_contract = None;
        for entry in &atom_type.entries {
            let contract = self.lookup_contract(&entry.name)?;
            for sig in &contract.function_sigs {
                if sig.name == member.clone() {
                    function_sig = Some(sig);
                    contract_to_use = Some(contract);
                    full_name_of_contract = Some(self.get_contract_stub(&entry.name).unwrap().1);
                    break;
                }
            }
        }
        if let None = function_sig {
            return Err(Box::new(PreprocessingError::FunctionDoesNotExist(member)));
        }
        let mut function_sig = function_sig.unwrap().clone();
        let contract_to_use = contract_to_use.unwrap();
        let full_name_of_contract = full_name_of_contract.unwrap();

        let index = contract_to_use.function_sigs
            .iter()
            .position(|a| a.name == function_sig.name);

        if let None = index {
            return Err(Box::new(PreprocessingError::FunctionDoesNotExist(member)));
        }
        let index = index.unwrap();

        function_sig.type_params.extend(contract_to_use.type_params.clone());
        let this_type = RType::FollowsType(RFollowsClause {
            entries: vec![
                RFollowsEntry {
                    name: full_name_of_contract.clone(),
                    type_args: contract_to_use.type_params
                        .iter()
                        .map(|t| RTypeArg::Ty(RType::GenericType(t.name.clone())))
                        .collect(),
                }
            ]
        });
        function_sig.params.insert(0, RParameter::named("this", this_type));

        let (callee_processed, _, callee_st) = self.check_exp(*callee.clone(), None)?;
        let wrap_this_in_deref = function_sig.this_arg == ThisArg::DirectThis;
        if wrap_this_in_deref {
            args.insert(0, PExpression::DerefFat(Box::new(*callee.clone())));
        } else {
            args.insert(0, *callee.clone());
        }

        let pure_sig = RFunctionSignature {
            params: function_sig.params,
            return_type: function_sig.return_type,
            type_params: function_sig.type_params,
        };

        let type_args = if let Some(ta) = type_args {
            Some(self.validate_type_args(&pure_sig.type_params, ta, format!("{}::{}", full_name_of_contract.codegen(0), member), "Function")?)
        } else {
            None
        };

        let call = self.check_call_base(pure_sig, member.clone(), args, type_args, &st_str)?;
        let mut statements = Vec::new();
        statements.extend(callee_st);
        statements.extend(call.statements);

        Ok((RExpression::FatCall {
            callee: Box::new(callee_processed),
            index_in_vtable: index,
            args: call.args,
        }, call.return_type, statements))
    }
    pub(super) fn check_direct_member_call(&mut self, atom_type: RType, mut args: Vec<PExpression>, callee: Box<PExpression>, member: String, type_args: Option<Vec<TypeArg>>, st_str: String, expected_type: Option<RType>) -> CheckResult<RExpression> {
        let caller_type = atom_type.name()?;
        let actual_func_addr = self.get_member_function_address(&atom_type, &member)?;

        args.insert(0, *callee);
        let true_type_args;
        if let Some(mut type_args) = type_args {
            let typedef = self.lookup_type(&caller_type)?;
            let mut bindings = HashMap::new();
            self.infer_arg_type(&RType::RefType(
                Box::new(RType::BasicType(caller_type.clone(), forwarded_type_args(&typedef.type_params))),
                true,
            ), &atom_type, &typedef.type_params, &mut bindings, &String::from("this"), &st_str)?;
            let mut beginning_type_args = Vec::new();
            for a in &typedef.type_params {
                beginning_type_args.push(bindings.remove(a).unwrap());
            }
            type_args.extend(self.devalidate_type_args(beginning_type_args));
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

    pub(super) fn get_member_function_address(&self, callee_type: &RType, member: &String) -> Result<FunctionAddress, CortexError> {
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
        let (sig, sig_prefix_used) = self.lookup_signature(&FunctionAddress::concat(&prefix, &addr))?;
        let type_args = if let Some(type_args) = type_args {
            Some(self.validate_type_args(&sig.type_params, type_args, addr.codegen(0), "Function")?)
        } else {
            None
        };
        let extended_prefix = PathIdent::concat(&sig_prefix_used, &prefix);
        let full_path = FunctionAddress::concat(&extended_prefix, &addr);
        let call = self.check_call_base(sig.clone(), full_path.codegen(0), arg_exps, type_args, st_str)?;
        let func_id = self.function_dict.add_call(full_path)?;
        Ok((RExpression::Call { addr: func_id, args: call.args }, call.return_type, call.statements))
    }

    fn check_call_base(&mut self, sig: RFunctionSignature, name: String, arg_exps: Vec<PExpression>, type_args: Option<Vec<RTypeArg>>, st_str: &String) -> Result<ProcessedCall, CortexError> {
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
            let (arg, typ, st) = self.check_exp(a, Some(sig.params.get(i).unwrap().typ.clone()))?;
            statements.extend(st);
            arg_types.push(typ);
            processed_args.push(arg);
        }

        let mut return_type = sig.return_type.clone();

        let mut param_names = Vec::<String>::with_capacity(sig.params.len());
        let mut param_types = Vec::<RType>::with_capacity(sig.params.len());
        for param in &sig.params {
            param_names.push(param.name.clone());
            param_types.push(param.typ.clone());
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
            let arg_type = self.clean_type(arg_type)?;
            let param_type = self.clean_type(param_types.get(i).unwrap().clone())?;
            if !self.is_subtype(&arg_type, &param_type)? {
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
        
        return_type = self.clean_type(return_type)?;

        self.current_type_env = Some(Box::new(self.current_type_env.take().unwrap().exit()?));

        Ok(ProcessedCall {
            args: final_args,
            return_type,
            statements,
        })
    }

    // Used to get bindings for a type (give param names and the concrete type)
    pub(super) fn get_bindings(type_params: &Vec<TypeParam>, typ: &RType) -> Result<HashMap<TypeParam, RTypeArg>, CortexError> {
        let mut type_args_handled = false;
        let mut typ = typ.clone();
        let mut bindings = HashMap::new();
        while !type_args_handled {
            if let RType::BasicType(_, type_args) = &typ {
                bindings = TypeEnvironment::create_bindings(type_params, &type_args);
                typ = TypeEnvironment::fill_type(typ, &bindings)?;
                type_args_handled = true;
            } else if let RType::RefType(r, _) = typ {
                typ = *r;
            }
        }
        Ok(bindings)
    }
    fn infer_type_args(&self, param_names: &Vec<String>, param_types: &Vec<RType>, 
            type_params: &Vec<TypeParam>, args: &Vec<RType>, 
            name: String, st_str: &String) -> Result<HashMap<TypeParam, RTypeArg>, CortexError> {
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
    fn infer_arg(&self, param_type: &RTypeArg, arg_type: &RTypeArg, type_params: &Vec<TypeParam>, 
        bindings: &mut HashMap<TypeParam, RTypeArg>, param_name: &String, st_str: &String)
         -> Result<(), CortexError> {
        match (param_type, arg_type) {
            (RTypeArg::Ty(t1), RTypeArg::Ty(t2)) => {
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
            (RTypeArg::Ty(ty), RTypeArg::Int(_)) => {
                bindings.insert(TypeParam::int(ty.name()?.get_back()?), arg_type.clone());
                Ok(())
            },
            (_, _) => {
                Err(Box::new(PreprocessingError::CouldNotInferTypeBinding(param_type.codegen(0))))
            },
        }
    }
    fn infer_arg_type(&self, param_type: &RType, arg_type: &RType, type_params: &Vec<TypeParam>, 
        bindings: &mut HashMap<TypeParam, RTypeArg>, param_name: &String, st_str: &String) 
        -> Result<(), CortexError> {
        let correct;
        match (&param_type, arg_type) {
            (RType::BasicType(_, type_args), arg_type) => {
                if let RType::BasicType(_, type_args2) = arg_type {
                    if type_args.len() == type_args2.len() {
                        for (type_param, type_arg) in type_args.iter().zip(type_args2) {
                            self.infer_arg(type_param, type_arg, type_params, bindings, param_name, st_str)?;
                        }
                        correct = true;
                    } else {
                        correct = false;
                    }
                } else {
                    correct = false;
                }
            },
            (RType::RefType(r1, _), RType::RefType(r2, _)) => {
                self.infer_arg_type(&*r1, &*r2, type_params, bindings, param_name, st_str)?;
                correct = true;
            },
            (RType::TupleType(t1), RType::TupleType(t2)) => {
                if t1.len() == t2.len() {
                    for (type1, type2) in t1.iter().zip(t2) {
                        self.infer_arg_type(type1, type2, type_params, bindings, param_name, st_str)?;
                    }
                    correct = true;
                } else {
                    correct = false;
                }
            },
            (RType::FollowsType(f1), RType::FollowsType(f2)) => {
                // As an example, we might have something like `follows X` and `follows X+Y`
                let mut true_correct: bool = true;
                for entry in &f1.entries {
                    let matching = f2.entries
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
            (RType::FollowsType(follows), RType::BasicType(name, type_args)) => {
                let mut true_correct = true;
                if let Some(typedef) = self.type_map.get(&name) {
                    let follows_entries = 
                        TypeEnvironment::fill_in_follows_entry_from_typedef(type_args.clone(), typedef.type_params.clone(), typedef.followed_contracts.clone())?;
                    'top: for entry in &follows.entries {
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
                    return Err(Box::new(PreprocessingError::TypeDoesNotExist(name.codegen(0))));
                }
                correct = true_correct;
            },
            (RType::FollowsType(..), RType::RefType(r2, ..)) => {
                self.infer_arg_type(param_type, &*r2, type_params, bindings, param_name, st_str)?;
                correct = true;
            },
            (RType::OptionalType(..), RType::NoneType) |
            (RType::NoneType, RType::NoneType) => {
                correct = true;
            },
            (RType::OptionalType(o), other) => {
                self.infer_arg_type(o, other, type_params, bindings, param_name, st_str)?;
                correct = true;
            },
            (RType::GenericType(name1), other) => {
                bindings.insert(TypeParam::ty(name1), RTypeArg::Ty(other.clone()));
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

    fn search_for_extension(&self, typ: &PathIdent, member: &String) -> Result<Option<&FunctionAddress>, CortexError> {
        // What lets a function be a valid extension to call right now?
        // 1. The target types are exactly equal
        // 2. The "back" of the function's name is equal to the member path we're trying to call
        // 3. The function's name is fully prefixed by either current_context, or a path we have imported
        let mut candidates = Vec::new();
        for p in self.function_signature_map.keys() {
            // Must be prefixed by current_context or any imported path
            let mut all_valid_prefixes = Vec::new();
            all_valid_prefixes.push(&self.current_context);
            all_valid_prefixes.extend(self.imported_paths.iter());

            for prefix in all_valid_prefixes {
                let back = p.own_module_path.get_back()?;
                if back == member {
                    if p.own_module_path.is_fully_prefixed_by(prefix) {
                        // Must have the same target type to be able to be called
                        if let Some(target) = &p.target {
                            if target == typ {
                                candidates.push(p);
                            }
                        }
                    }
                }
            }
        }

        if candidates.len() > 1 {
            Err(Box::new(PreprocessingError::AmbiguousExtensionCall(member.clone(), typ.codegen(0))))
        } else {
            Ok(candidates.get(0).cloned())
        }
    }
}
