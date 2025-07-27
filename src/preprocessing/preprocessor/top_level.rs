use std::collections::{HashSet, VecDeque};

use crate::{interpreting::error::CortexError, parsing::{ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Contract, Extension, FunctionSignature, MemberFunction, PFunction, Struct, ThisArg, TopLevel}}, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::function_address::FunctionAddress, error::PreprocessingError, module::{Module, ModuleError, TypeDefinition}}, r#type::{r#type::{forwarded_type_args, CortexType, FollowsEntry, TypeParam}, type_env::TypeEnvironment}};

use super::preprocessor::CortexPreprocessor;

impl CortexPreprocessor {
    pub(super) fn lookup_type(&self, path: &PathIdent) -> Result<&TypeDefinition, CortexError> {
        if path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.get_back()?) {
                return self.lookup_type(resolved);
            }
        }
        let res = self.lookup_type_with(path, &self.current_context);
        match res {
            Ok(r) => Ok(r),
            Err(e) => {
                for prefix in &self.imported_paths {
                    let res = self.lookup_type_with(path, prefix);
                    if let Ok(r) = res {
                        return Ok(r);
                    }
                }
    
                Err(e)
            },
        }
    }
    pub(super) fn has_type(&self, path: &PathIdent) -> bool {
        self.lookup_type(path).is_ok()
    }
    fn lookup_type_with(&self, path: &PathIdent, prefix: &PathIdent) -> Result<&TypeDefinition, CortexError> {
        let full_path = PathIdent::concat(prefix, &path);
        if let Some(c) = self.type_map.get(&full_path) {
            Ok(c)
        } else {
            Err(Box::new(PreprocessingError::TypeDoesNotExist(full_path.codegen(0))))
        }
    }

    pub(super) fn lookup_contract(&self, path: &PathIdent) -> Result<&Contract, CortexError> {
        if path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.get_back()?) {
                return self.lookup_contract(resolved);
            }
        }
        let res = self.lookup_contract_with(path, &self.current_context);
        match res {
            Ok(r) => Ok(r),
            Err(e) => {
                for prefix in &self.imported_paths {
                    let res = self.lookup_contract_with(path, prefix);
                    if let Ok(r) = res {
                        return Ok(r);
                    }
                }
    
                Err(e)
            },
        }
    }
    fn lookup_contract_with(&self, path: &PathIdent, prefix: &PathIdent) -> Result<&Contract, CortexError> {
        let full_path = PathIdent::concat(&prefix, &path);
        if let Some(c) = self.contract_map.get(&full_path) {
            Ok(c)
        } else {
            Err(Box::new(PreprocessingError::ContractDoesNotExist(full_path.codegen(0))))
        }
    }

    pub(super) fn lookup_signature(&self, path: &FunctionAddress) -> Result<(&FunctionSignature, PathIdent), CortexError> {
        if path.own_module_path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.own_module_path.get_back()?) {
                return self.lookup_signature(&FunctionAddress {
                    own_module_path: resolved.clone(),
                    target: path.target.clone(),
                });
            }
        }
        if let Some(target) = &path.target {
            if target.is_final() {
                if let Some(resolved) = self.imported_aliases.get(target.get_back()?) {
                    // Need to check both the path where own_module_path is extended, and where it isn't
                    // For example, may `import drawing::Point as Point` but then `Point::getX` could be under
                    // either `drawing::getX (on type drawing::Point)` - if a regular member function,
                    // or under any other prefix that's imported (`_::getX (on type drawing::Point)`)
                    // if it's an extension function
                    // So, we check both cases - the basic one of it being a member function first since it's more common
                    let resolved_prefix = resolved.without_last();
                    let first_attempt = self.lookup_signature(&FunctionAddress {
                        own_module_path: PathIdent::concat(&resolved_prefix, &path.own_module_path),
                        target: Some(resolved.clone()),
                    });
                    if let Ok(result) = first_attempt {
                        return Ok((result.0, resolved_prefix));
                    } else {
                        return self.lookup_signature(&FunctionAddress {
                            own_module_path: path.own_module_path.clone(),
                            target: Some(resolved.clone()),
                        });
                    }
                }
            }
        }
        
        let res = self.lookup_signature_with(path, &self.current_context);
        match res {
            Ok(r) => Ok((r, self.current_context.clone())),
            Err(e) => {
                for prefix in &self.imported_paths {
                    let res = self.lookup_signature_with(path, prefix);
                    if let Ok(r) = res {
                        return Ok((r, prefix.clone()));
                    }
                }
    
                Err(e)
            },
        }
    }
    fn lookup_signature_with(&self, path: &FunctionAddress, prefix: &PathIdent) -> Result<&FunctionSignature, CortexError> {
        let full_path: FunctionAddress = FunctionAddress::concat(prefix, &path);
        if let Some(sig) = self.function_signature_map.get(&full_path) {
            Ok(sig)
        } else {
            Err(Box::new(PreprocessingError::FunctionDoesNotExist(full_path.codegen(0))))
        }
    }

    pub(super) fn has_function(&self, path: &FunctionAddress) -> bool {
        self.lookup_signature(path).is_ok()
        // let full_path: FunctionAddress = FunctionAddress::concat(&self.current_context, &path);
        // self.function_signature_map.contains_key(&full_path)
    }

    fn construct_module(contents: Vec<TopLevel>) -> Result<Module, CortexError> {
        let mut module = Module::new();
        for item in contents.into_iter() {
            match item {
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
                TopLevel::Extension(item) => {
                    module.add_extension(item)?;
                },
                TopLevel::Contract(item) => {
                    module.add_contract(item)?;
                },
            }
        }
        Ok(module)
    }

    pub fn run_top_level(&mut self, top_level: TopLevel) -> Result<(), CortexError> {
        match top_level {
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
            TopLevel::Contract(contract) => {
                self.add_contract(PathIdent::empty(), contract)?;
                Ok(())
            },
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
        let extensions = module.take_extensions()?;
        let contracts = module.take_contracts()?;

        let context_to_return_to = std::mem::replace(&mut self.current_context, path.clone());

        for item in contracts {
            self.add_contract(path.clone(), item)?;
        }

        for item in structs {
            self.add_struct(path.clone(), item, &mut functions)?;
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
        for t in &sig.type_params {
            if seen_type_param_names.contains(t) {
                return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.name.clone())));
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
    fn add_contract(&mut self, n: PathIdent, item: Contract) -> Result<(), CortexError> {
        let full_path = PathIdent::continued(n, item.name.clone());
        if self.contract_map.contains_key(&full_path) {
            Err(Box::new(ModuleError::ContractAlreadyExists(full_path.codegen(0))))
        } else {
            self.contract_map.insert(full_path, item);
            Ok(())
        }
    }
    
    fn add_struct(&mut self, n: PathIdent, item: Struct, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        let full_path = PathIdent::continued(n.clone(), item.name.clone());
        if self.has_type(&full_path) {
            Err(Box::new(ModuleError::TypeAlreadyExists(full_path.codegen(0))))
        } else {
            let has_loop = self.search_struct_for_loops(&item)?;
            if has_loop {
                return Err(Box::new(PreprocessingError::StructContainsCircularFields(full_path.codegen(0))));
            }

            if let Some(clause) = &item.follows_clause {
                self.check_contract_follows(&item.functions, &clause.contracts)?;
            }
            Self::handle_member_functions(item.functions, n, &item.type_params, &item.name, funcs_to_add)?;

            let mut seen_type_param_names = HashSet::new();
            for t in &item.type_params {
                if seen_type_param_names.contains(t) {
                    return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.name.clone())));
                }
                seen_type_param_names.insert(t);
            }

            self.type_map.insert(full_path, TypeDefinition {
                fields: item.fields,
                type_params: item.type_params,
                followed_contracts: item.follows_clause
                    .map(|f| f.contracts.clone())
                    .unwrap_or(vec![]),
            });
            Ok(())
        }
    }
    fn check_contract_follows(&self, functions: &Vec<MemberFunction>, contracts: &Vec<FollowsEntry>) -> Result<(), CortexError> {
        let mut methods_to_contain = Vec::new();
        let mut method_names = HashSet::new();
        let mut contract_paths = HashSet::new();

        for entry in contracts {
            if contract_paths.contains(&entry.name) {
                return Err(Box::new(PreprocessingError::DuplicateInFollowsClause(entry.name.codegen(0))));
            }
            contract_paths.insert(entry.name.clone());
            let contract = self.lookup_contract(&entry.name)?;
            let type_bindings = TypeEnvironment::create_bindings(&contract.type_params, &entry.type_args);
            for func in &contract.function_sigs {
                if let OptionalIdentifier::Ident(name) = func.name.clone() {
                    if method_names.contains(&name) {
                        return Err(Box::new(PreprocessingError::AmbiguousFunctionFromMultipleContracts(name.clone())));
                    }
                    method_names.insert(name);
                }
                methods_to_contain.push(func.clone().fill_all(&type_bindings)?);
            }
        }

        for func in functions {
            methods_to_contain.retain(|m| m != &func.signature);
        }

        if methods_to_contain.len() > 0 {
            let joint = methods_to_contain
                .iter()
                .filter_map(|m| match &m.name {
                    OptionalIdentifier::Ident(n) => Some(n.clone()),
                    OptionalIdentifier::Ignore => None,
                })
                .collect::<Vec<_>>()
                .join(", ");
            return Err(Box::new(PreprocessingError::ContractFunctionsMissing(joint)));
        }
        
        Ok(())
    }
    fn handle_member_functions(functions: Vec<MemberFunction>, n: PathIdent, item_type_params: &Vec<TypeParam>, item_name: &String, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        for func in functions {
            match func.signature.name {
                OptionalIdentifier::Ident(func_name) => {
                    let new_param = Parameter::named("this", Self::this_arg_to_type(func.signature.this_arg, item_name, item_type_params));
                    let mut param_list = vec![new_param];
                    param_list.extend(func.signature.params);
                    let mut type_param_names = func.signature.type_params;
                    let intersecting_type_param = item_type_params.iter().find(|t| type_param_names.contains(t));
                    if let Some(p) = intersecting_type_param {
                        return Err(Box::new(ModuleError::DuplicateTypeArgumentName(p.name.clone())));
                    }
                    type_param_names.extend(item_type_params.clone());
                    let new_func = PFunction::new(
                        OptionalIdentifier::Ident(func_name.clone()),
                        param_list,
                        func.signature.return_type,
                        func.body,
                        type_param_names,
                    );
                    let addr = FunctionAddress {
                        own_module_path: PathIdent::continued(n.clone(), func_name),
                        target: Some(PathIdent::continued(n.clone(), item_name.clone())),
                    };
                    funcs_to_add.push((addr, new_func));
                },
                OptionalIdentifier::Ignore => (),
            }
        }
        Ok(())
    }
    
    fn add_extension(&mut self, n: PathIdent, item: Extension, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        if let Some(clause) = &item.follows_clause {
            self.check_contract_follows(&item.functions, &clause.contracts)?;
        }

        let item_name = item.name.get_back()?;
        let item_prefix = item.name.without_last();
        for func in item.functions {
            match func.signature.name {
                OptionalIdentifier::Ident(func_name) => {
                    let new_param = Parameter::named("this", Self::this_arg_to_type(func.signature.this_arg, item_name, &item.type_params).with_prefix(&item_prefix));
                    let mut param_list = vec![new_param];
                    param_list.extend(func.signature.params);
                    let mut type_param_names = func.signature.type_params;
                    let intersecting_type_param = item.type_params.iter().find(|t| type_param_names.contains(t));
                    if let Some(type_param) = intersecting_type_param {
                        return Err(Box::new(ModuleError::DuplicateTypeArgumentName(type_param.name.clone())));
                    }
                    type_param_names.extend(item.type_params.clone());
                    let new_func = PFunction::new(
                        OptionalIdentifier::Ident(func_name.clone()),
                        param_list,
                        func.signature.return_type,
                        func.body,
                        type_param_names,
                    );
                    let addr = FunctionAddress {
                        own_module_path: PathIdent::continued(n.clone(), func_name),
                        target: Some(PathIdent::concat(&n, &item.name)),
                    };
                    funcs_to_add.push((addr, new_func));
                },
                OptionalIdentifier::Ignore => (),
            }
        }

        let followed_contracts = item.follows_clause
            .map(|f| f.contracts.clone())
            .unwrap_or(vec![]);
        self.type_map
            .get_mut(&PathIdent::concat(&n, &item.name))
            .map(|t| t.followed_contracts.extend(followed_contracts));

        Ok(())
    }

    fn this_arg_to_type(this_arg: ThisArg, item_name: &String, type_params: &Vec<TypeParam>) -> CortexType {
        match this_arg {
            ThisArg::RefThis => 
                CortexType::reference(
                    CortexType::basic(PathIdent::simple(item_name.clone()), forwarded_type_args(type_params)),
                    false,
                ),
            ThisArg::RefMutThis => 
                CortexType::reference(
                    CortexType::basic(PathIdent::simple(item_name.clone()), forwarded_type_args(type_params)),
                    true,
                ),
            ThisArg::DirectThis => CortexType::basic(PathIdent::simple(item_name.clone()), forwarded_type_args(type_params)),
        }
    }

    fn search_struct_for_loops(&self, s: &Struct) -> Result<bool, CortexError> {
        let stype = CortexType::basic(PathIdent::simple(s.name.clone()), forwarded_type_args(&s.type_params));
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
                if !matches!(typ, CortexType::BasicType(_)) {
                    continue;
                }

                // Enqueue all fields of this type
                let typ_name = &typ.name()?;

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
    }
}
