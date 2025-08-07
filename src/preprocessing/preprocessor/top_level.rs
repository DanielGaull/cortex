use std::collections::{HashMap, HashSet, VecDeque};

use crate::{interpreting::error::CortexError, parsing::{ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Contract, Extension, FunctionSignature, Import, MemberFunction, MemberFunctionSignature, PFunction, Struct, ThisArg, TopLevel}}, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::{function::RFunctionSignature, function_address::FunctionAddress, top_level::{RContract, RMemberFunctionSignature, RParameter}, r#type::{is_path_a_core_type, RFollowsEntry, RType}}, error::PreprocessingError, module::{Module, ModuleError, TypeDefinition}}, r#type::{r#type::{forwarded_type_args, forwarded_type_args_unvalidated, CortexType, FollowsEntry, TypeParam}, type_env::TypeEnvironment}};

use super::preprocessor::CortexPreprocessor;

impl CortexPreprocessor {
    pub(super) fn lookup_type(&self, path: &PathIdent) -> Result<&TypeDefinition, CortexError> {
        if path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.get_back()?) {
                return self.lookup_type_with(resolved, &PathIdent::empty());
            }
        }
        let res = self.lookup_type_with(path, &self.current_context);
        match res {
            Ok(r) => Ok(r),
            Err(e) => {
                let mut valid_prefixes = self.imported_paths.clone();
                valid_prefixes.push(PathIdent::empty());
                for prefix in &valid_prefixes {
                    let res = self.lookup_type_with(path, prefix);
                    if let Ok(r) = res {
                        return Ok(r);
                    }
                }
    
                Err(e)
            },
        }
    }
    fn lookup_type_with(&self, path: &PathIdent, prefix: &PathIdent) -> Result<&TypeDefinition, CortexError> {
        let full_path = if is_path_a_core_type(path) {
            path.clone()
        } else {
            PathIdent::concat(prefix, &path)
        };
        if let Some(c) = self.type_map.get(&full_path) {
            Ok(c)
        } else {
            Err(Box::new(PreprocessingError::TypeDoesNotExist(full_path.codegen(0))))
        }
    }
    
    pub(super) fn has_struct(&self, path: &PathIdent) -> bool {
        self.get_struct_stub(path).is_some()
    }
    // Returns the type params for the struct, as well as the actual path to address the struct
    pub(super) fn get_struct_stub(&self, path: &PathIdent) -> Option<(&Vec<TypeParam>, PathIdent)> {
        if path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.get_back().unwrap()) {
                return self.get_struct_stub_with(resolved, &PathIdent::empty());
            }
        }
        let res = self.get_struct_stub_with(path, &self.current_context);
        if let Some(res) = res {
            Some(res)
        } else {
            let mut valid_prefixes = self.imported_paths.clone();
            valid_prefixes.push(PathIdent::empty());
            for prefix in &valid_prefixes {
                let res = self.get_struct_stub_with(path, prefix);
                if let Some(res) = res {
                    return Some(res);
                }
            }
            None
        }
    }
    fn get_struct_stub_with(&self, path: &PathIdent, prefix: &PathIdent) -> Option<(&Vec<TypeParam>, PathIdent)> {
        let full_path = if is_path_a_core_type(path) {
            path.clone()
        } else {
            PathIdent::concat(prefix, &path)
        };
        self.stubbed_structs.get(&full_path).map(|p| (p, full_path))
    }

    pub(super) fn lookup_contract(&self, path: &PathIdent) -> Result<&RContract, CortexError> {
        if path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.get_back()?) {
                return self.lookup_contract_with(resolved, &PathIdent::empty());
            }
        }
        let res = self.lookup_contract_with(path, &self.current_context);
        match res {
            Ok(r) => Ok(r),
            Err(e) => {
                let mut valid_prefixes = self.imported_paths.clone();
                valid_prefixes.push(PathIdent::empty());
                for prefix in &valid_prefixes {
                    let res = self.lookup_contract_with(path, prefix);
                    if let Ok(r) = res {
                        return Ok(r);
                    }
                }
    
                Err(e)
            },
        }
    }
    fn lookup_contract_with(&self, path: &PathIdent, prefix: &PathIdent) -> Result<&RContract, CortexError> {
        let full_path = PathIdent::concat(&prefix, &path);
        if let Some(c) = self.contract_map.get(&full_path) {
            Ok(c)
        } else {
            Err(Box::new(PreprocessingError::ContractDoesNotExist(full_path.codegen(0))))
        }
    }

    pub(super) fn has_contract(&self, path: &PathIdent) -> bool {
        self.get_contract_stub(path).is_some()
    }
    pub(super) fn get_contract_stub(&self, path: &PathIdent) -> Option<(&Vec<TypeParam>, PathIdent)> {
        if path.is_final() {
            if let Some(resolved) = self.imported_aliases.get(path.get_back().unwrap()) {
                return self.get_contract_stub_with(resolved, &PathIdent::empty());
            }
        }
        let res = self.get_contract_stub_with(path, &self.current_context);
        if let Some(res) = res {
            Some(res)
        } else {
            let mut valid_prefixes = self.imported_paths.clone();
            valid_prefixes.push(PathIdent::empty());
            for prefix in &valid_prefixes {
                let res = self.get_contract_stub_with(path, prefix);
                if let Some(res) = res {
                    return Some(res);
                }
            }
            None
        }
    }
    fn get_contract_stub_with(&self, path: &PathIdent, prefix: &PathIdent) -> Option<(&Vec<TypeParam>, PathIdent)> {
        let full_path = if is_path_a_core_type(path) {
            path.clone()
        } else {
            PathIdent::concat(prefix, &path)
        };
        self.stubbed_contracts.get(&full_path).map(|c| (c, full_path))
    }

    pub(super) fn lookup_signature(&self, path: &FunctionAddress) -> Result<(&RFunctionSignature, PathIdent), CortexError> {
        // If target is None, then this is as simple as the struct/contract lookup system
        // Because member functions cannot be directly imported, if target is Some, then we have these restrictions:
        // own_module_path must come from the same module as the target (since extensions are handled separately)
        // It *cannot* be an alias
        if path.target.is_none() {
            // Simple case mentioned above
            let own_module_path = &path.own_module_path;
            if own_module_path.is_final() {
                if let Some(resolved) = self.imported_aliases.get(own_module_path.get_back().unwrap()) {
                    let sig = FunctionAddress::simple(resolved.clone());
                    return Ok((self.lookup_signature_with(&sig, &PathIdent::empty())?, PathIdent::empty()));
                }
            }
        }
        let res = self.lookup_signature_with(&path, &self.current_context);
        match res {
            Ok(res) => Ok((res, self.current_context.clone())),
            Err(e) => {
                let mut valid_prefixes = self.imported_paths.clone();
                valid_prefixes.push(PathIdent::empty());
                for prefix in &valid_prefixes {
                    let res = self.lookup_signature_with(&path, prefix);
                    if let Ok(res) = res {
                        return Ok((res, prefix.clone()));
                    }
                }
                Err(e)
            },
        }
    }
    fn lookup_signature_with(&self, path: &FunctionAddress, prefix: &PathIdent) -> Result<&RFunctionSignature, CortexError> {
        let full_path: FunctionAddress = FunctionAddress::concat(prefix, &path);
        if let Some(sig) = self.function_signature_map.get(&full_path) {
            Ok(sig)
        } else {
            Err(Box::new(PreprocessingError::FunctionDoesNotExist(full_path.codegen(0))))
        }
    }

    pub(super) fn has_function(&self, path: &FunctionAddress) -> bool {
        self.get_function_stub(path).is_some()
    }
    pub(super) fn get_function_stub(&self, path: &FunctionAddress) -> Option<(&Vec<TypeParam>, FunctionAddress)> {
        if path.target.is_none() {
            let own_module_path = &path.own_module_path;
            if own_module_path.is_final() {
                if let Some(resolved) = self.imported_aliases.get(own_module_path.get_back().unwrap()) {
                    let sig = FunctionAddress::simple(resolved.clone());
                    return self.get_function_stub_with(&sig, &PathIdent::empty());
                }
            }
        }
        
        let res = self.get_function_stub_with(path, &self.current_context);
        if let Some(res) = res {
            Some(res)
        } else {
            let mut valid_prefixes = self.imported_paths.clone();
            valid_prefixes.push(PathIdent::empty());
            for prefix in &valid_prefixes {
                let res = self.get_function_stub_with(path, prefix);
                if let Some(res) = res {
                    return Some(res);
                }
            }
            None
        }
    }
    fn get_function_stub_with(&self, path: &FunctionAddress, prefix: &PathIdent) -> Option<(&Vec<TypeParam>, FunctionAddress)> {
        let full_path = FunctionAddress::concat(prefix, &path);
        self.stubbed_functions.get(&full_path).map(|f| (f, full_path))
    }

    fn construct_module_helper(&mut self, imports: Vec<Import>, contents: Vec<TopLevel>, maintain_imports: bool) -> Result<Module, CortexError> {
        let mut module = Module::new();
        for import in imports {
            self.handle_import(import)?;
        }
        for item in contents.into_iter() {
            match item {
                TopLevel::Module { name: submod_name, contents } => {
                    let new_module = self.construct_module(contents.imports, contents.content, maintain_imports)?;
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
    pub(crate) fn construct_module(&mut self, imports: Vec<Import>, contents: Vec<TopLevel>, maintain_imports: bool) -> Result<Module, CortexError> {
        if maintain_imports {
            self.construct_module_helper(imports, contents, maintain_imports)
        } else {
            let aliases_to_restore = std::mem::replace(&mut self.imported_aliases, HashMap::new());
            let paths_to_restore = std::mem::replace(&mut self.imported_paths, Vec::new());

            let result = self.construct_module_helper(imports, contents, maintain_imports);

            self.imported_aliases = aliases_to_restore;
            self.imported_paths = paths_to_restore;

            result
        }
    }

    pub fn run_top_level(&mut self, top_level: TopLevel) -> Result<(), CortexError> {
        let module = self.construct_module(vec![], vec![top_level], true)?;
        self.register_module(&PathIdent::empty(), module)?;
        Ok(())
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

        for s in &structs {
            let struct_path = PathIdent::continued(path.clone(), s.name.clone());
            if self.stubbed_structs.contains_key(&struct_path) {
                return Err(Box::new(ModuleError::StructAlreadyExists(struct_path.codegen(0))));
            }
            self.stubbed_structs.insert(struct_path.clone(), s.type_params.clone());

            for mf in &s.functions {
                if let OptionalIdentifier::Ident(name) = &mf.signature.name {
                    let function_path = FunctionAddress::member_func(
                        PathIdent::continued(path.clone(), name.clone()),
                        struct_path.clone()
                    );
                    if self.stubbed_functions.contains_key(&function_path) {
                        return Err(Box::new(ModuleError::FunctionAlreadyExists(function_path.codegen(0))));
                    }

                    self.stubbed_functions.insert(function_path, mf.signature.type_params.clone());
                }
            }
        }
        for c in &contracts {
            let contract_path = PathIdent::continued(path.clone(), c.name.clone());
            if self.stubbed_contracts.contains_key(&contract_path) {
                return Err(Box::new(ModuleError::ContractAlreadyExists(contract_path.codegen(0))));
            }
            self.stubbed_contracts.insert(contract_path, c.type_params.clone());
        }
        for (addr, f) in &functions {
            let function_path = FunctionAddress::concat(path, addr);
            if self.stubbed_functions.contains_key(&function_path) {
                return Err(Box::new(ModuleError::FunctionAlreadyExists(function_path.codegen(0))));
            }
            self.stubbed_functions.insert(function_path, f.type_params.clone());
        }
        for e in &extensions {
            let struct_path = &e.name;
            for mf in &e.functions {
                if let OptionalIdentifier::Ident(name) = &mf.signature.name {
                    let function_path = FunctionAddress::member_func(
                        PathIdent::continued(path.clone(), name.clone()),
                        struct_path.clone()
                    );
                    if self.stubbed_functions.contains_key(&function_path) {
                        return Err(Box::new(ModuleError::FunctionAlreadyExists(function_path.codegen(0))));
                    }

                    self.stubbed_functions.insert(function_path, mf.signature.type_params.clone());
                }
            }
        }

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
        let mut seen_type_param_names = HashSet::new();
        for t in &sig.type_params {
            if seen_type_param_names.contains(t) {
                return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.name.clone())));
            }
            seen_type_param_names.insert(t);
        }
        self.function_signature_map.insert(addr.clone(), self.validate_function_signature(sig)?);
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
        let mut members = Vec::new();
        for m in item.function_sigs {
            members.push(self.validate_member_function_signature(m)?);
        }

        self.contract_map.insert(full_path, RContract {
            type_params: item.type_params,
            function_sigs: members,
        });
        Ok(())
    }
    
    fn add_struct(&mut self, n: PathIdent, item: Struct, funcs_to_add: &mut Vec<(FunctionAddress, PFunction)>) -> Result<(), CortexError> {
        let full_path = PathIdent::continued(n.clone(), item.name.clone());
        let mut fields = HashMap::new();
        for f in &item.fields {
            fields.insert(f.0.clone(), self.validate_type(f.1.clone())?);
        }
        let has_loop = self.search_struct_for_loops(&item.name, &item.type_params, fields.values().cloned().into_iter().collect())?;
        if has_loop {
            return Err(Box::new(PreprocessingError::StructContainsCircularFields(full_path.codegen(0))));
        }
        
        if let Some(clause) = &item.follows_clause {
            self.check_contract_follows(
                &self.validate_member_function_signatures(&item.functions)?,
                &self.validate_follows_entries(clause.contracts.clone())?
            )?;
        }
        Self::handle_member_functions(item.functions, n, &item.type_params, &item.name, funcs_to_add)?;

        let mut seen_type_param_names = HashSet::new();
        for t in &item.type_params {
            if seen_type_param_names.contains(t) {
                return Err(Box::new(ModuleError::DuplicateTypeArgumentName(t.name.clone())));
            }
            seen_type_param_names.insert(t);
        }

        let followed_contracts = if let Some(clause) = item.follows_clause {
            self.validate_follows_entries(clause.contracts)?
        } else {
            vec![]
        };

        self.type_map.insert(full_path, TypeDefinition {
            fields: fields,
            type_params: item.type_params,
            followed_contracts,
        });
        Ok(())
    }
    fn check_contract_follows(&self, functions: &Vec<RMemberFunctionSignature>, contracts: &Vec<RFollowsEntry>) -> Result<(), CortexError> {
        let mut methods_to_contain = Vec::new();
        let mut method_names = HashSet::new();
        let mut contract_paths = HashSet::new();

        for entry in contracts {
            let name = entry.name.clone().subtract_if_possible(&self.current_context);
            if contract_paths.contains(&name) {
                return Err(Box::new(PreprocessingError::DuplicateInFollowsClause(name.codegen(0))));
            }
            contract_paths.insert(name.clone());
            let contract = self.lookup_contract(&name)?;
            let type_bindings = TypeEnvironment::create_bindings(&contract.type_params, &entry.type_args);
            for func in &contract.function_sigs {
                if method_names.contains(&func.name) {
                    return Err(Box::new(PreprocessingError::AmbiguousFunctionFromMultipleContracts(func.name.clone())));
                }
                method_names.insert(func.name.clone());
                methods_to_contain.push(func.clone().fill_all(&type_bindings)?);
            }
        }

        for signature in functions {
            methods_to_contain.retain(|m| m != signature);
        }

        if methods_to_contain.len() > 0 {
            let joint = methods_to_contain
                .iter()
                .map(|m| m.name.clone())
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
            self.check_contract_follows(
                &self.validate_member_function_signatures(&item.functions)?,
                &self.validate_follows_entries(clause.contracts.clone())?
            )?;
        }

        let item_as_type = CortexType::basic(item.name.clone(), item.type_args.clone());
        let validated = self.validate_type(item_as_type)?;
        let item_name;
        let item_prefix;
        let validated_target_path;
        match validated {
            RType::BasicType(path, _) => {
                validated_target_path = path.clone();
                item_name = path.get_back()?.clone();
                item_prefix = path.without_last();
            },
            _ => {
                return Err(Box::new(PreprocessingError::InvalidTypeProvidedToExtendBlock(validated.codegen(0))));
            }
        }

        for func in item.functions {
            match func.signature.name {
                OptionalIdentifier::Ident(func_name) => {
                    let new_param = Parameter::named("this", Self::this_arg_to_type(func.signature.this_arg, &item_name, &item.type_params).with_prefix(&item_prefix));
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
                        target: Some(validated_target_path.clone()),
                    };
                    funcs_to_add.push((addr, new_func));
                },
                OptionalIdentifier::Ignore => (),
            }
        }

        let followed_contracts = self.validate_follows_entries(item.follows_clause
            .map(|f| f.contracts.clone())
            .unwrap_or(vec![])
        )?;
        self.type_map
            .get_mut(&PathIdent::concat(&n, &item.name))
            .map(|t| t.followed_contracts.extend(followed_contracts));

        Ok(())
    }

    fn this_arg_to_type(this_arg: ThisArg, item_name: &String, type_params: &Vec<TypeParam>) -> CortexType {
        match this_arg {
            ThisArg::RefThis => 
            CortexType::reference(
                CortexType::basic(PathIdent::simple(item_name.clone()), forwarded_type_args_unvalidated(type_params)),
                    false,
                ),
            ThisArg::RefMutThis => 
            CortexType::reference(
                CortexType::basic(PathIdent::simple(item_name.clone()), forwarded_type_args_unvalidated(type_params)),
                    true,
                ),
            ThisArg::DirectThis => CortexType::basic(PathIdent::simple(item_name.clone()), forwarded_type_args_unvalidated(type_params)),
        }
    }

    fn search_struct_for_loops(&self, name: &String, type_params: &Vec<TypeParam>, fields: Vec<RType>) -> Result<bool, CortexError> {
        let stype = RType::basic(PathIdent::simple(name.clone()), forwarded_type_args(type_params));
        let mut q = VecDeque::new();
        for field in fields {
            q.push_back(field);
        }
        // Only need to search for references to this struct, everything else should be fine
        while !q.is_empty() {
            let typ = q.pop_front().unwrap();
            if typ == stype {
                return Ok(true);
            }
            if !typ.is_core() {
                if !matches!(typ, RType::BasicType(..)) {
                    continue;
                }

                // Enqueue all fields of this type
                let typ_name = &typ.name()?;

                // It's ok if the struct doesn't exist yet
                // If it has loops, then they will be caught when we visit this function upon registering it
                // Unfortunately, the order in which structs are added is not deterministic
                if self.has_struct(typ_name) {
                    let struc = self.lookup_type(typ_name)?;
                    for field in &struc.fields {
                        q.push_back(field.1.clone());
                    }
                }
            }
        }
        Ok(false)
    }

    fn validate_function_signature(&self, sig: FunctionSignature) -> Result<RFunctionSignature, CortexError> {
        let mut params = Vec::new();
        for p in sig.params {
            params.push(RParameter {
                name: p.name,
                typ: self.validate_type(p.typ)?
            });
        }
        let return_type = self.validate_type(sig.return_type)?;
        let sig = RFunctionSignature {
            params,
            return_type,
            type_params: sig.type_params,
        };
        Ok(sig)
    }
    fn validate_member_function_signature(&self, sig: MemberFunctionSignature) -> Result<RMemberFunctionSignature, CortexError> {
        let mut params = Vec::new();
        for p in sig.params {
            params.push(RParameter {
                name: p.name,
                typ: self.validate_type(p.typ)?
            });
        }
        let return_type = self.validate_type(sig.return_type)?;
        let sig = RMemberFunctionSignature {
            params,
            return_type,
            type_params: sig.type_params,
            name: match sig.name {
                OptionalIdentifier::Ident(n) => n,
                OptionalIdentifier::Ignore => String::from("~"),
            },
            this_arg: sig.this_arg,
        };
        Ok(sig)
    }

    fn validate_follows_entries(&self, clause: Vec<FollowsEntry>) -> Result<Vec<RFollowsEntry>, CortexError> {
        let mut entries = Vec::new();
        for entry in clause {
            let result = self.get_contract_stub(&entry.name);
            if result.is_none() {
                return Err(Box::new(PreprocessingError::ContractDoesNotExist(entry.name.codegen(0))));
            }
            let (contract_type_params, path) = result.unwrap();
            let path_name = path.codegen(0);
            entries.push(RFollowsEntry {
                name: path,
                type_args: self.validate_type_args(contract_type_params, entry.type_args, path_name, "Contract")?,
            });
        }
        Ok(entries)
    }

    fn validate_member_function_signatures(&self, sigs: &Vec<MemberFunction>) -> Result<Vec<RMemberFunctionSignature>, CortexError> {
        let mut result = Vec::new();
        for item in sigs {
            result.push(self.validate_member_function_signature(item.signature.clone())?);
        }
        Ok(result)
    }
}
