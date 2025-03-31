use std::collections::{HashMap, HashSet, VecDeque};

use crate::{parsing::ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Bundle, Function, FunctionSignature, Struct, ThisArg}, r#type::{forwarded_type_args, CortexType}}, preprocessing::module::{CompositeType, ModuleError}};

pub struct RModule {
    signatures: HashMap<String, FunctionSignature>,
    composites: HashMap<String, CompositeType>,
    children: HashMap<String, RModule>,
}

impl RModule {
    pub fn new() -> Self {
        RModule {
            signatures: HashMap::new(),
            composites: HashMap::new(),
            children: HashMap::new(),
        }
    }

    pub(crate) fn add_child(&mut self, name: String, other: RModule) {
        self.children.insert(name, other);
    }

    pub(crate) fn add_signature(&mut self, name: String, sig: FunctionSignature) -> Result<(), ModuleError> {
        if !self.signatures.contains_key(&name) {
            self.signatures.insert(name, sig);
            Ok(())
        } else {
            Err(ModuleError::FunctionAlreadyExists(name))
        }
    }

    pub(crate) fn add_struct(&mut self, item: Struct) -> Result<(), ModuleError> {
        match &item.name {
            OptionalIdentifier::Ident(name) => {
                if self.composites.contains_key(name) {
                    Err(ModuleError::TypeAlreadyExists(name.clone()))
                } else {
                    let has_loop = self.search_struct_for_loops(&item)?;
                    if has_loop {
                        Err(ModuleError::StructContainsCircularFields(name.clone()))
                    } else {
                        let mut seen_type_param_names = HashSet::new();
                        for t in &item.type_param_names {
                            if seen_type_param_names.contains(t) {
                                return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
                            }
                            seen_type_param_names.insert(t);
                        }

                        self.composites.insert(name.clone(), CompositeType {
                            fields: item.fields,
                            is_heap_allocated: false,
                            type_param_names: item.type_param_names,
                        });
                        Ok(())
                    }
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }

    pub(crate) fn add_bundle(&mut self, item: Bundle) -> Result<Vec<Function>, ModuleError> {
        match &item.name {
            OptionalIdentifier::Ident(item_name) => {
                if self.composites.contains_key(item_name) {
                    Err(ModuleError::TypeAlreadyExists(item_name.clone()))
                } else {
                    let mut seen_type_param_names = HashSet::new();
                    for t in &item.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
                        }
                        seen_type_param_names.insert(t);
                    }

                    let mut functions = Vec::new();

                    for func in item.functions {
                        match func.name {
                            OptionalIdentifier::Ident(func_name) => {
                                let new_param = Parameter::named(
                                    "this", 
                                    CortexType::reference(
                                        CortexType::basic(PathIdent::simple(item_name.clone()), false, forwarded_type_args(&item.type_param_names)),
                                        func.this_arg == ThisArg::MutThis
                                    ));
                                let mut param_list = vec![new_param];
                                param_list.extend(func.params);
                                let mut type_param_names = func.type_param_names;
                                type_param_names.extend(item.type_param_names.clone());
                                let new_func = Function::new(
                                    OptionalIdentifier::Ident(Bundle::get_bundle_func_name(item_name, &func_name)),
                                    param_list,
                                    func.return_type,
                                    func.body,
                                    type_param_names,
                                );
                                functions.push(new_func);
                            },
                            OptionalIdentifier::Ignore => (),
                        }
                    }

                    self.composites.insert(item_name.clone(), CompositeType {
                        fields: item.fields,
                        is_heap_allocated: false,
                        type_param_names: item.type_param_names,
                    });

                    Ok(functions)
                }
            },
            OptionalIdentifier::Ignore => Ok(vec![]),
        }
    }

    pub fn get_module_for(&self, path: &PathIdent) -> Result<&RModule, ModuleError> {
        if path.is_final() {
            return Ok(self);
        }
        let front = path.get_front().map_err(|e| ModuleError::PathError(e))?;
        if self.children.contains_key(front) {
            let child = self.children.get(front).unwrap();
            let next_path = path.pop_front().map_err(|e| ModuleError::PathError(e))?;
            child.get_module_for(&next_path)
        } else {
            Err(ModuleError::ModuleDoesNotExist(front.clone()))
        }
    }

    fn has_composite(&self, name: &PathIdent) -> bool {
        let module = self.get_module_for(name);
        if let Ok(m) = module {
            m.composites.contains_key(name.get_back().unwrap())
        } else {
            false
        }
    }
    pub(crate) fn lookup_composite(&self, name: &String) -> Result<&CompositeType, ModuleError> {
        if let Some(c) = self.composites.get(name) {
            Ok(c)
        } else {
            Err(ModuleError::TypeDoesNotExist(name.clone()))
        }
    }

    fn search_struct_for_loops(&self, s: &Struct) -> Result<bool, ModuleError> {
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
                        let typ_name = typ.name().map_err(|e| ModuleError::TypeError(e))?;

                        // It's ok if the struct doesn't exist yet
                        // If it has loops, then they will be caught when we visit this function upon registering it
                        // Unfortunately, the order in which structs are added is not deterministic
                        if self.has_composite(typ_name) {
                            let module = self.get_module_for(typ_name)?;
                            let struc = module.lookup_composite(typ_name.get_back().map_err(|e| ModuleError::PathError(e))?)?;
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
}
