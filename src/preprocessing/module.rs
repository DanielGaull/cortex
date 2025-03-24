use std::{collections::{HashMap, HashSet, VecDeque}, rc::Rc};

use thiserror::Error;

use crate::parsing::ast::{expression::{OptionalIdentifier, Parameter, PathError, PathIdent}, top_level::{Bundle, Function, Struct, ThisArg}, r#type::{forwarded_type_args, CortexType, TypeError}};

#[derive(Error, Debug, PartialEq)]
pub enum ModuleError {
    #[error("Module \"{0}\" already exists")]
    ModuleAlreadyExists(String),
    #[error("Module \"{0}\" was not found")]
    ModuleDoesNotExist(String),
    #[error("Path error: \"{0}\"")]
    PathError(PathError),

    #[error("Function \"{0}\" already exists")]
    FunctionAlreadyExists(String),
    #[error("Function \"{0}\" was not found")]
    FunctionDoesNotExist(String),
    #[error("Function \"{0}\" is in use")]
    FunctionInUse(String),

    #[error("Type \"{0}\" already exists")]
    TypeAlreadyExists(String),
    #[error("Type \"{0}\" does not exist")]
    TypeDoesNotExist(String),

    #[error("Struct \"{0}\" contains at least one field that references back to itself")]
    StructContainsCircularFields(String),

    #[error("Duplicate type argument name: {0}")]
    DuplicateTypeArgumentName(String),

    #[error("Type Error: {0}")]
    TypeError(TypeError),
}

pub struct CompositeType {
    pub(crate) fields: HashMap<String, CortexType>,
    pub(crate) type_param_names: Vec<String>,
    pub(crate) is_heap_allocated: bool,
}


pub struct Module {
    children: HashMap<String, Module>,
    functions: HashMap<String, Function>,
    composites: HashMap<String, Rc<CompositeType>>,
}

impl Module {
    pub fn new() -> Self {
        Self::with_children(HashMap::new())
    }
    pub fn with_children(children: HashMap<String, Module>) -> Self {
        Module {
            children: children,
            functions: HashMap::new(),
            composites: HashMap::new(),
        }
    }

    pub fn add_child(&mut self, name: String, module: Module) -> Result<(), ModuleError> {
        if self.children.contains_key(&name) {
            Err(ModuleError::ModuleAlreadyExists(name))
        } else {
            self.children.insert(name, module);
            Ok(())
        }
    }

    pub fn get_module_for(&self, path: &PathIdent) -> Result<&Module, ModuleError> {
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
    pub fn get_module_for_mut(&mut self, path: &PathIdent) -> Result<&mut Module, ModuleError> {
        if path.is_final() {
            return Ok(self);
        }
        let front = path.get_front().map_err(|e| ModuleError::PathError(e))?;
        if self.children.contains_key(front) {
            let child = self.children.get_mut(front).unwrap();
            let next_path = path.pop_front().map_err(|e| ModuleError::PathError(e))?;
            child.get_module_for_mut(&next_path)
        } else {
            Err(ModuleError::ModuleDoesNotExist(front.clone()))
        }
    }
    pub fn children_iter_mut(&mut self) -> impl Iterator<Item = (&String, &mut Module)> {
        self.children.iter_mut()
    }

    pub fn add_module(&mut self, path: &PathIdent, module: Module) -> Result<(), ModuleError> {
        if path.is_final() {
            let name = path.get_front().map_err(|e| ModuleError::PathError(e))?.clone();
            self.add_child(name, module)?;
            return Ok(());
        }
        let front = path.get_front().map_err(|e| ModuleError::PathError(e))?;
        if self.children.contains_key(front) {
            let child = self.children.get_mut(front).unwrap();
            let next_path = path.pop_front().map_err(|e| ModuleError::PathError(e))?;
            child.add_module(&next_path, module)
        } else {
            Err(ModuleError::ModuleDoesNotExist(front.clone()))
        }
    }

    pub fn take_functions(&mut self) -> Result<Vec<Function>, ModuleError> {
        let res = std::mem::take(&mut self.functions).into_values().collect();
        Ok(res)
    }
    pub fn add_function(&mut self, func: Function) -> Result<(), ModuleError> {
        match &func.name {
            OptionalIdentifier::Ident(name) => {
                if self.functions.contains_key(name) {
                    Err(ModuleError::FunctionAlreadyExists(name.clone()))
                } else {
                    let mut seen_type_param_names = HashSet::new();
                    for t in &func.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
                        }
                        seen_type_param_names.insert(t);
                    }

                    self.functions.insert(name.clone(), func);
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }

    fn get_composite_internal(&self, name: &String) -> Option<Rc<CompositeType>> {
        if self.composites.contains_key(name) {
            Some(self.composites.get(name).unwrap().clone())
        } else {
            None
        }
    }
    pub fn get_composite(&self, name: &String) -> Result<Rc<CompositeType>, ModuleError> {
        let search_result = self.get_composite_internal(name);
        if let Some(func) = search_result {
            Ok(func)
        } else {
            Err(ModuleError::TypeDoesNotExist(name.clone()))
        }
    }
    pub fn add_struct(&mut self, item: Struct) -> Result<(), ModuleError> {
        match &item.name {
            OptionalIdentifier::Ident(name) => {
                if let Some(_) = self.get_composite_internal(&name) {
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

                        self.composites.insert(name.clone(), Rc::from(CompositeType {
                            fields: item.fields,
                            is_heap_allocated: false,
                            type_param_names: item.type_param_names,
                        }));
                        Ok(())
                    }
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }
    pub fn add_bundle(&mut self, item: Bundle) -> Result<(), ModuleError> {
        match &item.name {
            OptionalIdentifier::Ident(name) => {
                if let Some(_) = self.get_composite_internal(&name) {
                    Err(ModuleError::TypeAlreadyExists(name.clone()))
                } else {
                    for func in item.functions {
                        match func.name {
                            OptionalIdentifier::Ident(func_name) => {
                                let new_param = Parameter::named(
                                    "this", 
                                    CortexType::reference(
                                        CortexType::basic(PathIdent::simple(name.clone()), false, forwarded_type_args(&item.type_param_names)),
                                        func.this_arg == ThisArg::MutThis
                                    ));
                                let mut param_list = vec![new_param];
                                param_list.extend(func.params);
                                let mut type_param_names = func.type_param_names;
                                type_param_names.extend(item.type_param_names.clone());
                                let new_func = Function::new(
                                    OptionalIdentifier::Ident(Bundle::get_bundle_func_name(name, &func_name)),
                                    param_list,
                                    func.return_type,
                                    func.body,
                                    type_param_names,
                                );
                                self.add_function(new_func)?;
                            },
                            OptionalIdentifier::Ignore => (),
                        }
                    }

                    let mut seen_type_param_names = HashSet::new();
                    for t in &item.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
                        }
                        seen_type_param_names.insert(t);
                    }

                    self.composites.insert(name.clone(), Rc::from(CompositeType {
                        fields: item.fields,
                        is_heap_allocated: true,
                        type_param_names: item.type_param_names,
                    }));
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
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
                        let struc = self
                            .get_module_for(typ_name)?
                            .get_composite(typ_name.get_back().map_err(|e| ModuleError::PathError(e))?)?;
                        
                        for field in &struc.fields {
                            q.push_back(field.1.clone());
                        }
                    }
                }
                Ok(false)
            },
            OptionalIdentifier::Ignore => Ok(false),
        }
    }
}
