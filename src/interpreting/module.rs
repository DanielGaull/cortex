use std::{collections::{HashMap, VecDeque}, rc::Rc};

use thiserror::Error;

use crate::parsing::ast::{expression::{OptionalIdentifier, Parameter, PathError, PathIdent}, top_level::{Bundle, Function, Struct}, r#type::CortexType};

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

    #[error("Type \"{0}\" already exists")]
    TypeAlreadyExists(String),
    #[error("Type \"{0}\" does not exist")]
    TypeDoesNotExist(String),

    #[error("Struct \"{0}\" contains at least one field that references back to itself")]
    StructContainsCircularFields(String),
}

pub struct CompositeType {
    pub(crate) fields: HashMap<String, CortexType>,
    pub(crate) is_heap_allocated: bool,
}

pub struct Module {
    children: HashMap<String, Module>,
    functions: HashMap<String, Rc<Function>>,
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
        if path.is_final().map_err(|e| ModuleError::PathError(e))? {
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
    pub fn add_module(&mut self, path: &PathIdent, module: Module) -> Result<(), ModuleError> {
        if path.is_final().map_err(|e| ModuleError::PathError(e))? {
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

    fn get_function_internal(&self, name: &String) -> Option<Rc<Function>> {
        if self.functions.contains_key(name) {
            Some(self.functions.get(name).unwrap().clone())
        } else {
            None
        }
    }
    pub fn get_function(&self, name: &String) -> Result<Rc<Function>, ModuleError> {
        let search_result = self.get_function_internal(name);
        if let Some(func) = search_result {
            Ok(func)
        } else {
            Err(ModuleError::FunctionDoesNotExist(name.clone()))
        }
    }
    pub fn add_function(&mut self, func: Function) -> Result<(), ModuleError> {
        match &func.name {
            OptionalIdentifier::Ident(name) => {
                if let Some(_) = self.get_function_internal(&name) {
                    Err(ModuleError::FunctionAlreadyExists(name.clone()))
                } else {
                    self.functions.insert(name.clone(), Rc::from(func));
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
                        self.composites.insert(name.clone(), Rc::from(CompositeType {
                            fields: item.fields,
                            is_heap_allocated: false,
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
                    self.composites.insert(name.clone(), Rc::from(CompositeType {
                        fields: item.fields,
                        is_heap_allocated: true,
                    }));
                    for func in item.functions {
                        match func.name {
                            OptionalIdentifier::Ident(func_name) => {
                                let new_param = Parameter::named("this", CortexType::new(PathIdent::simple(name.clone()), false));
                                let mut param_list = vec![new_param];
                                param_list.extend(func.params);
                                let new_func = Function::new(
                                    OptionalIdentifier::Ident(Bundle::get_bundle_func_name(name, &func_name)),
                                    param_list,
                                    func.return_type,
                                    func.body,
                                );
                                self.add_function(new_func)?;
                            },
                            OptionalIdentifier::Ignore => (),
                        }
                    }
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }

    fn search_struct_for_loops(&self, s: &Struct) -> Result<bool, ModuleError> {
        match &s.name {
            OptionalIdentifier::Ident(name) => {
                let stype = CortexType::new(PathIdent::simple(name.clone()), false);
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
                        let struc = self
                            .get_module_for(typ.name())?
                            .get_composite(typ.name().get_back().map_err(|e| ModuleError::PathError(e))?)?;
                        
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
