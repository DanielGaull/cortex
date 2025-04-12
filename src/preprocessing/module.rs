use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::parsing::ast::{expression::{OptionalIdentifier, PathError, PathIdent}, top_level::{Bundle, Extension, PFunction, Struct}, r#type::{CortexType, TypeError}};

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

pub struct TypeDefinition {
    pub(crate) fields: HashMap<String, CortexType>,
    pub(crate) type_param_names: Vec<String>,
    pub(crate) is_heap_allocated: bool,
}


pub struct Module {
    children: HashMap<String, Module>,
    functions: HashMap<String, PFunction>,
    structs: HashMap<String, Struct>,
    bundles: HashMap<String, Bundle>,
    extensions: Vec<Extension>,
}

impl Module {
    pub fn new() -> Self {
        Self::with_children(HashMap::new())
    }
    pub fn with_children(children: HashMap<String, Module>) -> Self {
        Module {
            children: children,
            functions: HashMap::new(),
            structs: HashMap::new(),
            bundles: HashMap::new(),
            extensions: Vec::new(),
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
    pub fn children_iter(&mut self) -> impl Iterator<Item = (String, Module)> {
        let children = std::mem::take(&mut self.children);
        children.into_iter()
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

    pub fn take_functions(&mut self) -> Result<Vec<PFunction>, ModuleError> {
        let res = std::mem::take(&mut self.functions).into_values().collect();
        Ok(res)
    }
    pub fn add_function(&mut self, func: PFunction) -> Result<(), ModuleError> {
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

    pub fn take_structs(&mut self) -> Result<Vec<Struct>, ModuleError> {
        let res = std::mem::take(&mut self.structs).into_values().collect();
        Ok(res)
    }
    pub fn add_struct(&mut self, item: Struct) -> Result<(), ModuleError> {
        match &item.name {
            OptionalIdentifier::Ident(name) => {
                if self.structs.contains_key(name) {
                    Err(ModuleError::TypeAlreadyExists(name.clone()))
                } else {
                    let mut seen_type_param_names = HashSet::new();
                    for t in &item.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
                        }
                        seen_type_param_names.insert(t);
                    }

                    self.structs.insert(name.clone(), item);
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }

    pub fn take_bundles(&mut self) -> Result<Vec<Bundle>, ModuleError> {
        let res = std::mem::take(&mut self.bundles).into_values().collect();
        Ok(res)
    }
    pub fn add_bundle(&mut self, item: Bundle) -> Result<(), ModuleError> {
        match &item.name {
            OptionalIdentifier::Ident(name) => {
                if self.bundles.contains_key(name) {
                    Err(ModuleError::TypeAlreadyExists(name.clone()))
                } else {
                    let mut seen_type_param_names = HashSet::new();
                    for t in &item.type_param_names {
                        if seen_type_param_names.contains(t) {
                            return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
                        }
                        seen_type_param_names.insert(t);
                    }

                    self.bundles.insert(name.clone(), item);
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }

    pub fn take_extensions(&mut self) -> Result<Vec<Extension>, ModuleError> {
        let res = std::mem::take(&mut self.extensions);
        Ok(res)
    }
    pub fn add_extension(&mut self, item: Extension) -> Result<(), ModuleError> {
        let mut seen_type_param_names = HashSet::new();
        for t in &item.type_param_names {
            if seen_type_param_names.contains(t) {
                return Err(ModuleError::DuplicateTypeArgumentName(t.clone()));
            }
            seen_type_param_names.insert(t);
        }

        self.extensions.push(item);
        Ok(())
    }
}
