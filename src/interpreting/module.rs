use std::collections::HashMap;

use thiserror::Error;

use crate::parsing::ast::expression::{PathError, PathIdent};

use super::env::Environment;

pub struct Module {
    env: Environment,
    children: HashMap<String, Module>,
}

#[derive(Error, Debug)]
pub enum ModuleError {
    #[error("Module \"{0}\" already exists")]
    ModuleAlreadyExists(String),
    #[error("Module \"{0}\" was not found")]
    ModuleDoesNotExist(String),
    #[error("Path error: \"{0}\"")]
    PathError(PathError),
}

impl Module {
    pub fn new(env: Environment) -> Self {
        Module {
            env: env,
            children: HashMap::new(),
        }
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn add_child(&mut self, name: String, module: Module) -> Result<(), ModuleError> {
        if self.children.contains_key(&name) {
            Err(ModuleError::ModuleAlreadyExists(name))
        } else {
            self.children.insert(name, module);
            Ok(())
        }
    }

    pub fn get_module(&self, path: &PathIdent) -> Result<&Module, ModuleError> {
        if path.is_final().map_err(|e| ModuleError::PathError(e))? {
            return Ok(self);
        }
        let front = path.get_front().map_err(|e| ModuleError::PathError(e))?;
        if self.children.contains_key(front) {
            let child = self.children.get(front).unwrap();
            let next_path = path.pop_front().map_err(|e| ModuleError::PathError(e))?;
            child.get_module(&next_path)
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
}
