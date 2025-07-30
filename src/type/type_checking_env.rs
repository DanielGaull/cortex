use std::collections::HashMap;

use crate::interpreting::env::EnvError;

pub struct TypeCheckingEnvironment<T> {
    vars: HashMap<String, Var<T>>,
    parent: Option<Box<TypeCheckingEnvironment<T>>>,
}

impl<T> TypeCheckingEnvironment<T> {
    pub fn new(parent: TypeCheckingEnvironment<T>) -> Self {
        TypeCheckingEnvironment {
            vars: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }
    pub fn base() -> Self {
        TypeCheckingEnvironment {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn add(&mut self, name: String, typ: T, is_const: bool) -> Result<(), EnvError> {
        if self.vars.contains_key(&name) {
            Err(EnvError::VariableAlreadyExists(name))
        } else {
            self.vars.insert(name, Var { typ, is_const });
            Ok(())
        }
    }

    pub fn get(&self, name: &String) -> Result<&T, EnvError> {
        if let Some(result) = self.vars.get(name) {
            Ok(&result.typ)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            Err(EnvError::VariableDoesNotExist(name.clone()))
        }
    }

    pub fn is_const(&self, name: &String) -> Result<bool, EnvError> {
        if let Some(result) = self.vars.get(name) {
            Ok(result.is_const)
        } else if let Some(parent) = &self.parent {
            parent.is_const(name)
        } else {
            Err(EnvError::VariableDoesNotExist(name.clone()))
        }
    }

    pub fn exit(self) -> Result<TypeCheckingEnvironment<T>, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }
}

struct Var<T> {
    typ: T,
    is_const: bool,
}
