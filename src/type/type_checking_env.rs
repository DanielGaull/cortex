use std::collections::HashMap;

use crate::interpreting::env::EnvError;

use super::r#type::CortexType;

pub struct TypeCheckingEnvironment {
    vars: HashMap<String, Var>,
    parent: Option<Box<TypeCheckingEnvironment>>,
}

impl TypeCheckingEnvironment {
    pub fn new(parent: TypeCheckingEnvironment) -> Self {
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

    pub fn add(&mut self, name: String, typ: CortexType, is_const: bool) -> Result<(), EnvError> {
        if self.vars.contains_key(&name) {
            Err(EnvError::VariableAlreadyExists(name))
        } else {
            self.vars.insert(name, Var { typ, is_const });
            Ok(())
        }
    }

    pub fn get(&self, name: &String) -> Result<&CortexType, EnvError> {
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

    pub fn exit(self) -> Result<TypeCheckingEnvironment, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }
}

struct Var {
    typ: CortexType,
    is_const: bool,
}
