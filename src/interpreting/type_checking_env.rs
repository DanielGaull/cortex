use std::collections::HashMap;

use crate::parsing::ast::r#type::CortexType;

use super::env::EnvError;


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

    pub fn get(&self, name: &String) -> Option<&CortexType> {
        if let Some(result) = self.vars.get(name) {
            Some(&result.typ)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn is_const(&self, name: &String) -> Option<bool> {
        if let Some(result) = self.vars.get(name) {
            Some(result.is_const)
        } else if let Some(parent) = &self.parent {
            parent.is_const(name)
        } else {
            None
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
