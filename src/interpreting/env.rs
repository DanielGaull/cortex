use std::{cell::RefCell, collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::parsing::ast::r#type::CortexType;

use super::value::CortexValue;

#[derive(Error, Debug, PartialEq)]
pub enum EnvError {
    #[error("Cannot modify: Value \"{0}\" is constant")]
    ModifyConstant(String),
    #[error("Variable \"{0}\" already exists")]
    VariableAlreadyExists(String),
    #[error("Variable \"{0}\" was not found")]
    VariableDoesNotExist(String),

    #[error("Cannot return from a base environment")]
    AlreadyBase,
}

pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Variable>,
}
impl Environment {
    fn full_new(parent: Option<Box<Environment>>) -> Self {
        Environment {
            parent: parent,
            variables: HashMap::new(),
        }
    }
    pub fn new(parent: Environment) -> Self {
        Self::full_new(Some(Box::new(parent)))
    }
    pub fn base() -> Self {
        Self::full_new(None)
    }

    pub fn foreach<F>(&self, mut func: F)
    where F: FnMut(&str, &CortexValue) -> () {
        for (name, var) in &self.variables {
            func(name.as_str(), &var.value().borrow().clone());
        }
        if let Some(p) = &self.parent {
            p.foreach(func);
        }
    }

    pub fn exit(self) -> Result<Environment, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }

    fn get_variable(&self, name: &String) -> Option<&Variable> {
        if self.variables.contains_key(name) {
            Some(self.variables.get(name).unwrap())
        } else {
            self.parent.as_ref().and_then(|env| env.get_variable(name))
        }
    }
    fn get_variable_mut(&mut self, name: &String) -> Option<&mut Variable> {
        if self.variables.contains_key(name) {
            Some(self.variables.get_mut(name).unwrap())
        } else {
            self.parent.as_mut().and_then(|env| env.get_variable_mut(name))
        }
    }

    pub fn add_var(&mut self, name: String, typ: CortexType, value: CortexValue) -> Result<(), EnvError> {
        // Check that value doesn't exist yet
        if let Some(_) = self.variables.get(&name) {
            Err(EnvError::VariableAlreadyExists(name.clone()))
        } else {
            let var = Variable::var(name.clone(), typ, value);
            self.variables.insert(name, var);
            Ok(())
        }
    }
    pub fn add_const(&mut self, name: String, typ: CortexType, value: CortexValue) -> Result<(), EnvError> {
        // Check that value doesn't exist yet
        if let Some(_) = self.get_variable(&name) {
            Err(EnvError::VariableAlreadyExists(name.clone()))
        } else {
            let var = Variable::constant(name.clone(), typ, value);
            self.variables.insert(name, var);
            Ok(())
        }
    }

    pub fn get_type_of(&self, name: &String) -> Result<&CortexType, EnvError> {
        let search_result = self.get_variable(name);
        if let Some(var) = search_result {
            Ok(var.typ())
        } else {
            Err(EnvError::VariableDoesNotExist(name.clone()))
        }
    }

    pub fn get_value(&self, name: &str) -> Result<CortexValue, EnvError> {
        let search_result = self.get_variable(&String::from(name));
        if let Some(var) = search_result {
            Ok(var.value().borrow().clone())
        } else {
            Err(EnvError::VariableDoesNotExist(String::from(name)))
        }
    }
    pub fn get_cell(&self, name: &str) -> Result<Rc<RefCell<CortexValue>>, EnvError> {
        let search_result = self.get_variable(&String::from(name));
        if let Some(var) = search_result {
            Ok(var.value().clone())
        } else {
            Err(EnvError::VariableDoesNotExist(String::from(name)))
        }
    }
    pub fn set_value(&mut self, name: &str, value: CortexValue) -> Result<(), EnvError> {
        let search_result = self.get_variable_mut(&String::from(name));
        if let Some(var) = search_result {
            var.set(value)
        } else {
            Err(EnvError::VariableDoesNotExist(String::from(name)))
        }
    }
}

struct Variable {
    is_const: bool,
    declared_type: CortexType,
    value: Rc<RefCell<CortexValue>>,
    name: String,
}
impl Variable {
    fn var(name: String, typ: CortexType, value: CortexValue) -> Self {
        Variable {
            name: name,
            is_const: false,
            declared_type: typ,
            value: Rc::new(RefCell::new(value)),
        }
    }
    fn constant(name: String, typ: CortexType, value: CortexValue) -> Self {
        Variable {
            name: name,
            is_const: true,
            declared_type: typ,
            value: Rc::new(RefCell::new(value)),
        }
    }

    fn value(&self) -> &Rc<RefCell<CortexValue>> {
        &self.value
    }
    fn typ(&self) -> &CortexType {
        &self.declared_type
    }
    fn set(&mut self, value: CortexValue) -> Result<(), EnvError> {
        // Does not check the type, that is the interpreter's job before
        // calling this function
        if self.is_const {
            Err(EnvError::ModifyConstant(self.name.clone()))
        } else {
            self.value = Rc::new(RefCell::new(value));
            Ok(())
        }
    }
}
