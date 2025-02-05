use std::collections::HashMap;

use thiserror::Error;

use crate::parsing::ast::{expression::OptionalIdentifier, top_level::Function};

use super::{r#type::CortexType, value::CortexValue};

#[derive(Error, Debug)]
pub enum EnvError {
    #[error("Cannot modify: Value \"{0}\" is constant")]
    ModifyConstant(String),
    #[error("Variable \"{0}\" already exists")]
    VariableAlreadyExists(String),
    #[error("Variable \"{0}\" was not found")]
    VariableDoesNotExist(String),

    #[error("Function \"{0}\" already exists")]
    FunctionAlreadyExists(String),
    #[error("Function \"{0}\" was not found")]
    FunctionDoesNotExist(String),
}

pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Variable>,
    functions: HashMap<String, Function>,
}
impl Environment {
    pub fn new(parent: Environment) -> Self {
        Environment {
            parent: Some(Box::new(parent)),
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
    pub fn base() -> Self {
        Environment {
            parent: None,
            variables: HashMap::new(),
            functions: HashMap::new(),
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
        if let Some(_) = self.get_variable(&name) {
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

    pub fn get_value(&self, name: &String) -> Result<&CortexValue, EnvError> {
        let search_result = self.get_variable(name);
        if let Some(var) = search_result {
            Ok(var.value())
        } else {
            Err(EnvError::VariableDoesNotExist(name.clone()))
        }
    }
    pub fn set_value(&mut self, name: &String, value: CortexValue) -> Result<(), EnvError> {
        let search_result = self.get_variable_mut(name);
        if let Some(var) = search_result {
            var.set(value)
        } else {
            Err(EnvError::VariableDoesNotExist(name.clone()))
        }
    }

    fn get_function_internal(&self, name: &String) -> Option<&Function> {
        if self.functions.contains_key(name) {
            Some(self.functions.get(name).unwrap())
        } else {
            self.parent.as_ref().and_then(|env| env.get_function_internal(name))
        }
    }
    pub fn get_function(&self, name: &String) -> Result<&Function, EnvError> {
        let search_result = self.get_function_internal(name);
        if let Some(func) = search_result {
            Ok(func)
        } else {
            Err(EnvError::FunctionDoesNotExist(name.clone()))
        }
    }
    pub fn add_function(&mut self, func: Function) -> Result<(), EnvError> {
        // If function doesn't have an identifier, then it doesn't get added
        match &func.name {
            OptionalIdentifier::Ident(name) => {
                // Check that function doesn't exist yet
                if let Some(_) = self.get_function_internal(&name) {
                    Err(EnvError::FunctionAlreadyExists(name.clone()))
                } else {
                    self.functions.insert(name.clone(), func);
                    Ok(())
                }
            },
            OptionalIdentifier::Ignore => Ok(()),
        }
    }
}

pub struct Variable {
    is_const: bool,
    declared_type: CortexType,
    value: CortexValue,
    name: String,
}
impl Variable {
    pub fn var(name: String, typ: CortexType, value: CortexValue) -> Self {
        Variable {
            name: name,
            is_const: false,
            declared_type: typ,
            value: value,
        }
    }
    pub fn constant(name: String, typ: CortexType, value: CortexValue) -> Self {
        Variable {
            name: name,
            is_const: true,
            declared_type: typ,
            value: value,
        }
    }

    pub fn value(&self) -> &CortexValue {
        &self.value
    }
    pub fn typ(&self) -> &CortexType {
        &self.declared_type
    }
    pub fn name(&self) -> &String {
        &self.name
    }
    pub fn set(&mut self, value: CortexValue) -> Result<(), EnvError> {
        // Does not check the type, that is the interpreter's job before
        // calling this function
        if self.is_const {
            Err(EnvError::ModifyConstant(self.name.clone()))
        } else {
            self.value = value;
            Ok(())
        }
    }
}
