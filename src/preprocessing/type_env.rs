use std::collections::HashMap;

use crate::{interpreting::env::EnvError, parsing::ast::r#type::CortexType};

pub struct TypeEnvironment {
    bindings: HashMap<String, CortexType>,
    parent: Option<Box<TypeEnvironment>>,
}

impl TypeEnvironment {
    pub fn new(parent: TypeEnvironment) -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }
    pub fn base() -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn add(&mut self, name: String, typ: CortexType) {
        self.bindings.insert(name, typ);
    }

    pub fn get(&self, name: &String) -> Option<&CortexType> {
        if let Some(result) = self.bindings.get(name) {
            Some(result)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn fill_in(&self, typ: CortexType) -> CortexType {
        match typ {
            CortexType::BasicType { optional, name, type_args } => {
                if name.is_final() {
                    let ident = name.get_back().unwrap();
                    if let Some(result) = self.get(ident) {
                        result.clone().to_optional_if_true(optional)
                    } else {
                        CortexType::BasicType { optional, name: name, type_args: type_args.into_iter().map(|t| self.fill_in(t)).collect() }
                    }
                } else {
                    CortexType::BasicType { optional, name: name, type_args: type_args.into_iter().map(|t| self.fill_in(t)).collect() }
                }
            },
            CortexType::RefType { contained, mutable } => {
                let new_contained = self.fill_in(*contained);
                CortexType::RefType { contained: Box::new(new_contained), mutable: mutable }
            },
            CortexType::Unknown(b) => CortexType::Unknown(b),
        }
    }

    pub fn exit(self) -> Result<TypeEnvironment, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }

    pub fn does_arg_list_contain<'a>(type_param_names: &Vec<String>, typ: &'a CortexType) -> Option<&'a String> {
        let typ_name = typ.name().ok()?;
        if typ_name.is_final() {
            let name = typ_name.get_back().unwrap();
            if type_param_names.contains(name) {
                Some(name)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn fill(typ: CortexType, bindings: &HashMap<String, CortexType>) -> CortexType {
        match typ {
            CortexType::BasicType { optional, name, type_args } => {
                if name.is_final() {
                    let ident = name.get_back().unwrap();
                    if let Some(result) = bindings.get(ident) {
                        result.clone().to_optional_if_true(optional)
                    } else {
                        CortexType::BasicType { optional, name: name, type_args: type_args.into_iter().map(|t| Self::fill(t, bindings)).collect() }
                    }
                } else {
                    CortexType::BasicType { optional, name: name, type_args: type_args.into_iter().map(|t| Self::fill(t, bindings)).collect() }
                }
            },
            CortexType::RefType { contained, mutable } => {
                let new_contained = Self::fill(*contained, bindings);
                CortexType::RefType { contained: Box::new(new_contained), mutable: mutable }
            },
            CortexType::Unknown(b) => CortexType::Unknown(b),
        }
    }

    pub fn create_bindings(names: &Vec<String>, types: &Vec<CortexType>) -> HashMap<String, CortexType> {
        names.clone().into_iter().zip(types.clone()).collect()
    }
}
