use std::collections::HashMap;

use crate::parsing::ast::r#type::CortexType;

use super::env::EnvError;

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

    fn find_binding(&self, name: &String) -> Option<&CortexType> {
        if let Some(result) = self.bindings.get(name) {
            Some(result)
        } else if let Some(parent) = &self.parent {
            parent.find_binding(name)
        } else {
            None
        }
    }

    pub fn contains_binding(&self, typ: &CortexType) -> bool {
        match typ {
            CortexType::BasicType { nullable: _, name, type_args } => {
                if name.is_final() {
                    let ident = name.get_back().unwrap();
                    if let Some(_) = self.find_binding(ident) {
                        true
                    } else {
                        type_args.iter().any(|t| self.contains_binding(t))
                    }
                } else {
                    type_args.iter().any(|t| self.contains_binding(t))
                }
            },
            CortexType::RefType { contained, mutable: _ } => {
                self.contains_binding(contained)
            },
            CortexType::Unknown(_) => false,
        }
    }

    pub fn fill_in(&self, typ: CortexType) -> CortexType {
        match typ {
            CortexType::BasicType { nullable, name, type_args } => {
                if name.is_final() {
                    let ident = name.get_back().unwrap();
                    if let Some(result) = self.find_binding(ident) {
                        result.clone().to_nullable_if_true(nullable)
                    } else {
                        CortexType::BasicType { nullable: nullable, name: name, type_args: type_args.into_iter().map(|t| self.fill_in(t)).collect() }
                    }
                } else {
                    CortexType::BasicType { nullable: nullable, name: name, type_args: type_args.into_iter().map(|t| self.fill_in(t)).collect() }
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
            CortexType::BasicType { nullable, name, type_args } => {
                if name.is_final() {
                    let ident = name.get_back().unwrap();
                    if let Some(result) = bindings.get(ident) {
                        result.clone().to_nullable_if_true(nullable)
                    } else {
                        CortexType::BasicType { nullable: nullable, name: name, type_args: type_args.into_iter().map(|t| Self::fill(t, bindings)).collect() }
                    }
                } else {
                    CortexType::BasicType { nullable: nullable, name: name, type_args: type_args.into_iter().map(|t| Self::fill(t, bindings)).collect() }
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
