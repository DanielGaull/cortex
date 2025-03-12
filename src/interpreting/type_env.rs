use std::collections::HashMap;

use crate::parsing::ast::r#type::CortexType;

pub struct TypeEnvironment {
    bindings: HashMap<String, CortexType>,
    parent: Option<Box<TypeEnvironment>>,
}

impl TypeEnvironment {
    pub fn new(parent: Box<TypeEnvironment>) -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
            parent: Some(parent),
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

    pub fn fill_in(&self, typ: CortexType) -> CortexType {
        match typ {
            CortexType::BasicType { nullable, name, type_args } => {
                if name.is_final() {
                    let ident = name.get_back().unwrap();
                    if let Some(result) = self.find_binding(ident) {
                        result.clone().to_nullable_value(nullable)
                    } else {
                        CortexType::BasicType { nullable: nullable, name: name, type_args: type_args }
                    }
                } else {
                    CortexType::BasicType { nullable: nullable, name: name, type_args: type_args }
                }
            },
            CortexType::RefType { contained, mutable } => {
                let new_contained = self.fill_in(*contained);
                CortexType::RefType { contained: Box::new(new_contained), mutable: mutable }
            },
        }
    }
}
