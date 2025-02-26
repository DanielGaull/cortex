use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::expression::PathIdent;

#[derive(Clone, Debug)]
pub struct CortexType {
    pub(crate) name: PathIdent,
    pub(crate) nullable: bool,
    pub(crate) is_any: bool,
}

impl SimpleCodeGen for CortexType {
    fn codegen(&self, _: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.name.codegen(0));
        if self.nullable {
            s.push_str("?");
        }
        s
    }
}

impl CortexType {
    pub fn name(&self) -> &PathIdent {
        &self.name
    }
    pub fn nullable(&self) -> bool {
        self.nullable
    }

    pub fn new(name: PathIdent, nullable: bool) -> Self {
        CortexType {
            name: name,
            nullable: nullable,
            is_any: false,
        }
    }
    pub fn simple(name: &str, nullable: bool) -> Self {
        Self::new(PathIdent::simple(String::from(name)), nullable)
    }
    pub fn number(nullable: bool) -> Self {
        Self::simple("number", nullable)
    }
    pub fn boolean(nullable: bool) -> Self {
        Self::simple("bool", nullable)
    }
    pub fn string(nullable: bool) -> Self {
        Self::simple("string", nullable)
    }
    pub fn void(nullable: bool) -> Self {
        Self::simple("void", nullable)
    }
    pub fn null() -> Self {
        Self::simple("null", true)
    }
    pub fn any(nullable: bool) -> Self {
        CortexType {
            name: PathIdent::simple(String::from("any")),
            nullable: nullable,
            is_any: true,
        }
    }
    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        CortexType {
            name: PathIdent::concat(path, &self.name),
            nullable: self.nullable,
            is_any: self.is_any,
        }
    }
    pub fn with_prefix_if_not_core(self, prefix: &PathIdent) -> Self {
        if !self.is_core() {
            self.with_prefix(prefix)
        } else {
            self
        }
    }

    pub fn is_core(&self) -> bool {
        self.name.is_final().unwrap() && 
            matches!(self.name.get_back().unwrap().as_str(), "number" | "bool" | "string" | "void" | "null" | "any")
    }

    pub fn to_nullable(self) -> Self {
        CortexType {
            name: self.name,
            nullable: true,
            is_any: self.is_any,
        }
    }

    pub fn to_non_nullable(self) -> Self {
        CortexType {
            name: self.name,
            nullable: false,
            is_any: self.is_any,
        }
    }

    pub fn combine_with(self, other: CortexType) -> Option<CortexType> {
        if self.is_any || other.is_any {
            Some(Self::any(self.nullable || other.nullable))
        } else {
            if self.name == other.name {
                Some(Self::new(self.name, self.nullable || other.nullable))
            } else if self == CortexType::null() {
                Some(Self::new(other.name, true))
            } else if other == CortexType::null() {
                Some(Self::new(self.name, true))
            } else {
                None
            }
        }
    }

    pub fn is_subtype_of(&self, other: &CortexType) -> bool {
        if other.is_any {
            return true;
        }
        if other.nullable && self == &CortexType::null() {
            return true;
        }
        if &self.name == &other.name {
            if self.nullable && !other.nullable {
                return false;
            }
            return true;
        }
        false
    }
}

impl PartialEq for CortexType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.nullable == other.nullable && self.is_any == other.is_any
    }
}
