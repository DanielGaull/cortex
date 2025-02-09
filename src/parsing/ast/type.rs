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

    pub fn is_subtype_of(&self, other: &CortexType) -> bool {
        if other.is_any {
            return true;
        }
        if self.nullable && other == &CortexType::null() {
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
