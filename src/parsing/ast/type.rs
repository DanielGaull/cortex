use crate::parsing::codegen::r#trait::SimpleCodeGen;

#[derive(Clone, Debug)]
pub struct CortexType {
    pub(crate) name: String,
    pub(crate) nullable: bool,
    pub(crate) is_any: bool,
}

impl SimpleCodeGen for CortexType {
    fn codegen(&self, _: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.name);
        if self.nullable {
            s.push_str("?");
        }
        s
    }
}

impl CortexType {
    pub fn new(name: &str, nullable: bool) -> Self {
        CortexType {
            name: String::from(name),
            nullable: nullable,
            is_any: false,
        }
    }
    pub fn number(nullable: bool) -> Self {
        Self::new("number", nullable)
    }
    pub fn boolean(nullable: bool) -> Self {
        Self::new("bool", nullable)
    }
    pub fn string(nullable: bool) -> Self {
        Self::new("string", nullable)
    }
    pub fn void(nullable: bool) -> Self {
        Self::new("void", nullable)
    }
    pub fn null() -> Self {
        Self::new("null", true)
    }
    pub fn any(nullable: bool) -> Self {
        CortexType {
            name: String::from("any"),
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
