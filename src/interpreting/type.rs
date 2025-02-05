#[derive(Clone)]
pub struct CortexType {
    name: String,
    nullable: bool,
    is_any: bool,
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
        if &self.name == &other.name {
            if self.nullable && !other.nullable {
                return false;
            }
            return true;
        }
        false
    }
}
