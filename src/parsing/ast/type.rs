use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::expression::PathIdent;

#[derive(Clone, Debug, PartialEq)]
pub enum CortexType {
    BasicType {
        nullable: bool,
        name: PathIdent,
    },
    PointerType {
        nullable: bool,
        name: PathIdent,
        mutable: bool,
    },
}

impl SimpleCodeGen for CortexType {
    fn codegen(&self, _: usize) -> String {
        match self {
            CortexType::BasicType { nullable, name } => {
                let mut s = String::new();
                s.push_str(&name.codegen(0));
                if *nullable {
                    s.push_str("?");
                }
                s
            },
            CortexType::PointerType { nullable, name, mutable } => {
                let mut s = String::new();
                if *mutable {
                    s.push_str("mut ");
                }
                s.push_str(&
                    name.codegen(0));
                if *nullable {
                    s.push_str("?");
                }
                s
            },
        }
    }
}

impl CortexType {
    pub fn new(name: PathIdent, nullable: bool) -> Self {
        Self::BasicType {
            name: name,
            nullable: nullable,
        }
    }
    pub fn pointer(name: PathIdent, nullable: bool, mutable: bool) -> Self {
        Self::PointerType {
            name: name,
            nullable: nullable,
            mutable: mutable,
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
    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        match self {
            CortexType::BasicType { nullable, name } => {
                CortexType::BasicType {
                    name: PathIdent::concat(path, name),
                    nullable: *nullable,
                }
            },
            CortexType::PointerType { nullable, name, mutable } => {
                CortexType::PointerType {
                    name: PathIdent::concat(path, name),
                    nullable: *nullable,
                    mutable: *mutable,
                }
            },
        }
    }
    pub fn with_prefix_if_not_core(self, prefix: &PathIdent) -> Self {
        if !self.is_core() {
            self.with_prefix(prefix)
        } else {
            self
        }
    }

    pub fn prefix(&self) -> PathIdent {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                name.without_last()
            },
            CortexType::PointerType { nullable: _, name, mutable: _ } => {
                name.without_last()
            },
        }
    }
    pub fn nullable(&self) -> bool {
        match self {
            CortexType::BasicType { nullable, name: _ } => {
                *nullable
            },
            CortexType::PointerType { nullable, name: _, mutable: _ } => {
                *nullable
            },
        }
    }

    pub fn is_core(&self) -> bool {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                name.is_final().unwrap() && 
                    matches!(name.get_back().unwrap().as_str(), "number" | "bool" | "string" | "void" | "null" | "any")
            },
            CortexType::PointerType { nullable: _, name, mutable: _ } => {
                name.is_final().unwrap() && 
                    matches!(name.get_back().unwrap().as_str(), "number" | "bool" | "string" | "void" | "null" | "any")
            },
        }
    }

    pub fn to_nullable(self) -> Self {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                CortexType::BasicType { nullable: true, name: name }
            },
            CortexType::PointerType { nullable: _, name, mutable } => {
                CortexType::PointerType { nullable: true, name: name, mutable: mutable }
            },
        }
    }
    pub fn to_non_nullable(self) -> Self {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                CortexType::BasicType { nullable: false, name: name }
            },
            CortexType::PointerType { nullable: _, name, mutable } => {
                CortexType::PointerType { nullable: false, name: name, mutable: mutable }
            },
        }
    }

    pub fn types(&self) -> Vec<&PathIdent> {
        match self {
            CortexType::BasicType { nullable: _, name } => vec![name],
            CortexType::PointerType { nullable: _, name, mutable: _ } => vec![name],
        }
    }
    pub fn name(&self) -> &PathIdent {
        match self {
            CortexType::BasicType { nullable: _, name } => name,
            CortexType::PointerType { nullable: _, name, mutable: _ } => name,
        }
    }

    pub fn combine_with(self, other: CortexType) -> Option<CortexType> {
        let is_first_null_type = self == CortexType::null();
        let is_second_null_type = other == CortexType::null();
        if is_first_null_type {
            Some(other.to_nullable())
        } else if is_second_null_type {
            Some(self.to_non_nullable())
        } else if let (
            CortexType::BasicType { nullable: n1, name:name1 }, 
            CortexType::BasicType { nullable: n2, name: name2 }
        ) = (&self, &other) {
            if name1 == name2 {
                Some(Self::new(name1.clone(), *n1 || *n2))
            } else {
                None
            }
        } else if let (
            CortexType::PointerType { nullable: n2, name: name1, mutable: m1 },
            CortexType::PointerType { nullable: n1, name: name2, mutable: m2 }
        ) = (&self, &other) {
            if name1 == name2 {
                Some(Self::pointer(name1.clone(), *n1 || *n2, *m1 || *m2))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn is_subtype_of(&self, other: &CortexType) -> bool {
        if other.nullable() && self == &CortexType::null() {
            return true;
        }
        if !are_same_variant(self, other) {
            return false;
        }
        if let (
            CortexType::BasicType { nullable: n1, name: name1 },
            CortexType::BasicType { nullable: n2, name: name2 }
        ) = (self, other) {
            if name1 == name2 {
                if *n1 && !*n2 {
                    false
                } else {
                    true
                }
            } else {
                false
            }
        } else if let (
            CortexType::PointerType { nullable: n1, name: name1, mutable: m1 },
            CortexType::PointerType { nullable: n2, name: name2, mutable: m2 }
        ) = (self, other) {
            if name1 == name2 {
                if *n1 && !*n2 {
                    false
                } else if *m1 && !*m2 {
                    false
                } else {
                    true
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}

fn are_same_variant<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
