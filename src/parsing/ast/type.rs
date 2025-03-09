use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::expression::PathIdent;

#[derive(Clone, Debug, PartialEq)]
pub enum CortexType {
    BasicType {
        nullable: bool,
        name: PathIdent,
    },
    PointerType {
        contained: Box<CortexType>,
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
            CortexType::PointerType { contained, mutable } => {
                let mut s = String::new();
                if *mutable {
                    s.push_str("mut ");
                }
                s.push_str(&contained.codegen(0));
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
    pub fn pointer(contained: CortexType, mutable: bool) -> Self {
        Self::PointerType {
            contained: Box::new(contained),
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
            CortexType::PointerType { contained, mutable } => {
                CortexType::PointerType {
                    contained: contained.clone(),
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
            CortexType::PointerType { contained, mutable: _ } => {
                contained.prefix()
            },
        }
    }
    pub fn nullable(&self) -> bool {
        match self {
            CortexType::BasicType { nullable, name: _ } => {
                *nullable
            },
            CortexType::PointerType { contained, mutable: _ } => {
                contained.nullable()
            },
        }
    }

    pub fn is_core(&self) -> bool {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                name.is_final().unwrap() && 
                    matches!(name.get_back().unwrap().as_str(), "number" | "bool" | "string" | "void" | "null" | "any")
            },
            CortexType::PointerType { contained, mutable: _ } => {
                contained.is_core()
            },
        }
    }

    pub fn to_nullable(self) -> Self {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                CortexType::BasicType { nullable: true, name: name }
            },
            CortexType::PointerType { contained, mutable } => {
                CortexType::PointerType { contained: Box::new(contained.to_nullable()), mutable: mutable }
            },
        }
    }
    pub fn to_non_nullable(self) -> Self {
        match self {
            CortexType::BasicType { nullable: _, name } => {
                CortexType::BasicType { nullable: false, name: name }
            },
            CortexType::PointerType { contained, mutable } => {
                CortexType::PointerType { contained: Box::new(contained.to_non_nullable()), mutable: mutable }
            },
        }
    }

    pub fn types(&self) -> Vec<&PathIdent> {
        match self {
            CortexType::BasicType { nullable: _, name } => vec![name],
            CortexType::PointerType { contained, mutable: _ } => contained.types(),
        }
    }
    pub fn name(&self) -> &PathIdent {
        match self {
            CortexType::BasicType { nullable: _, name } => name,
            CortexType::PointerType { contained, mutable: _ } => contained.name(),
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
            CortexType::PointerType { contained: c1, mutable: m1 },
            CortexType::PointerType { contained: c2, mutable: m2 }
        ) = (&self, &other) {
            if let Some(res) = c1.clone().combine_with(*c2.clone()) {
                Some(Self::pointer(res, *m1 || *m2))
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
            CortexType::PointerType { contained: c1, mutable: m1 },
            CortexType::PointerType { contained: c2, mutable: m2 }
        ) = (self, other) {
            if c1.is_subtype_of(c2) {
                if *m1 && !*m2 {
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
