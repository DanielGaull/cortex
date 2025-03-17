use thiserror::Error;

use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::expression::PathIdent;

#[derive(Error, Debug, PartialEq)]
pub enum TypeError {
    #[error("Unknown type: not valid in this context")]
    UnknownTypeNotValid,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CortexType {
    // Represents a simple named type that may or may not have type arguments
    BasicType {
        nullable: bool,
        name: PathIdent,
        type_args: Vec<CortexType>,
    },
    // Represents a reference to some other type
    RefType {
        contained: Box<CortexType>,
        mutable: bool,
    },
    // Represents a type that is currently unknown, but will be resolved at some point
    Unknown(bool),
}

impl SimpleCodeGen for CortexType {
    fn codegen(&self, _: usize) -> String {
        match self {
            CortexType::BasicType { nullable, name, type_args } => {
                let mut s = String::new();
                s.push_str(&name.codegen(0));
                if type_args.len() > 0 {
                    s.push_str("<");
                    s.push_str(&type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(","));
                    s.push_str(">");
                }
                if *nullable {
                    s.push_str("?");
                }
                s
            },
            CortexType::RefType { contained, mutable } => {
                let mut s = String::from("&");
                if *mutable {
                    s.push_str("mut ");
                }
                s.push_str(&contained.codegen(0));
                s
            },
            CortexType::Unknown(optional) => format!("<unknown{}>", if *optional {"?"} else {""}),
        }
    }
}

impl CortexType {
    pub fn basic(name: PathIdent, nullable: bool, type_args: Vec<CortexType>) -> Self {
        Self::BasicType {
            name: name,
            nullable: nullable,
            type_args: type_args,
        }
    }
    pub fn basic_simple(name: &str, nullable: bool, type_args: Vec<CortexType>) -> Self {
        Self::BasicType {
            name: PathIdent::simple(String::from(name)),
            nullable: nullable,
            type_args: type_args,
        }
    }
    pub fn reference(contained: CortexType, mutable: bool) -> Self {
        Self::RefType {
            contained: Box::new(contained),
            mutable: mutable,
        }
    }
    pub fn simple(name: &str, nullable: bool) -> Self {
        Self::basic(PathIdent::simple(String::from(name)), nullable, vec![])
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
    pub fn list(typ: CortexType, nullable: bool) -> Self {
        Self::basic(PathIdent::simple(String::from("list")), nullable, vec![typ])
    }
    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        match self {
            CortexType::BasicType { nullable, name, type_args } => {
                CortexType::BasicType {
                    name: PathIdent::concat(path, name),
                    nullable: *nullable,
                    type_args: type_args.clone(),
                }
            },
            CortexType::RefType { contained, mutable } => {
                CortexType::RefType {
                    contained: Box::new(contained.with_prefix(path)),
                    mutable: *mutable,
                }
            },
            CortexType::Unknown(b) => CortexType::Unknown(*b),
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
            CortexType::BasicType { nullable: _, name, type_args: _ } => {
                name.without_last()
            },
            CortexType::RefType { contained, mutable: _ } => {
                contained.prefix()
            },
            CortexType::Unknown(_) => PathIdent::empty(),
        }
    }
    pub fn nullable(&self) -> bool {
        match self {
            CortexType::BasicType { nullable, name: _, type_args: _ } => {
                *nullable
            },
            CortexType::RefType { contained, mutable: _ } => {
                contained.nullable()
            },
            CortexType::Unknown(optional) => *optional,
        }
    }

    pub fn is_core(&self) -> bool {
        match self {
            CortexType::BasicType { nullable: _, name, type_args: _ } => {
                name.is_final() && 
                    matches!(name.get_back().unwrap().as_str(), "number" | "bool" | "string" | "void" | "null" | "list")
            },
            CortexType::RefType { contained, mutable: _ } => {
                contained.is_core()
            },
            CortexType::Unknown(_) => true,
        }
    }

    pub fn to_nullable(self) -> Self {
        self.to_nullable_value(true)
    }
    pub fn to_non_nullable(self) -> Self {
        self.to_nullable_value(false)
    }
    pub fn to_nullable_value(self, value: bool) -> Self {
        match self {
            CortexType::BasicType { nullable: _, name, type_args } => {
                CortexType::BasicType { nullable: value, name: name, type_args: type_args }
            },
            CortexType::RefType { contained, mutable } => {
                CortexType::RefType { contained: Box::new(contained.to_nullable_value(value)), mutable: mutable }
            },
            CortexType::Unknown(_) => CortexType::Unknown(value),
        }
    }
    pub fn to_nullable_if_true(self, value: bool) -> Self {
        if value {
            self.to_nullable()
        } else {
            self
        }
    }

    pub fn types(&self) -> Result<Vec<&PathIdent>, TypeError> {
        match self {
            CortexType::BasicType { nullable: _, name, type_args: _ } => Ok(vec![name]),
            CortexType::RefType { contained, mutable: _ } => contained.types(),
            CortexType::Unknown(_) => Err(TypeError::UnknownTypeNotValid),
        }
    }
    pub fn name(&self) -> Result<&PathIdent, TypeError> {
        match self {
            CortexType::BasicType { nullable: _, name, type_args: _ } => Ok(name),
            CortexType::RefType { contained, mutable: _ } => contained.name(),
            CortexType::Unknown(_) => Err(TypeError::UnknownTypeNotValid),
        }
    }

    pub fn combine_with(self, other: CortexType) -> Option<CortexType> {
        let is_first_null_type = self == CortexType::null();
        let is_second_null_type = other == CortexType::null();
        if is_first_null_type {
            Some(other.to_nullable())
        } else if is_second_null_type {
            Some(self.to_nullable())
        } else if let (
            CortexType::BasicType { nullable: n1, name: name1, type_args: ta1 }, 
            CortexType::BasicType { nullable: n2, name: name2, type_args: ta2 }
        ) = (&self, &other) {
            if name1 == name2 {
                if !are_type_args_equal(ta1, ta2) {
                    if ta1.len() == 1 && ta2.len() == 1 {
                        if let Some(inner) = ta1.get(0).unwrap().clone().combine_with(ta2.get(0).unwrap().clone()) {
                            Some(Self::basic(name1.clone(), *n1 || *n2, vec![inner]))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    Some(Self::basic(name1.clone(), *n1 || *n2, ta1.clone()))
                }
            } else {
                None
            }
        } else if let (
            CortexType::RefType { contained: c1, mutable: m1 },
            CortexType::RefType { contained: c2, mutable: m2 }
        ) = (&self, &other) {
            if let Some(res) = c1.clone().combine_with(*c2.clone()) {
                Some(Self::reference(res, *m1 || *m2))
            } else {
                None
            }
        } else if let CortexType::Unknown(optional) = &self {
            Some(other.to_nullable_if_true(*optional))
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
            CortexType::BasicType { nullable: n1, name: name1, type_args: ta1 },
            CortexType::BasicType { nullable: n2, name: name2, type_args: ta2 }
        ) = (self, other) {
            if name1 == name2 {
                if *n1 && !*n2 {
                    false
                } else if !are_type_args_equal(ta1, ta2) {
                    if ta1.len() == 1 && ta2.len() == 1 {
                        ta1.get(0).unwrap().is_subtype_of(ta2.get(0).unwrap())
                    } else {
                        false
                    }
                } else {
                    true
                }
            } else {
                false
            }
        } else if let (
            CortexType::RefType { contained: c1, mutable: m1 },
            CortexType::RefType { contained: c2, mutable: m2 }
        ) = (self, other) {
            if c1.is_subtype_of(c2) {
                if !*m1 && *m2 {
                    false
                } else {
                    true
                }
            } else {
                false
            }
        } else if let CortexType::Unknown(optional) = self {
            if *optional {
                other.nullable()
            } else {
                true
            }
        } else {
            false
        }
    }
}

fn are_same_variant<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

fn are_type_args_equal(a: &Vec<CortexType>, b: &Vec<CortexType>) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for i in 0..a.len() {
        if a.get(i).unwrap() != b.get(i).unwrap() {
            return false;
        }
    }
    true
}

pub fn forwarded_type_args(names: &Vec<String>) -> Vec<CortexType> {
    let mut type_args = Vec::new();
    for name in names {
        type_args.push(CortexType::basic(PathIdent::simple(name.clone()), false, vec![]));
    }
    type_args
}
