use thiserror::Error;

use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::expression::PathIdent;

macro_rules! core_types {
    () => {
        "number" | "bool" | "string" | "void" | "none" | "list" | "char" | "range"
    }
}
macro_rules! non_composite_types {
    () => {
        "number" | "bool" | "string" | "void" | "none" | "char"
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum TypeError {
    #[error("Unknown type: not valid in this context")]
    UnknownTypeNotValid,
    #[error("Tuple type: not valid in this context")]
    TupleTypeNotValid,
    #[error("Follows clause type: not valid in this context")]
    FollowsTypeNotValid,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BasicType {
    pub(crate) optional: bool,
    pub(crate) name: PathIdent,
    pub(crate) type_args: Vec<CortexType>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct RefType {
    pub(crate) contained: Box<CortexType>,
    pub(crate) mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleType {
    pub(crate) types: Vec<CortexType>,
    pub(crate) optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FollowsType {
    pub(crate) clause: FollowsClause,
    pub(crate) optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CortexType {
    // Represents a simple named type that may or may not have type arguments
    BasicType(BasicType),
    // Represents a reference to some other type
    RefType(RefType),
    // Represents a type that is currently unknown, but will be resolved at some point
    Unknown(bool),
    // Represents a tuple type
    TupleType(TupleType),
    // Represents a "follows" type
    FollowsType(FollowsType),
}

impl SimpleCodeGen for CortexType {
    fn codegen(&self, _: usize) -> String {
        match self {
            CortexType::BasicType(b) => {
                let mut s = String::new();
                s.push_str(&b.name.codegen(0));
                if b.type_args.len() > 0 {
                    s.push_str("<");
                    s.push_str(&b.type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(","));
                    s.push_str(">");
                }
                if b.optional {
                    s.push_str("?");
                }
                s
            },
            CortexType::RefType(r) => {
                let mut s = String::from("&");
                if r.mutable {
                    s.push_str("mut ");
                }
                s.push_str(&r.contained.codegen(0));
                s
            },
            CortexType::Unknown(optional) => format!("{{unknown{}}}", if *optional {"?"} else {""}),
            CortexType::TupleType(t) => {
                if t.types.len() == 1 {
                    format!("({},){}", t.types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "), if t.optional {"?"} else {""})
                } else {
                    format!("({}){}", t.types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "), if t.optional {"?"} else {""})
                }
            },
            CortexType::FollowsType(t) => {
                format!("{}{}", t.clause.codegen(0), if t.optional {"?"} else {""})
            },
        }
    }
}

impl CortexType {
    pub fn basic(name: PathIdent, optional: bool, type_args: Vec<CortexType>) -> Self {
        Self::BasicType(BasicType {
            name: name,
            optional,
            type_args: type_args,
        })
    }
    pub fn basic_simple(name: &str, optional: bool, type_args: Vec<CortexType>) -> Self {
        Self::BasicType(BasicType {
            name: PathIdent::simple(String::from(name)),
            optional,
            type_args: type_args,
        })
    }
    pub fn reference(contained: CortexType, mutable: bool) -> Self {
        Self::RefType(RefType {
            contained: Box::new(contained),
            mutable: mutable,
        })
    }
    pub fn tuple(types: Vec<CortexType>, optional: bool) -> Self {
        Self::TupleType(TupleType { types, optional })
    }
    pub fn simple(name: &str, optional: bool) -> Self {
        Self::basic(PathIdent::simple(String::from(name)), optional, vec![])
    }
    pub fn number(optional: bool) -> Self {
        Self::simple("number", optional)
    }
    pub fn boolean(optional: bool) -> Self {
        Self::simple("bool", optional)
    }
    pub fn string(optional: bool) -> Self {
        Self::simple("string", optional)
    }
    pub fn void(optional: bool) -> Self {
        Self::simple("void", optional)
    }
    pub fn none() -> Self {
        Self::simple("none", true)
    }
    pub fn list(typ: CortexType, optional: bool) -> Self {
        Self::basic(PathIdent::simple(String::from("list")), optional, vec![typ])
    }
    pub fn char(optional: bool) -> Self {
        Self::simple("char", optional)
    }
    pub fn range(optional: bool) -> Self {
        Self::simple("range", optional)
    }
    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        match self {
            CortexType::BasicType(b) => {
                CortexType::BasicType(BasicType {
                    name: PathIdent::concat(path, &b.name),
                    optional: b.optional,
                    type_args: b.type_args.clone(),
                })
            },
            CortexType::RefType(r) => {
                CortexType::RefType(RefType {
                    contained: Box::new(r.contained.with_prefix(path)),
                    mutable: r.mutable,
                })
            },
            CortexType::Unknown(b) => CortexType::Unknown(*b),
            CortexType::TupleType(t) => {
                CortexType::TupleType(TupleType { types: t.types.iter().map(|t| t.with_prefix(path).clone()).collect(), optional: t.optional })
            },
            CortexType::FollowsType(f) => {
                CortexType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.iter().map(|c| FollowsEntry {
                            name: PathIdent::concat(path, &c.name),
                            type_args: c.type_args.iter().map(|t| t.with_prefix(path)).collect(),
                        }).collect(),
                    },
                    optional: f.optional,
                })
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
    pub fn subtract_if_possible(self, prefix: &PathIdent) -> Self {
        match self {
            CortexType::BasicType(b) => {
                if b.name.is_prefixed_by(prefix) {
                    CortexType::BasicType(BasicType { optional: b.optional, name: b.name.subtract(prefix).unwrap(), type_args: b.type_args })
                } else {
                    CortexType::BasicType(b)
                }
            },
            CortexType::RefType(r) => {
                CortexType::RefType(RefType { contained: Box::new(r.contained.subtract_if_possible(prefix)), mutable: r.mutable })
            },
            CortexType::Unknown(b) => CortexType::Unknown(b),
            CortexType::TupleType(t) => {
                CortexType::TupleType(TupleType { types: t.types.iter().map(|t| t.clone().subtract_if_possible(prefix)).collect(), optional: t.optional })
            },
            CortexType::FollowsType(f) => {
                CortexType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.into_iter().map(|c| FollowsEntry {
                            name: c.name.subtract_if_possible(prefix),
                            type_args: c.type_args.into_iter().map(|t| t.subtract_if_possible(prefix)).collect(),
                        }).collect()
                    },
                    optional: f.optional,
                })
            },
        }
    }
    // Forwards immutability if mutable is false. If mutable is true, returns self
    // Only forwards it if this is a reference type
    pub fn forward_immutability(self, mutable: bool) -> Self {
        if mutable {
            self
        } else {
            match self {
                CortexType::RefType(r) => {
                    CortexType::RefType(RefType { contained: r.contained, mutable: false })
                },
                other => other
            }
        }
    }

    pub fn prefix(&self) -> PathIdent {
        match self {
            CortexType::BasicType(b) => {
                b.name.without_last()
            },
            CortexType::RefType(r) => {
                r.contained.prefix()
            },
            CortexType::Unknown(_) | CortexType::TupleType(_) | CortexType::FollowsType(_) => PathIdent::empty(),
        }
    }
    pub fn optional(&self) -> bool {
        match self {
            CortexType::BasicType(b) => {
                b.optional
            },
            CortexType::RefType(r) => {
                r.contained.optional()
            },
            CortexType::Unknown(optional) => *optional,
            CortexType::TupleType(t) => t.optional,
            CortexType::FollowsType(t) => t.optional,
        }
    }

    pub fn is_core(&self) -> bool {
        match self {
            CortexType::BasicType(b) => {
                b.name.is_final() && 
                    matches!(b.name.get_back().unwrap().as_str(), core_types!())
            },
            CortexType::RefType(r) => {
                r.contained.is_core()
            },
            CortexType::Unknown(_) => true,
            CortexType::TupleType(_) => false,
            CortexType::FollowsType(_) => false,
        }
    }
    pub fn is_non_composite(&self) -> bool {
        match self {
            CortexType::BasicType(b) => {
                b.name.is_final() && 
                    matches!(b.name.get_back().unwrap().as_str(), non_composite_types!())
            },
            CortexType::RefType(r) => {
                r.contained.is_non_composite()
            },
            CortexType::Unknown(_) => true,
            CortexType::TupleType(_) => true,
            CortexType::FollowsType(_) => true,
        }
    }

    pub fn to_optional(self) -> Self {
        self.to_optional_value(true)
    }
    pub fn to_non_optional(self) -> Self {
        self.to_optional_value(false)
    }
    pub fn to_optional_value(self, value: bool) -> Self {
        match self {
            CortexType::BasicType(b) => {
                CortexType::BasicType(BasicType { optional: value, name: b.name, type_args: b.type_args })
            },
            CortexType::RefType(r) => {
                CortexType::RefType(RefType { contained: Box::new(r.contained.to_optional_value(value)), mutable: r.mutable })
            },
            CortexType::Unknown(_) => CortexType::Unknown(value),
            CortexType::TupleType(t) => CortexType::TupleType(TupleType { types: t.types, optional: value }),
            CortexType::FollowsType(t) => CortexType::FollowsType(FollowsType {
                clause: t.clause,
                optional: value,
            })
        }
    }
    pub fn to_optional_if_true(self, value: bool) -> Self {
        if value {
            self.to_optional()
        } else {
            self
        }
    }

    pub fn name(&self) -> Result<&PathIdent, TypeError> {
        match self {
            CortexType::BasicType(b) => Ok(&b.name),
            CortexType::RefType(r) => r.contained.name(),
            CortexType::Unknown(_) => Err(TypeError::UnknownTypeNotValid),
            CortexType::TupleType(_) => Err(TypeError::TupleTypeNotValid),
            CortexType::FollowsType(_) => Err(TypeError::FollowsTypeNotValid),
        }
    }

    pub fn combine_with(self, other: CortexType) -> Option<CortexType> {
        let is_first_none_type = self == CortexType::none();
        let is_second_none_type = other == CortexType::none();
        if is_first_none_type {
            return Some(other.to_optional());
        } else if is_second_none_type {
            return Some(self.to_optional());
        } 

        match (self, other) {
            (CortexType::BasicType(b1), CortexType::BasicType(b2)) => {
                if b1.name == b2.name {
                    if !are_type_args_equal(&b1.type_args, &b2.type_args) {
                        // When there's only 1 type argument, we can try to combine it (ex. a list<number?> with a list<number>)
                        if b1.type_args.len() == 1 && b2.type_args.len() == 1 {
                            if let Some(inner) = b1.type_args.get(0).unwrap().clone().combine_with(b2.type_args.get(0).unwrap().clone()) {
                                Some(Self::basic(b1.name.clone(), b1.optional || b2.optional, vec![inner]))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        Some(Self::basic(b1.name.clone(), b1.optional || b2.optional, b1.type_args.clone()))
                    }
                } else {
                    None
                }
            },
            (CortexType::RefType(r1), CortexType::RefType(r2)) => {
                if let Some(res) = r1.contained.clone().combine_with(*r2.contained.clone()) {
                    Some(Self::reference(res, r1.mutable || r2.mutable))
                } else {
                    None
                }
            },
            (CortexType::Unknown(optional), other) => {
                Some(other.to_optional_if_true(optional))
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    let mut types = Vec::new();
                    for (t1, t2) in t1.types.into_iter().zip(t2.types) {
                        let new = t1.combine_with(t2);
                        if let Some(t) = new {
                            types.push(t);
                        } else {
                            return None;
                        }
                    }
                    Some(CortexType::TupleType(TupleType { types, optional: t1.optional || t2.optional }))
                } else {
                    None
                }
            },
            _ => None
        }
    }

    pub fn is_subtype_of(&self, other: &CortexType) -> bool {
        if other.optional() && self == &CortexType::none() {
            return true;
        }

        match (self, other) {
            (CortexType::BasicType(b1), CortexType::BasicType(b2)) => {
                if b1.name == b2.name {
                    if b1.optional && !b2.optional {
                        false
                    } else if !are_type_args_equal(&b1.type_args, &b2.type_args) {
                        if b1.type_args.len() == 1 && b2.type_args.len() == 1 {
                            b1.type_args.get(0).unwrap().is_subtype_of(b2.type_args.get(0).unwrap())
                        } else {
                            false
                        }
                    } else {
                        true
                    }
                } else {
                    false
                }
            },
            (CortexType::RefType(r1), CortexType::RefType(r2)) => {
                if r1.contained.is_subtype_of(&*r2.contained) {
                    if !r1.mutable && r2.mutable {
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            },
            (CortexType::Unknown(optional), other) => {
                if *optional {
                    other.optional()
                } else {
                    true
                }
            },
            (other, CortexType::Unknown(optional)) => {
                if *optional {
                    other.optional()
                } else {
                    true
                }
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    for (t1, t2) in t1.types.iter().zip(&t2.types) {
                        if !t1.is_subtype_of(t2) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            },
            _ => false,
        }
    }
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

#[derive(Clone, Debug, PartialEq)]
pub struct FollowsEntry {
    pub(crate) name: PathIdent,
    pub(crate) type_args: Vec<CortexType>,
}
impl SimpleCodeGen for FollowsEntry {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.name.codegen(indent));
        if self.type_args.len() > 0 {
            s.push_str(&format!("<{}>", self.type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", ")));
        }
        s
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct FollowsClause {
    pub(crate) contracts: Vec<FollowsEntry>,
}
impl SimpleCodeGen for FollowsClause {
    fn codegen(&self, indent: usize) -> String {
        format!("follows {}", self.contracts.iter().map(|c| c.codegen(indent)).collect::<Vec<_>>().join(" + "))
    }
}
