use std::collections::{HashMap, HashSet};

use once_cell::sync::Lazy;
use thiserror::Error;

use crate::{interpreting::error::CortexError, parsing::codegen::r#trait::SimpleCodeGen, preprocessing::{module::TypeDefinition, type_env::TypeEnvironment}};

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
    #[error("Tuple type: not valid in this context")]
    TupleTypeNotValid,
    #[error("Follows clause type: not valid in this context")]
    FollowsTypeNotValid,
    #[error("Generic type {0} is not defined")]
    GenericNotDefined(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BasicType {
    pub(crate) name: PathIdent,
    pub(crate) type_args: Vec<TypeArg>,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RefType {
    pub(crate) contained: Box<CortexType>,
    pub(crate) mutable: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TupleType {
    pub(crate) types: Vec<CortexType>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FollowsType {
    pub(crate) clause: FollowsClause,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum CortexType {
    // Represents a simple named type that may or may not have type arguments
    BasicType(BasicType),
    // Represents a reference to some other type
    RefType(RefType),
    // Represents a tuple type
    TupleType(TupleType),
    // Represents a "follows" type
    FollowsType(FollowsType),
    // Represents an optional type - either is a value or is `none`
    OptionalType(Box<CortexType>),
    // Represents the unique `none` type
    NoneType,
    // Represents a generic type
    GenericType(String),
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
            CortexType::TupleType(t) => {
                if t.types.len() == 1 {
                    format!("({},)", t.types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                } else {
                    format!("({})", t.types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                }
            },
            CortexType::FollowsType(t) => {
                format!("{}", t.clause.codegen(0))
            },
            CortexType::OptionalType(inner) => {
                format!("{}?", inner.codegen_wrap_if_needed())
            },
            CortexType::NoneType => String::from("none"),
            CortexType::GenericType(name) => name.clone(),
        }
    }
}

static NONE_NAME: Lazy<PathIdent> = Lazy::new(|| PathIdent::new(vec!["none"]));

impl CortexType {
    fn codegen_wrap_if_needed(&self) -> String {
        if self.needs_to_be_wrapped() {
            format!("({})", self.codegen(0))
        } else {
            self.codegen(0)
        }
    }

    fn needs_to_be_wrapped(&self) -> bool {
        match &self {
            CortexType::BasicType(_) => false,
            CortexType::RefType(_) => true,
            CortexType::TupleType(_) => false,
            CortexType::FollowsType(_) => true,
            CortexType::OptionalType(_) => true,
            CortexType::NoneType => false,
            CortexType::GenericType(_) => false,
        }
    }
}

impl std::fmt::Debug for CortexType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.codegen(0))
    }
}

impl CortexType {
    pub fn basic(name: PathIdent, type_args: Vec<TypeArg>) -> Self {
        Self::BasicType(BasicType {
            name: name,
            type_args: type_args,
        })
    }
    pub fn basic_simple(name: &str, type_args: Vec<TypeArg>) -> Self {
        Self::BasicType(BasicType {
            name: PathIdent::simple(String::from(name)),
            type_args: type_args,
        })
    }
    pub fn reference(contained: CortexType, mutable: bool) -> Self {
        Self::RefType(RefType {
            contained: Box::new(contained),
            mutable: mutable,
        })
    }
    pub fn tuple(types: Vec<CortexType>) -> Self {
        Self::TupleType(TupleType { types })
    }
    pub fn simple(name: &str) -> Self {
        Self::basic(PathIdent::simple(String::from(name)), vec![])
    }
    pub fn number() -> Self {
        Self::simple("number")
    }
    pub fn boolean() -> Self {
        Self::simple("bool")
    }
    pub fn string() -> Self {
        Self::simple("string")
    }
    pub fn void() -> Self {
        Self::simple("void")
    }
    pub fn none() -> Self {
        Self::NoneType
    }
    pub fn list(typ: CortexType) -> Self {
        Self::basic(
            PathIdent::simple(String::from("list")), 
            vec![TypeArg::Ty(typ)]
        )
    }
    pub fn char() -> Self {
        Self::simple("char")
    }
    pub fn range() -> Self {
        Self::simple("range")
    }
    pub fn generic(name: &str) -> Self {
        Self::GenericType(String::from(name))
    }
    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        match self {
            CortexType::BasicType(b) => {
                CortexType::BasicType(BasicType {
                    name: PathIdent::concat(path, &b.name),
                    type_args: b.type_args.clone(),
                })
            },
            CortexType::RefType(r) => {
                CortexType::RefType(RefType {
                    contained: Box::new(r.contained.with_prefix(path)),
                    mutable: r.mutable,
                })
            },
            CortexType::TupleType(t) => {
                CortexType::TupleType(TupleType { types: t.types.iter().map(|t| t.with_prefix(path).clone()).collect() })
            },
            CortexType::FollowsType(f) => {
                CortexType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.iter().map(|c| FollowsEntry {
                            name: PathIdent::concat(path, &c.name),
                            type_args: c.type_args.iter().map(|t| t.with_prefix(path)).collect(),
                        }).collect(),
                    },
                })
            },
            CortexType::OptionalType(t) => {
                CortexType::OptionalType(Box::new(t.with_prefix(path)))
            },
            CortexType::NoneType => CortexType::NoneType,
            CortexType::GenericType(name) => CortexType::GenericType(name.clone()),
        }
    }
    pub fn with_prefix_if_not_core(self, prefix: &PathIdent) -> Self {
        match self {
            CortexType::TupleType(t) => {
                CortexType::TupleType(TupleType {
                    types: t.types
                        .into_iter()
                        .map(|t| t.with_prefix_if_not_core(prefix))
                        .collect(),
                })
            },
            other => {
                if !other.is_core() {
                    other.with_prefix(prefix)
                } else {
                    other
                }
            }
        }
    }
    pub fn subtract_if_possible(self, prefix: &PathIdent) -> Self {
        match self {
            CortexType::BasicType(b) => {
                if b.name.is_prefixed_by(prefix) {
                    CortexType::BasicType(BasicType { name: b.name.subtract(prefix).unwrap(), type_args: b.type_args })
                } else {
                    CortexType::BasicType(b)
                }
            },
            CortexType::RefType(r) => {
                CortexType::RefType(RefType { contained: Box::new(r.contained.subtract_if_possible(prefix)), mutable: r.mutable })
            },
            CortexType::TupleType(t) => {
                CortexType::TupleType(TupleType { types: t.types.iter().map(|t| t.clone().subtract_if_possible(prefix)).collect() })
            },
            CortexType::FollowsType(f) => {
                CortexType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.into_iter().map(|c| FollowsEntry {
                            name: c.name.subtract_if_possible(prefix),
                            type_args: c.type_args.into_iter().map(|t| t.subtract_if_possible(prefix)).collect(),
                        }).collect()
                    },
                })
            },
            CortexType::OptionalType(t) => {
                CortexType::OptionalType(Box::new(t.subtract_if_possible(prefix)))
            },
            CortexType::NoneType => CortexType::NoneType,
            CortexType::GenericType(name) => CortexType::GenericType(name),
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
            CortexType::TupleType(_) | CortexType::FollowsType(_) | 
            CortexType::NoneType | CortexType::GenericType(_) => PathIdent::empty(),
            CortexType::OptionalType(t) => t.prefix(),
        }
    }
    pub fn optional(&self) -> bool {
        match self {
            CortexType::BasicType(_) | CortexType::RefType(_) | CortexType::TupleType(_) | 
            CortexType::FollowsType(_) | CortexType::GenericType(_) => {
                false
            },
            CortexType::OptionalType(_) => true,
            CortexType::NoneType => true,
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
            CortexType::TupleType(_) => false,
            CortexType::FollowsType(_) => false,
            CortexType::OptionalType(t) => t.is_core(),
            CortexType::NoneType => true,
            CortexType::GenericType(_) => false,
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
            CortexType::TupleType(_) => true,
            CortexType::FollowsType(_) => true,
            CortexType::OptionalType(t) => t.is_non_composite(),
            CortexType::NoneType => true,
            CortexType::GenericType(_) => false,
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
            CortexType::OptionalType(inner) => {
                if value {
                    CortexType::OptionalType(inner)
                } else {
                    *inner
                }
            },
            other => {
                if value {
                    CortexType::OptionalType(Box::new(other))
                } else {
                    other
                }
            },
        }
    }
    pub fn to_optional_if_true(self, value: bool) -> Self {
        if value {
            self.to_optional()
        } else {
            self
        }
    }

    pub fn name(&self) -> Result<PathIdent, TypeError> {
        match self {
            CortexType::BasicType(b) => Ok(b.name.clone()),
            CortexType::RefType(r) => r.contained.name(),
            CortexType::TupleType(_) => Err(TypeError::TupleTypeNotValid),
            CortexType::FollowsType(_) => Err(TypeError::FollowsTypeNotValid),
            CortexType::OptionalType(t) => t.name(),
            CortexType::NoneType => Ok(NONE_NAME.clone()),
            CortexType::GenericType(name) => Ok(PathIdent::new(vec![name])),
        }
    }

    pub fn combine_with(self, other: CortexType, type_defs: &HashMap<PathIdent, TypeDefinition>) -> Option<CortexType> {
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
                            if let Some(inner) = b1.type_args.get(0).unwrap().clone().combine_with(b2.type_args.get(0).unwrap().clone(), type_defs) {
                                Some(Self::basic(b1.name.clone(), vec![inner]))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        Some(Self::basic(b1.name.clone(), b1.type_args.clone()))
                    }
                } else {
                    None
                }
            },
            (CortexType::RefType(r1), CortexType::RefType(r2)) => {
                if let Some(res) = r1.contained.clone().combine_with(*r2.contained.clone(), type_defs) {
                    Some(Self::reference(res, r1.mutable || r2.mutable))
                } else {
                    None
                }
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    let mut types = Vec::new();
                    for (t1, t2) in t1.types.into_iter().zip(t2.types) {
                        let new = t1.combine_with(t2, type_defs);
                        if let Some(t) = new {
                            types.push(t);
                        } else {
                            return None;
                        }
                    }
                    Some(CortexType::TupleType(TupleType { types }))
                } else {
                    None
                }
            },
            (CortexType::FollowsType(t1), CortexType::FollowsType(t2)) => {
                let mut common_contracts = HashSet::new();
                for c1 in &t1.clause.contracts {
                    for c2 in &t2.clause.contracts {
                        if c1 == c2 {
                            common_contracts.insert(c1.clone());
                        }
                    }
                }
                if common_contracts.is_empty() {
                    None
                } else {
                    Some(CortexType::FollowsType(FollowsType {
                        clause: FollowsClause {
                            contracts: common_contracts.into_iter().collect(),
                        },
                    }))
                }
            },
            (CortexType::FollowsType(f), CortexType::RefType(r)) => {
                CortexType::FollowsType(f).combine_with(*r.contained, type_defs)
            },
            (CortexType::FollowsType(f), CortexType::BasicType(b)) => {
                if let Some(type_def) = type_defs.get(&b.name) {
                    let mut common_contracts = HashSet::new();
                    for c1 in &f.clause.contracts {
                        for c2 in &type_def.followed_contracts {
                            if c1 == c2 {
                                common_contracts.insert(c1.clone());
                            }
                        }
                    }
                    if common_contracts.is_empty() {
                        None
                    } else {
                        Some(CortexType::FollowsType(FollowsType {
                            clause: FollowsClause {
                                contracts: common_contracts.into_iter().collect(),
                            },
                        }))
                    }
                } else {
                    None
                }
            },
            (CortexType::NoneType, CortexType::OptionalType(o)) | 
            (CortexType::OptionalType(o), CortexType::NoneType)=> {
                Some(CortexType::OptionalType(o))
            },
            (CortexType::OptionalType(t1), other) | 
            (other, CortexType::OptionalType(t1)) => {
                if let Some(result) = t1.combine_with(other, type_defs) {
                    Some(CortexType::OptionalType(Box::new(result)))
                } else {
                    None
                }
            },
            (CortexType::NoneType, CortexType::NoneType) => {
                Some(CortexType::NoneType)
            },
            (CortexType::GenericType(n1), CortexType::GenericType(n2)) => {
                if n1 == n2 {
                    Some(CortexType::GenericType(n1))
                } else {
                    None
                }
            },
            _ => None
        }
    }

    pub fn is_subtype_of(&self, other: &CortexType, type_defs: &HashMap<PathIdent, TypeDefinition>) -> Result<bool, CortexError> {
        if other.optional() && self == &CortexType::none() {
            return Ok(true);
        }

        match (self, other) {
            (CortexType::BasicType(b1), CortexType::BasicType(b2)) => {
                if b1.name == b2.name {
                    Ok(are_type_args_equal(&b1.type_args, &b2.type_args))
                } else {
                    Ok(false)
                }
            },
            (CortexType::RefType(r1), CortexType::RefType(r2)) => {
                if r1.contained.is_subtype_of(&*r2.contained, type_defs)? {
                    if !r1.mutable && r2.mutable {
                        Ok(false)
                    } else {
                        Ok(true)
                    }
                } else {
                    Ok(false)
                }
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    for (t1, t2) in t1.types.iter().zip(&t2.types) {
                        if !t1.is_subtype_of(t2, type_defs)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            },
            (CortexType::FollowsType(t1), CortexType::FollowsType(t2)) => {
                // have to be no contracts in t2 that aren't in t1
                for c in &t2.clause.contracts {
                    if !t1.clause.contracts.contains(c) {
                        return Ok(false);
                    }
                }
                Ok(true)
            },
            (CortexType::RefType(r), CortexType::FollowsType(_)) => {
                r.contained.is_subtype_of(other, type_defs)
            },
            (CortexType::BasicType(b), CortexType::FollowsType(f)) => {
                if let Some(type_def) = type_defs.get(&b.name) {
                    let entries_tentative = 
                        TypeEnvironment::fill_in_follows_entry_from_typedef(
                            b.clone(), 
                            type_def.type_params.clone(), 
                            type_def.followed_contracts.clone()
                        );
                    if entries_tentative.is_err() {
                        return Ok(false);
                    }
                    let entries = entries_tentative.unwrap();
                    
                    // have to be no contracts in f that aren't in b
                    for c in &f.clause.contracts {
                        if !entries.contains(c) {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            },
            (CortexType::OptionalType(o1), CortexType::OptionalType(o2)) => {
                o1.is_subtype_of(o2, type_defs)
            },
            (CortexType::NoneType, CortexType::OptionalType(_)) => {
                Ok(true)
            },
            (other, CortexType::OptionalType(o)) => {
                other.is_subtype_of(&*o, type_defs)
            },
            (CortexType::NoneType, CortexType::NoneType) => {
                Ok(true)
            },
            (CortexType::GenericType(n1), CortexType::GenericType(n2)) => {
                Ok(n1 == n2)
            },
            _ => Ok(false),
        }
    }
}

fn are_type_args_equal(a: &Vec<TypeArg>, b: &Vec<TypeArg>) -> bool {
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

pub fn forwarded_type_args(params: &Vec<TypeParam>) -> Vec<TypeArg> {
    let mut type_args = Vec::new();
    for p in params {
        type_args.push(TypeArg::Ty(CortexType::GenericType(p.name.clone())));
    }
    type_args
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FollowsEntry {
    pub(crate) name: PathIdent,
    pub(crate) type_args: Vec<TypeArg>,
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
impl FollowsEntry {
    pub fn new(name: PathIdent, type_args: Vec<TypeArg>) -> Self {
        FollowsEntry {
            name,
            type_args,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FollowsClause {
    pub(crate) contracts: Vec<FollowsEntry>,
}
impl SimpleCodeGen for FollowsClause {
    fn codegen(&self, indent: usize) -> String {
        format!("follows {}", self.contracts.iter().map(|c| c.codegen(indent)).collect::<Vec<_>>().join(" + "))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeParamType {
    Ty,
    Int,
}
impl SimpleCodeGen for TypeParamType {
    fn codegen(&self, _: usize) -> String {
        match self {
            TypeParamType::Ty => String::from("ty"),
            TypeParamType::Int => String::from("int"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeParam {
    pub(crate) name: String,
    pub(crate) typ: TypeParamType,
}

impl TypeParam {
    pub fn new(name: &str, typ: TypeParamType) -> Self {
        Self {
            name: String::from(name),
            typ,
        }
    }
    pub fn ty(name: &str) -> Self {
        Self::new(name, TypeParamType::Ty)
    }
    pub fn int(name: &str) -> Self {
        Self::new(name, TypeParamType::Int)
    }
}

impl SimpleCodeGen for TypeParam {
    fn codegen(&self, indent: usize) -> String {
        format!("{}: {}", self.name, self.typ.codegen(indent))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeArg {
    Ty(CortexType),
    Int(i32),
}
impl SimpleCodeGen for TypeArg {
    fn codegen(&self, indent: usize) -> String {
        match self {
            TypeArg::Ty(ty) => ty.codegen(indent),
            TypeArg::Int(i) => format!("{}", i),
        }
    }
}
impl TypeArg {
    pub fn combine_with(self, other: Self, type_defs: &HashMap<PathIdent, TypeDefinition>) -> Option<Self> {
        match (self, other) {
            (TypeArg::Ty(t1), TypeArg::Ty(t2)) => {
                Some(TypeArg::Ty(t1.combine_with(t2, type_defs)?))
            },
            (TypeArg::Int(i1), TypeArg::Int(i2)) => {
                if i1 == i2 {
                    Some(TypeArg::Int(i1))
                } else {
                    None
                }
            },
            (_, _) => None,
        }
    }
    
    pub fn with_prefix(&self, prefix: &PathIdent) -> Self {
        match self {
            TypeArg::Ty(typ) => TypeArg::Ty(typ.with_prefix(prefix)),
            other => other.clone(),
        }
    }

    pub(crate) fn subtract_if_possible(self, prefix: &PathIdent) -> Self {
        match self {
            TypeArg::Ty(typ) => TypeArg::Ty(typ.subtract_if_possible(prefix)),
            other => other,
        }
    }
}
