use thiserror::Error;

use crate::{parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen}, preprocessing::ast::r#type::{RType, RTypeArg}};

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

    pub fn name(&self) -> Result<PathIdent, TypeError> {
        match &self {
            CortexType::BasicType(b) => Ok(b.name.clone()),
            CortexType::RefType(r) => r.contained.name(),
            CortexType::TupleType(_) => Err(TypeError::TupleTypeNotValid),
            CortexType::FollowsType(_) => Err(TypeError::FollowsTypeNotValid),
            CortexType::OptionalType(t) => t.name(),
            CortexType::NoneType => Ok(PathIdent::new(vec!["none"])),
            CortexType::GenericType(g) => Ok(PathIdent::new(vec![g])),
        }
    }
}

pub fn forwarded_type_args(params: &Vec<TypeParam>) -> Vec<RTypeArg> {
    let mut type_args = Vec::new();
    for p in params {
        type_args.push(RTypeArg::Ty(RType::GenericType(p.name.clone())));
    }
    type_args
}
pub fn forwarded_type_args_unvalidated(params: &Vec<TypeParam>) -> Vec<TypeArg> {
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
impl FollowsClause {
    pub fn new(contracts: Vec<FollowsEntry>) -> Self {
        Self {
            contracts
        }
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
    pub fn with_prefix(&self, prefix: &PathIdent) -> Self {
        match self {
            TypeArg::Ty(typ) => TypeArg::Ty(typ.with_prefix(prefix)),
            other => other.clone(),
        }
    }
}
