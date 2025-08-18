use thiserror::Error;

use crate::{parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen}, preprocessing::ast::r#type::{RType, RTypeArg}};

#[derive(Error, Debug, PartialEq)]
pub enum TypeError {
    #[error("Tuple type: not valid in this context")]
    TupleTypeNotValid,
    #[error("Follows clause type: not valid in this context")]
    FollowsTypeNotValid,
    #[error("Function pointer type: not valid in this context")]
    FunctionTypeNotValid,
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
    pub(crate) contained: Box<PType>,
    pub(crate) mutable: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TupleType {
    pub(crate) types: Vec<PType>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FollowsType {
    pub(crate) clause: FollowsClause,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) param_types: Vec<PType>,
    pub(crate) return_type: Box<PType>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum PType {
    // Represents a simple named type that may or may not have type arguments
    BasicType(BasicType),
    // Represents a reference to some other type
    RefType(RefType),
    // Represents a tuple type
    TupleType(TupleType),
    // Represents a "follows" type
    FollowsType(FollowsType),
    // Represents an optional type - either is a value or is `none`
    OptionalType(Box<PType>),
    // Represents the unique `none` type
    NoneType,
    // Represents a generic type
    GenericType(String),
    // Represents a function type. For now, only function pointers; but in the future could support closures as well
    FunctionType(FunctionType),
}

impl SimpleCodeGen for PType {
    fn codegen(&self, _: usize) -> String {
        match self {
            PType::BasicType(b) => {
                let mut s = String::new();
                s.push_str(&b.name.codegen(0));
                if b.type_args.len() > 0 {
                    s.push_str("<");
                    s.push_str(&b.type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(","));
                    s.push_str(">");
                }
                s
            },
            PType::RefType(r) => {
                let mut s = String::from("&");
                if r.mutable {
                    s.push_str("mut ");
                }
                s.push_str(&r.contained.codegen(0));
                s
            },
            PType::TupleType(t) => {
                if t.types.len() == 1 {
                    format!("({},)", t.types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                } else {
                    format!("({})", t.types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                }
            },
            PType::FollowsType(t) => {
                format!("{}", t.clause.codegen(0))
            },
            PType::OptionalType(inner) => {
                format!("{}?", inner.codegen_wrap_if_needed())
            },
            PType::NoneType => String::from("none"),
            PType::GenericType(name) => name.clone(),
            PType::FunctionType(f) => {
                format!(
                    "{}({}) => {}",
                    if f.type_params.len() > 0 {
                        format!("<{}>", f.type_params.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                    } else {
                        String::new()
                    },
                    f.param_types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "),
                    f.return_type.codegen(0)
                )
            },
        }
    }
}

impl PType {
    fn codegen_wrap_if_needed(&self) -> String {
        if self.needs_to_be_wrapped() {
            format!("({})", self.codegen(0))
        } else {
            self.codegen(0)
        }
    }

    fn needs_to_be_wrapped(&self) -> bool {
        match &self {
            PType::BasicType(_) => false,
            PType::RefType(_) => true,
            PType::TupleType(_) => false,
            PType::FollowsType(_) => true,
            PType::OptionalType(_) => true,
            PType::NoneType => false,
            PType::GenericType(_) => false,
            PType::FunctionType(_) => true,
        }
    }
}

impl std::fmt::Debug for PType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.codegen(0))
    }
}

impl PType {
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
    pub fn reference(contained: PType, mutable: bool) -> Self {
        Self::RefType(RefType {
            contained: Box::new(contained),
            mutable: mutable,
        })
    }
    pub fn tuple(types: Vec<PType>) -> Self {
        Self::TupleType(TupleType { types })
    }
    pub fn simple(name: &str) -> Self {
        Self::basic(PathIdent::simple(String::from(name)), vec![])
    }
    pub fn u8() -> Self {
        Self::simple("u8")
    }
    pub fn i8() -> Self {
        Self::simple("i8")
    }
    pub fn u16() -> Self {
        Self::simple("u16")
    }
    pub fn i16() -> Self {
        Self::simple("i16")
    }
    pub fn u32() -> Self {
        Self::simple("u32")
    }
    pub fn i32() -> Self {
        Self::simple("i32")
    }
    pub fn u64() -> Self {
        Self::simple("u64")
    }
    pub fn i64() -> Self {
        Self::simple("i64")
    }
    pub fn usz() -> Self {
        Self::simple("usz")
    }
    pub fn isz() -> Self {
        Self::simple("isz")
    }
    pub fn f32() -> Self {
        Self::simple("f32")
    }
    pub fn f64() -> Self {
        Self::simple("f64")
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
    pub fn list(typ: PType) -> Self {
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
    pub fn anonbox() -> Self {
        Self::simple("anonbox")
    }
    pub fn span(typ: PType) -> Self {
        Self::basic(
            PathIdent::simple(String::from("span")), 
            vec![TypeArg::Ty(typ)]
        )
    }

    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        match self {
            PType::BasicType(b) => {
                PType::BasicType(BasicType {
                    name: PathIdent::concat(path, &b.name),
                    type_args: b.type_args.clone(),
                })
            },
            PType::RefType(r) => {
                PType::RefType(RefType {
                    contained: Box::new(r.contained.with_prefix(path)),
                    mutable: r.mutable,
                })
            },
            PType::TupleType(t) => {
                PType::TupleType(TupleType { types: t.types.iter().map(|t| t.with_prefix(path).clone()).collect() })
            },
            PType::FollowsType(f) => {
                PType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.iter().map(|c| FollowsEntry {
                            name: PathIdent::concat(path, &c.name),
                            type_args: c.type_args.iter().map(|t| t.with_prefix(path)).collect(),
                        }).collect(),
                    },
                })
            },
            PType::OptionalType(t) => {
                PType::OptionalType(Box::new(t.with_prefix(path)))
            },
            PType::NoneType => PType::NoneType,
            PType::GenericType(name) => PType::GenericType(name.clone()),
            PType::FunctionType(f) => 
                PType::FunctionType(
                    FunctionType {
                        type_params: f.type_params.clone(),
                        param_types: f.param_types.iter().map(|p| p.with_prefix(path)).collect(),
                        return_type: Box::new(f.return_type.with_prefix(path)),
                    }
                )
        }
    }

    pub fn name(&self) -> Result<PathIdent, TypeError> {
        match &self {
            PType::BasicType(b) => Ok(b.name.clone()),
            PType::RefType(r) => r.contained.name(),
            PType::TupleType(_) => Err(TypeError::TupleTypeNotValid),
            PType::FollowsType(_) => Err(TypeError::FollowsTypeNotValid),
            PType::OptionalType(t) => t.name(),
            PType::NoneType => Ok(PathIdent::new(vec!["none"])),
            PType::GenericType(g) => Ok(PathIdent::new(vec![g])),
            PType::FunctionType(_) => Err(TypeError::FunctionTypeNotValid),
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
        type_args.push(TypeArg::Ty(PType::GenericType(p.name.clone())));
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
    Ty(PType),
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
