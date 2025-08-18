use crate::{parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen}, r#type::r#type::{TypeError, TypeParam}};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RFollowsClause {
    pub(crate) entries: Vec<RFollowsEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RFollowsEntry {
    pub(crate) name: PathIdent,
    pub(crate) type_args: Vec<RTypeArg>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RTypeArg {
    Ty(RType),
    Int(i32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RType {
    BasicType(PathIdent, Vec<RTypeArg>),
    RefType(Box<RType>, bool),
    TupleType(Vec<RType>),
    FollowsType(RFollowsClause),
    OptionalType(Box<RType>),
    NoneType,
    GenericType(String),
    FunctionType(Vec<TypeParam>, Vec<RType>, Box<RType>)
}

macro_rules! core_types {
    () => {
        "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64" | 
        "isz" | "usz" | "f32" | "f64" | 
        "bool" | "string" | "void" | "none" | "list" | "char" | "range" | "anonbox" | "span"
    }
}
macro_rules! non_composite_types {
    () => {
        "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64" | 
        "isz" | "usz" | "f32" | "f64" | 
        "bool" | "string" | "void" | "none" | "char" | "anonbox" | "span"
    }
}

pub fn is_path_a_core_type(path: &PathIdent) -> bool {
    path.is_final() && matches!(path.get_back().unwrap().as_str(), core_types!())
}

impl RType {
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
    pub fn char() -> Self {
        Self::simple("char")
    }
    pub fn range() -> Self {
        Self::simple("range")
    }
    pub fn void() -> Self {
        Self::simple("void")
    }
    pub fn string() -> Self {
        Self::simple("string")
    }
    pub fn anonbox() -> Self {
        Self::simple("anonbox")
    }
    pub fn none() -> Self {
        Self::NoneType
    }
    pub fn list(inner: RType) -> Self {
        Self::basic(PathIdent::new(vec!["list"]), vec![RTypeArg::Ty(inner)])
    }
    pub fn span(inner: RType) -> Self {
        Self::basic(PathIdent::new(vec!["span"]), vec![RTypeArg::Ty(inner)])
    }

    pub fn basic(name: PathIdent, args: Vec<RTypeArg>) -> Self {
        Self::BasicType(name, args)
    }
    pub fn reference(contained: RType, mutable: bool) -> Self {
        Self::RefType(Box::new(contained), mutable)
    }

    fn simple(name: &str) -> Self {
        Self::BasicType(PathIdent::simple(String::from(name)), vec![])
    }

    pub fn name(&self) -> Result<PathIdent, TypeError> {
        match &self {
            RType::BasicType(name, _) => Ok(name.clone()),
            RType::RefType(r, _) => r.name(),
            RType::TupleType(_) => Err(TypeError::TupleTypeNotValid),
            RType::FollowsType(_) => Err(TypeError::FollowsTypeNotValid),
            RType::OptionalType(t) => t.name(),
            RType::NoneType => Ok(PathIdent::new(vec!["none"])),
            RType::GenericType(g) => Ok(PathIdent::new(vec![g])),
            RType::FunctionType(..) => Err(TypeError::FunctionTypeNotValid),
        }
    }

    pub fn is_core(&self) -> bool {
        match self {
            Self::BasicType(name, ..) => {
                is_path_a_core_type(name)
            },
            Self::RefType(r, ..) => {
                r.is_core()
            },
            Self::TupleType(_) => false,
            Self::FollowsType(_) => false,
            Self::OptionalType(t) => t.is_core(),
            Self::NoneType => true,
            Self::GenericType(_) => false,
            Self::FunctionType(..) => false,
        }
    }

    pub fn optional(&self) -> bool {
        match self {
            Self::BasicType(..) | Self::RefType(..) | Self::TupleType(..) | 
            Self::FollowsType(..) | Self::GenericType(..) | Self::FunctionType(..) => {
                false
            },
            Self::OptionalType(..) => true,
            Self::NoneType => true,
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
            Self::OptionalType(inner) => {
                if value {
                    Self::OptionalType(inner)
                } else {
                    *inner
                }
            },
            other => {
                if value {
                    Self::OptionalType(Box::new(other))
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

    fn codegen_wrap_if_needed(&self) -> String {
        if self.needs_to_be_wrapped() {
            format!("({})", self.codegen(0))
        } else {
            self.codegen(0)
        }
    }

    fn needs_to_be_wrapped(&self) -> bool {
        match &self {
            Self::BasicType(..) => false,
            Self::RefType(..) => true,
            Self::TupleType(..) => false,
            Self::FollowsType(..) => true,
            Self::OptionalType(..) => true,
            Self::NoneType => false,
            Self::GenericType(..) => false,
            Self::FunctionType(..) => true,
        }
    }

    // Forwards immutability if mutable is false. If mutable is true, returns self
    // Only forwards it if this is a reference type
    pub fn forward_immutability(self, mutable: bool) -> Self {
        if mutable {
            self
        } else {
            match self {
                RType::RefType(r, _) => {
                    RType::RefType(r, false)
                },
                other => other
            }
        }
    }

    pub fn is_non_composite(&self) -> bool {
        match self {
            RType::BasicType(name, ..) => {
                name.is_final() && 
                    matches!(name.get_back().unwrap().as_str(), non_composite_types!())
            },
            RType::RefType(r, ..) => {
                r.is_non_composite()
            },
            RType::TupleType(..) => true,
            RType::FollowsType(..) => true,
            RType::OptionalType(t) => t.is_non_composite(),
            RType::NoneType => true,
            RType::GenericType(..) => false,
            Self::FunctionType(..) => true,
        }
    }
}
impl SimpleCodeGen for RType {
    fn codegen(&self, _: usize) -> String {
        match self {
            Self::BasicType(name, type_args) => {
                let mut s = String::new();
                s.push_str(&name.codegen(0));
                if type_args.len() > 0 {
                    s.push_str("<");
                    s.push_str(&type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(","));
                    s.push_str(">");
                }
                s
            },
            Self::RefType(r, mutable) => {
                let mut s = String::from("&");
                if *mutable {
                    s.push_str("mut ");
                }
                s.push_str(&r.codegen(0));
                s
            },
            Self::TupleType(t) => {
                if t.len() == 1 {
                    format!("({},)", t.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                } else {
                    format!("({})", t.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                }
            },
            Self::FollowsType(f) => {
                format!("{}", f.codegen(0))
            },
            Self::OptionalType(inner) => {
                format!("{}?", inner.codegen_wrap_if_needed())
            },
            Self::NoneType => String::from("none"),
            Self::GenericType(name) => name.clone(),
            Self::FunctionType(type_params, param_types, return_type) => {
                format!(
                    "{}({}) => {}",
                    if type_params.len() > 0 {
                        format!("<{}>", type_params.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "))
                    } else {
                        String::new()
                    },
                    param_types.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "),
                    return_type.codegen(0)
                )
            },
        }
    }
}

impl SimpleCodeGen for RFollowsClause {
    fn codegen(&self, indent: usize) -> String {
        format!("follows {}", self.entries.iter().map(|c| c.codegen(indent)).collect::<Vec<_>>().join(" + "))
    }
}
impl SimpleCodeGen for RFollowsEntry {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.name.codegen(indent));
        if self.type_args.len() > 0 {
            s.push_str(&format!("<{}>", self.type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", ")));
        }
        s
    }
}
impl SimpleCodeGen for RTypeArg {
    fn codegen(&self, indent: usize) -> String {
        match self {
            RTypeArg::Ty(ty) => ty.codegen(indent),
            RTypeArg::Int(i) => format!("{}", i),
        }
    }
}
