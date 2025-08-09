use crate::{parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen}, r#type::r#type::TypeError};

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
impl RTypeArg {
    pub fn with_prefix(&self, prefix: &PathIdent) -> Self {
        match self {
            RTypeArg::Ty(typ) => RTypeArg::Ty(typ.with_prefix(prefix)),
            other => other.clone(),
        }
    }
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
}

macro_rules! core_types {
    () => {
        "number" | "bool" | "string" | "void" | "none" | "list" | "char" | "range" | "anonbox"
    }
}
macro_rules! non_composite_types {
    () => {
        "number" | "bool" | "string" | "void" | "none" | "char" | "anonbox"
    }
}

pub fn is_path_a_core_type(path: &PathIdent) -> bool {
    path.is_final() && matches!(path.get_back().unwrap().as_str(), core_types!())
}

impl RType {
    pub fn number() -> Self {
        Self::simple("number")
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
        }
    }

    pub fn with_prefix(&self, path: &PathIdent) -> Self {
        match self {
            Self::BasicType(name, type_args) => {
                Self::BasicType(PathIdent::concat(path, &name), type_args.clone())
            },
            Self::RefType(r, mutable) => {
                Self::RefType(Box::new(r.with_prefix(path)), *mutable)
            },
            Self::TupleType(t) => {
                Self::TupleType(t.iter().map(|t| t.with_prefix(path).clone()).collect())
            },
            Self::FollowsType(f) => {
                Self::FollowsType(
                    RFollowsClause {
                        entries: f.entries.iter().map(|c| RFollowsEntry {
                            name: PathIdent::concat(path, &c.name),
                            type_args: c.type_args.iter().map(|t| t.with_prefix(path)).collect(),
                        }).collect(),
                    },
                )
            },
            Self::OptionalType(t) => {
                Self::OptionalType(Box::new(t.with_prefix(path)))
            },
            Self::NoneType => Self::NoneType,
            Self::GenericType(name) => Self::GenericType(name.clone()),
        }
    }

    pub fn optional(&self) -> bool {
        match self {
            Self::BasicType(..) | Self::RefType(..) | Self::TupleType(..) | 
            Self::FollowsType(..) | Self::GenericType(..) => {
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
