use crate::{parsing::ast::expression::PathIdent, r#type::r#type::TypeError};

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
    pub fn none() -> Self {
        Self::simple("none")
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
}
