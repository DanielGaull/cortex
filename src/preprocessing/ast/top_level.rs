use std::collections::HashMap;

use crate::{parsing::ast::{expression::PathIdent, top_level::ThisArg}, r#type::r#type::TypeParam};

use super::{function::RBody, r#type::{RFollowsClause, RType}};

pub struct RStruct {
    pub(crate) name: String,
    pub(crate) fields: HashMap<String, RType>,
    pub(crate) functions: Vec<RMemberFunction>,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) follows_clause: Option<RFollowsClause>,
}

pub struct RExtension {
    pub(crate) name: PathIdent,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) functions: Vec<RMemberFunction>,
    pub(crate) follows_clause: Option<RFollowsClause>,
}

pub struct RContract {
    pub(crate) name: String,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) function_sigs: Vec<RMemberFunctionSignature>,
}

#[derive(Clone, PartialEq)]
pub struct RMemberFunctionSignature {
    pub(crate) name: String,
    pub(crate) this_arg: ThisArg,
    pub(crate) params: Vec<RParameter>,
    pub(crate) return_type: RType,
    pub(crate) type_params: Vec<TypeParam>,
}

pub struct RMemberFunction {
    pub(crate) signature: RMemberFunctionSignature,
    pub(crate) body: RBody,
}

#[derive(Clone, PartialEq)]
pub struct RParameter {
    pub(crate) name: String,
    pub(crate) typ: RType,
}
