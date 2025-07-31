use std::collections::HashMap;

use crate::{parsing::ast::{expression::PathIdent, top_level::ThisArg}, r#type::{r#type::{TypeError, TypeParam}, type_env::TypeEnvironment}};

use super::{function::RBody, r#type::{RFollowsClause, RType, RTypeArg}};

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
impl RMemberFunctionSignature {
    pub fn new(name: String, params: Vec<RParameter>, return_type: RType, this_arg: ThisArg, type_params: Vec<TypeParam>) -> Self {
        RMemberFunctionSignature {
            name: name,
            params: params,
            return_type: return_type,
            this_arg: this_arg,
            type_params,
        }
    }

    pub fn fill_all(self, bindings: &HashMap<TypeParam, RTypeArg>) -> Result<Self, TypeError> {
        Ok(Self::new(
            self.name,
            self.params
                .into_iter()
                .map(|p| Ok(RParameter {
                    name: p.name,
                    typ: TypeEnvironment::fill_type(p.typ, bindings)?
                }))
                .collect::<Result<Vec<_>, _>>()?,
            TypeEnvironment::fill_type(self.return_type, bindings)?,
            self.this_arg,
            self.type_params,
        ))
    }
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
impl RParameter {
    pub fn named(name: &str, typ: RType) -> Self {
        Self {
            name: String::from(name),
            typ
        }
    }
}
