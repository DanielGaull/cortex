use std::collections::HashMap;

use crate::{parsing::ast::top_level::ThisArg, r#type::{r#type::{TypeError, TypeParam}, type_env::TypeEnvironment}};

use super::r#type::{RType, RTypeArg};


pub struct RContract {
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
                    typ: TypeEnvironment::fill_type(p.typ, bindings)
                }))
                .collect::<Result<Vec<_>, _>>()?,
            TypeEnvironment::fill_type(self.return_type, bindings),
            self.this_arg,
            self.type_params,
        ))
    }
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
