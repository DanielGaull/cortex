use std::collections::HashMap;

use crate::{interpreting::env::EnvError, preprocessing::ast::r#type::{RFollowsClause, RFollowsEntry, RType, RTypeArg}};

use super::r#type::{TypeError, TypeParam};

pub struct TypeEnvironment {
    bindings: HashMap<TypeParam, RTypeArg>,
    parent: Option<Box<TypeEnvironment>>,
}

impl TypeEnvironment {
    pub fn new(parent: TypeEnvironment) -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }
    pub fn base() -> Self {
        TypeEnvironment {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn add(&mut self, param: TypeParam, value: RTypeArg) {
        self.bindings.insert(param, value);
    }

    pub fn fill_in(&self, typ: RType) -> Result<RType, TypeError> {
        Self::fill_type(typ, &self.bindings)
    }

    pub fn exit(self) -> Result<TypeEnvironment, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }

    pub fn fill(arg: RTypeArg, bindings: &HashMap<TypeParam, RTypeArg>) -> Result<RTypeArg, TypeError> {
        match arg {
            RTypeArg::Ty(ty) => Ok(RTypeArg::Ty(Self::fill_type(ty, bindings)?)),
            other => Ok(other),
        }
    }

    pub fn fill_type(typ: RType, bindings: &HashMap<TypeParam, RTypeArg>) -> Result<RType, TypeError> {
        match typ {
            RType::BasicType(name, type_args) => {
                Ok(RType::BasicType(name, type_args.into_iter().map(|t| Self::fill(t, bindings)).collect::<Result<Vec<_>, _>>()?))
            },
            RType::RefType(contained, mutable) => {
                let new_contained = Self::fill_type(*contained, bindings)?;
                Ok(RType::RefType(Box::new(new_contained), mutable ))
            },
            RType::TupleType(t) => {
                let new_types = t.into_iter().map(|t| Self::fill_type(t, bindings))
                    .collect::<Result<Vec<_>,_>>()?;
                Ok(RType::TupleType(new_types))
            },
            RType::FollowsType(f) => {
                Ok(RType::FollowsType(RFollowsClause {
                    entries: f.entries.into_iter().map(|c| Ok(RFollowsEntry {
                        name: c.name,
                        type_args: c.type_args.into_iter().map(|t| Self::fill(t, bindings)).collect::<Result<Vec<_>, _>>()?
                    })).collect::<Result<Vec<_>, _>>()?,
                }))
            },
            RType::OptionalType(t) => {
                Ok(RType::OptionalType(Box::new(Self::fill_type(*t, bindings)?)))
            },
            RType::NoneType => Ok(RType::NoneType),
            RType::GenericType(name) => {
                if let Some(value) = bindings.get(&TypeParam::ty(&name)) {
                    if let RTypeArg::Ty(ty) = value {
                        return Ok(ty.clone());
                    }
                }
                // Ex. in preprocessing a function, we need to fill in parameter types *before*
                // we attempt to infer them, so there are cases where we want to return back
                // what we read in here
                Ok(RType::GenericType(name))
            }
        }
    }

    pub fn create_bindings(params: &Vec<TypeParam>, values: &Vec<RTypeArg>) -> HashMap<TypeParam, RTypeArg> {
        params.clone().into_iter().zip(values.clone()).collect()
    }

    // For example, going from Iterator<D> where Wrapper<D> follows Iterator<D> when we have a Wrapper<number>
    // to an Iterator<number>
    // Returns a list (in the same order as in the typedef) of all follows entries, filled in
    pub(crate) fn fill_in_follows_entry_from_typedef(type_args: Vec<RTypeArg>, type_params: Vec<TypeParam>, followed_contracts: Vec<RFollowsEntry>) -> Result<Vec<RFollowsEntry>, TypeError> {
        let type_arg_map: HashMap<_, _> = type_params.into_iter().zip(type_args).collect();
        let mut result = Vec::new();
        for init_entry in followed_contracts {
            let mut args = Vec::new();
            for arg in init_entry.type_args {
                args.push(TypeEnvironment::fill(arg, &type_arg_map)?);
            }
            result.push(RFollowsEntry {
                name: init_entry.name,
                type_args: args,
            });
        }
        Ok(result)
    }
}
