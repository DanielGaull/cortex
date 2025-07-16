use std::collections::HashMap;

use crate::{interpreting::env::EnvError, parsing::ast::r#type::{BasicType, CortexType, FollowsClause, FollowsEntry, FollowsType, RefType, TupleType, TypeArg, TypeError, TypeParam}};

pub struct TypeEnvironment {
    bindings: HashMap<TypeParam, TypeArg>,
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

    pub fn add(&mut self, param: TypeParam, value: TypeArg) {
        self.bindings.insert(param, value);
    }

    pub fn fill_in(&self, typ: CortexType) -> Result<CortexType, TypeError> {
        Self::fill_type(typ, &self.bindings)
    }

    pub fn exit(self) -> Result<TypeEnvironment, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }

    // pub fn does_arg_list_contain<'a>(type_params: &'a Vec<TypeParam>, typ: &CortexType) -> Option<&'a TypeParam> {
    //     let typ_name = typ.name().ok()?;
    //     if typ_name.is_final() {
    //         let name = typ_name.get_back().ok()?;
    //         if let Some(entry) = type_params.iter().find(|p| &p.name == name) {
    //             Some(entry)
    //         } else {
    //             None
    //         }
    //     } else {
    //         None
    //     }
    // }

    pub fn fill(arg: TypeArg, bindings: &HashMap<TypeParam, TypeArg>) -> Result<TypeArg, TypeError> {
        match arg {
            TypeArg::Ty(ty) => Ok(TypeArg::Ty(Self::fill_type(ty, bindings)?)),
            other => Ok(other),
        }
    }

    pub fn fill_type(typ: CortexType, bindings: &HashMap<TypeParam, TypeArg>) -> Result<CortexType, TypeError> {
        match typ {
            CortexType::BasicType(b) => {
                // if !b.name.is_empty() {
                //     let ident = b.name.get_back().unwrap();
                //     if let Some(result) = bindings.get(&TypeParam::ty(ident)) {
                //         if let TypeArg::Ty(ty) = result {
                //             return Ok(ty.clone());
                //         }
                //     }
                // }
                Ok(CortexType::BasicType(BasicType { name: b.name, type_args: b.type_args.into_iter().map(|t| Self::fill(t, bindings)).collect::<Result<Vec<_>, _>>()? }))
            },
            CortexType::RefType(r) => {
                let new_contained = Self::fill_type(*r.contained, bindings)?;
                Ok(CortexType::RefType(RefType { contained: Box::new(new_contained), mutable: r.mutable }))
            },
            CortexType::TupleType(t) => {
                let new_types = t.types.into_iter().map(|t| Self::fill_type(t, bindings))
                    .collect::<Result<Vec<_>,_>>()?;
                Ok(CortexType::TupleType(TupleType { types: new_types }))
            },
            CortexType::FollowsType(f) => {
                Ok(CortexType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.into_iter().map(|c| Ok(FollowsEntry {
                            name: c.name,
                            type_args: c.type_args.into_iter().map(|t| Self::fill(t, bindings)).collect::<Result<Vec<_>, _>>()?
                        })).collect::<Result<Vec<_>, _>>()?,
                    },
                }))
            },
            CortexType::OptionalType(t) => {
                Ok(CortexType::OptionalType(Box::new(Self::fill_type(*t, bindings)?)))
            },
            CortexType::NoneType => Ok(CortexType::NoneType),
            CortexType::GenericType(name) => {
                if let Some(value) = bindings.get(&TypeParam::ty(&name)) {
                    if let TypeArg::Ty(ty) = value {
                        return Ok(ty.clone());
                    }
                }
                // Ex. in preprocessing a function, we need to fill in parameter types *before*
                // we attempt to infer them, so there are cases where we want to return back
                // what we read in here
                Ok(CortexType::GenericType(name))
            }
        }
    }

    pub fn create_bindings(params: &Vec<TypeParam>, values: &Vec<TypeArg>) -> HashMap<TypeParam, TypeArg> {
        params.clone().into_iter().zip(values.clone()).collect()
    }

    // For example, going from Iterator<D> where Wrapper<D> follows Iterator<D> when we have a Wrapper<number>
    // to an Iterator<number>
    // Returns a list (in the same order as in the typedef) of all follows entries, filled in
    pub(crate) fn fill_in_follows_entry_from_typedef(concrete_type: BasicType, type_params: Vec<TypeParam>, followed_contracts: Vec<FollowsEntry>) -> Result<Vec<FollowsEntry>, TypeError> {
        let type_arg_map: HashMap<_, _> = type_params.into_iter().zip(concrete_type.type_args).collect();
        let mut result = Vec::new();
        for init_entry in followed_contracts {
            let mut args = Vec::new();
            for arg in init_entry.type_args {
                args.push(TypeEnvironment::fill(arg, &type_arg_map)?);
            }
            result.push(FollowsEntry {
                name: init_entry.name,
                type_args: args,
            });
        }
        Ok(result)
    }
}
