use std::collections::HashMap;

use crate::{interpreting::env::EnvError, parsing::ast::r#type::{BasicType, CortexType, FollowsClause, FollowsEntry, FollowsType, RefType, TupleType}};

pub struct TypeEnvironment {
    bindings: HashMap<String, CortexType>,
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

    pub fn add(&mut self, name: String, typ: CortexType) {
        self.bindings.insert(name, typ);
    }

    pub fn fill_in(&self, typ: CortexType) -> CortexType {
        Self::fill(typ, &self.bindings)
    }

    pub fn exit(self) -> Result<TypeEnvironment, EnvError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(EnvError::AlreadyBase)
        }
    }

    pub fn does_arg_list_contain<'a>(type_param_names: &Vec<String>, typ: &'a CortexType) -> Option<&'a String> {
        let typ_name = typ.name().ok()?;
        if typ_name.is_final() {
            let name = typ_name.get_back().unwrap();
            if type_param_names.contains(name) {
                Some(name)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn fill(typ: CortexType, bindings: &HashMap<String, CortexType>) -> CortexType {
        match typ {
            CortexType::BasicType(b) => {
                if !b.name.is_empty() {
                    let ident = b.name.get_back().unwrap();
                    if let Some(result) = bindings.get(ident) {
                        result.clone()
                    } else {
                        CortexType::BasicType(BasicType { name: b.name, type_args: b.type_args.into_iter().map(|t| Self::fill(t, bindings)).collect() })
                    }
                } else {
                    CortexType::BasicType(BasicType { name: b.name, type_args: b.type_args.into_iter().map(|t| Self::fill(t, bindings)).collect() })
                }
            },
            CortexType::RefType(r) => {
                let new_contained = Self::fill(*r.contained, bindings);
                CortexType::RefType(RefType { contained: Box::new(new_contained), mutable: r.mutable })
            },
            CortexType::TupleType(t) => {
                let new_types = t.types.into_iter().map(|t| Self::fill(t, bindings)).collect();
                CortexType::TupleType(TupleType {
                    types: new_types,
                })
            },
            CortexType::FollowsType(f) => {
                CortexType::FollowsType(FollowsType {
                    clause: FollowsClause {
                        contracts: f.clause.contracts.into_iter().map(|c| FollowsEntry {
                            name: c.name,
                            type_args: c.type_args.into_iter().map(|t| Self::fill(t, bindings)).collect()
                        }).collect(),
                    },
                })
            },
            CortexType::OptionalType(t) => {
                CortexType::OptionalType(Box::new(Self::fill(*t, bindings)))
            },
            CortexType::NoneType => CortexType::NoneType,
        }
    }

    pub fn create_bindings(names: &Vec<String>, types: &Vec<CortexType>) -> HashMap<String, CortexType> {
        names.clone().into_iter().zip(types.clone()).collect()
    }

    // For example, going from Iterator<D> where Wrapper<D> follows Iterator<D> when we have a Wrapper<number>
    // to an Iterator<number>
    // Returns a list (in the same order as in the typedef) of all follows entries, filled in
    pub(crate) fn fill_in_follows_entry_from_typedef(concrete_type: BasicType, type_param_names: Vec<String>, followed_contracts: Vec<FollowsEntry>) -> Vec<FollowsEntry> {
        let type_arg_map: HashMap<_, _> = type_param_names.into_iter().zip(concrete_type.type_args).collect();
        let mut result = Vec::new();
        for init_entry in followed_contracts {
            let mut args = Vec::new();
            for arg in init_entry.type_args {
                args.push(TypeEnvironment::fill(arg, &type_arg_map));
            }
            result.push(FollowsEntry {
                name: init_entry.name,
                type_args: args,
            });
        }
        result
    }
}
