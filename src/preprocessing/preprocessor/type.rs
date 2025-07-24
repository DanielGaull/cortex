use std::collections::HashSet;

use crate::{interpreting::error::CortexError, r#type::{r#type::{CortexType, FollowsClause, FollowsType, TupleType, TypeArg, TypeParam}, type_env::TypeEnvironment}};

use super::preprocessor::CortexPreprocessor;

fn are_type_args_equal(a: &Vec<TypeArg>, b: &Vec<TypeArg>) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for i in 0..a.len() {
        if a.get(i).unwrap() != b.get(i).unwrap() {
            return false;
        }
    }
    true
}

pub fn forwarded_type_args(params: &Vec<TypeParam>) -> Vec<TypeArg> {
    let mut type_args = Vec::new();
    for p in params {
        type_args.push(TypeArg::Ty(CortexType::GenericType(p.name.clone())));
    }
    type_args
}

impl CortexPreprocessor {
    fn combine_type_args(&self, first: TypeArg, second: TypeArg) -> Result<Option<TypeArg>, CortexError> {
        match (first, second) {
            (TypeArg::Ty(t1), TypeArg::Ty(t2)) => {
                if let Some(result) = self.combine_types(t1, t2)? {
                    Ok(Some(TypeArg::Ty(result)))
                } else {
                    Ok(None)
                }
            },
            (TypeArg::Int(i1), TypeArg::Int(i2)) => {
                if i1 == i2 {
                    Ok(Some(TypeArg::Int(i1)))
                } else {
                    Ok(None)
                }
            },
            (_, _) => Ok(None),
        }
    }

    pub fn combine_types(&self, first: CortexType, second: CortexType) -> Result<Option<CortexType>, CortexError> {
        let is_first_none_type = first == CortexType::none();
        let is_second_none_type = second == CortexType::none();
        if is_first_none_type {
            return Ok(Some(second.to_optional()));
        } else if is_second_none_type {
            return Ok(Some(first.to_optional()));
        } 

        match (first, second) {
            (CortexType::BasicType(b1), CortexType::BasicType(b2)) => {
                if b1.name == b2.name {
                    if !are_type_args_equal(&b1.type_args, &b2.type_args) {
                        // When there's only 1 type argument, we can try to combine it (ex. a list<number?> with a list<number>)
                        if b1.type_args.len() == 1 && b2.type_args.len() == 1 {
                            if let Some(inner) = self.combine_type_args(b1.type_args.get(0).unwrap().clone(), b2.type_args.get(0).unwrap().clone())? {
                                Ok(Some(CortexType::basic(b1.name.clone(), vec![inner])))
                            } else {
                                Ok(None)
                            }
                        } else {
                            Ok(None)
                        }
                    } else {
                        Ok(Some(CortexType::basic(b1.name.clone(), b1.type_args.clone())))
                    }
                } else {
                    Ok(None)
                }
            },
            (CortexType::RefType(r1), CortexType::RefType(r2)) => {
                if let Some(res) = self.combine_types(*r1.contained.clone(), *r2.contained.clone())? {
                    Ok(Some(CortexType::reference(res, r1.mutable || r2.mutable)))
                } else {
                    Ok(None)
                }
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    let mut types = Vec::new();
                    for (t1, t2) in t1.types.into_iter().zip(t2.types) {
                        let new = self.combine_types(t1, t2)?;
                        if let Some(t) = new {
                            types.push(t);
                        } else {
                            return Ok(None);
                        }
                    }
                    Ok(Some(CortexType::TupleType(TupleType { types })))
                } else {
                    Ok(None)
                }
            },
            (CortexType::FollowsType(t1), CortexType::FollowsType(t2)) => {
                let mut common_contracts = HashSet::new();
                for c1 in &t1.clause.contracts {
                    for c2 in &t2.clause.contracts {
                        if c1 == c2 {
                            common_contracts.insert(c1.clone());
                        }
                    }
                }
                if common_contracts.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(CortexType::FollowsType(FollowsType {
                        clause: FollowsClause {
                            contracts: common_contracts.into_iter().collect(),
                        },
                    })))
                }
            },
            (CortexType::FollowsType(f), CortexType::RefType(r)) => {
                self.combine_types(CortexType::FollowsType(f), *r.contained)
            },
            (CortexType::FollowsType(f), CortexType::BasicType(b)) => {
                let type_def = self.lookup_type(&b.name)?;
                let mut common_contracts = HashSet::new();
                for c1 in &f.clause.contracts {
                    for c2 in &type_def.followed_contracts {
                        if c1 == c2 {
                            common_contracts.insert(c1.clone());
                        }
                    }
                }
                if common_contracts.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(CortexType::FollowsType(FollowsType {
                        clause: FollowsClause {
                            contracts: common_contracts.into_iter().collect(),
                        },
                    })))
                }
            },
            (CortexType::NoneType, CortexType::OptionalType(o)) | 
            (CortexType::OptionalType(o), CortexType::NoneType) => {
                Ok(Some(CortexType::OptionalType(o)))
            },
            (CortexType::OptionalType(t1), other) | 
            (other, CortexType::OptionalType(t1)) => {
                if let Some(result) = self.combine_types(*t1, other)? {
                    Ok(Some(CortexType::OptionalType(Box::new(result))))
                } else {
                    Ok(None)
                }
            },
            (CortexType::NoneType, CortexType::NoneType) => {
                Ok(Some(CortexType::NoneType))
            },
            (CortexType::GenericType(n1), CortexType::GenericType(n2)) => {
                if n1 == n2 {
                    Ok(Some(CortexType::GenericType(n1)))
                } else {
                    Ok(None)
                }
            },
            _ => Ok(None)
        }
    }

    pub fn is_subtype(&self, first: &CortexType, second: &CortexType) -> Result<bool, CortexError> {
        if second.optional() && first == &CortexType::none() {
            return Ok(true);
        }

        match (first, second) {
            (CortexType::BasicType(b1), CortexType::BasicType(b2)) => {
                if b1.name == b2.name {
                    Ok(are_type_args_equal(&b1.type_args, &b2.type_args))
                } else {
                    Ok(false)
                }
            },
            (CortexType::RefType(r1), CortexType::RefType(r2)) => {
                if self.is_subtype(&*r1.contained, &*r2.contained)? {
                    if !r1.mutable && r2.mutable {
                        Ok(false)
                    } else {
                        Ok(true)
                    }
                } else {
                    Ok(false)
                }
            },
            (CortexType::TupleType(t1), CortexType::TupleType(t2)) => {
                if t1.types.len() == t2.types.len() {
                    for (t1, t2) in t1.types.iter().zip(&t2.types) {
                        if !self.is_subtype(t1, t2)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            },
            (CortexType::FollowsType(t1), CortexType::FollowsType(t2)) => {
                // have to be no contracts in t2 that aren't in t1
                for c in &t2.clause.contracts {
                    if !t1.clause.contracts.contains(c) {
                        return Ok(false);
                    }
                }
                Ok(true)
            },
            (CortexType::RefType(r), CortexType::FollowsType(_)) => {
                self.is_subtype(&*r.contained, second)
            },
            (CortexType::BasicType(b), CortexType::FollowsType(f)) => {
                let type_def = self.lookup_type(&b.name)?;
                let entries_tentative = 
                    TypeEnvironment::fill_in_follows_entry_from_typedef(
                        b.clone(), 
                        type_def.type_params.clone(), 
                        type_def.followed_contracts.clone()
                    );
                if entries_tentative.is_err() {
                    return Ok(false);
                }
                let entries = entries_tentative.unwrap();
                
                // have to be no contracts in f that aren't in b
                for c in &f.clause.contracts {
                    if !entries.contains(c) {
                        return Ok(false);
                    }
                }
                Ok(true)
            },
            (CortexType::OptionalType(o1), CortexType::OptionalType(o2)) => {
                self.is_subtype(o1, o2)
            },
            (CortexType::NoneType, CortexType::OptionalType(_)) => {
                Ok(true)
            },
            (other, CortexType::OptionalType(o)) => {
                self.is_subtype(other, &*o)
            },
            (CortexType::NoneType, CortexType::NoneType) => {
                Ok(true)
            },
            (CortexType::GenericType(n1), CortexType::GenericType(n2)) => {
                Ok(n1 == n2)
            },
            _ => Ok(false),
        }
    }
}
