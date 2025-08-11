use std::collections::HashSet;

use crate::{interpreting::error::CortexError, preprocessing::ast::r#type::{RFollowsClause, RType, RTypeArg}, r#type::{r#type::{PType, TypeArg, TypeParam}, type_env::TypeEnvironment}};

use super::preprocessor::CortexPreprocessor;

fn are_type_args_equal(a: &Vec<RTypeArg>, b: &Vec<RTypeArg>) -> bool {
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
        type_args.push(TypeArg::Ty(PType::GenericType(p.name.clone())));
    }
    type_args
}

impl CortexPreprocessor {
    fn combine_type_args(&self, first: RTypeArg, second: RTypeArg) -> Result<Option<RTypeArg>, CortexError> {
        match (first, second) {
            (RTypeArg::Ty(t1), RTypeArg::Ty(t2)) => {
                if let Some(result) = self.combine_types(t1, t2)? {
                    Ok(Some(RTypeArg::Ty(result)))
                } else {
                    Ok(None)
                }
            },
            (RTypeArg::Int(i1), RTypeArg::Int(i2)) => {
                if i1 == i2 {
                    Ok(Some(RTypeArg::Int(i1)))
                } else {
                    Ok(None)
                }
            },
            (_, _) => Ok(None),
        }
    }

    pub fn combine_types(&self, first: RType, second: RType) -> Result<Option<RType>, CortexError> {
        let is_first_none_type = first == RType::none();
        let is_second_none_type = second == RType::none();
        if is_first_none_type {
            return Ok(Some(second.to_optional()));
        } else if is_second_none_type {
            return Ok(Some(first.to_optional()));
        } 

        match (first, second) {
            (RType::BasicType(name1, ta1), RType::BasicType(name2, ta2)) => {
                if name1 == name2 {
                    if !are_type_args_equal(&ta1, &ta2) {
                        // When there's only 1 type argument, we can try to combine it (ex. a list<number?> with a list<number>)
                        if ta1.len() == 1 && ta2.len() == 1 {
                            if let Some(inner) = self.combine_type_args(ta1.get(0).unwrap().clone(), ta2.get(0).unwrap().clone())? {
                                Ok(Some(RType::basic(name1.clone(), vec![inner])))
                            } else {
                                Ok(None)
                            }
                        } else {
                            Ok(None)
                        }
                    } else {
                        Ok(Some(RType::basic(name1.clone(), ta1.clone())))
                    }
                } else {
                    Ok(None)
                }
            },
            (RType::RefType(r1, m1), RType::RefType(r2, m2)) => {
                if let Some(res) = self.combine_types(*r1.clone(), *r2.clone())? {
                    Ok(Some(RType::reference(res, m1 || m2)))
                } else {
                    Ok(None)
                }
            },
            (RType::TupleType(t1), RType::TupleType(t2)) => {
                if t1.len() == t2.len() {
                    let mut types = Vec::new();
                    for (t1, t2) in t1.into_iter().zip(t2) {
                        let new = self.combine_types(t1, t2)?;
                        if let Some(t) = new {
                            types.push(t);
                        } else {
                            return Ok(None);
                        }
                    }
                    Ok(Some(RType::TupleType(types)))
                } else {
                    Ok(None)
                }
            },
            (RType::FollowsType(t1), RType::FollowsType(t2)) => {
                let mut common_contracts = HashSet::new();
                for c1 in &t1.entries {
                    for c2 in &t2.entries {
                        if c1 == c2 {
                            common_contracts.insert(c1.clone());
                        }
                    }
                }
                if common_contracts.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(RType::FollowsType(RFollowsClause {
                        entries: common_contracts.into_iter().collect(),
                    })))
                }
            },
            (RType::FollowsType(f), RType::RefType(r, _)) => {
                self.combine_types(RType::FollowsType(f), *r)
            },
            (RType::FollowsType(f), RType::BasicType(name, _)) => {
                let type_def = self.lookup_type(&name)?;
                let mut common_contracts = HashSet::new();
                for c1 in &f.entries {
                    for c2 in &type_def.followed_contracts {
                        if c1 == c2 {
                            common_contracts.insert(c1.clone());
                        }
                    }
                }
                if common_contracts.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(RType::FollowsType(RFollowsClause {
                        entries: common_contracts.into_iter().collect(),
                    })))
                }
            },
            (RType::NoneType, RType::OptionalType(o)) | 
            (RType::OptionalType(o), RType::NoneType) => {
                Ok(Some(RType::OptionalType(o)))
            },
            (RType::OptionalType(t1), other) | 
            (other, RType::OptionalType(t1)) => {
                if let Some(result) = self.combine_types(*t1, other)? {
                    Ok(Some(RType::OptionalType(Box::new(result))))
                } else {
                    Ok(None)
                }
            },
            (RType::NoneType, RType::NoneType) => {
                Ok(Some(RType::NoneType))
            },
            (RType::GenericType(n1), RType::GenericType(n2)) => {
                if n1 == n2 {
                    Ok(Some(RType::GenericType(n1)))
                } else {
                    Ok(None)
                }
            },
            _ => Ok(None)
        }
    }

    pub fn is_subtype(&self, first: &RType, second: &RType) -> Result<bool, CortexError> {
        if second.optional() && first == &RType::none() {
            return Ok(true);
        }

        match (first, second) {
            (RType::BasicType(name1, ta1), RType::BasicType(name2, ta2)) => {
                if name1 == name2 {
                    Ok(are_type_args_equal(&ta1, &ta2))
                } else {
                    Ok(false)
                }
            },
            (RType::RefType(r1, m1), RType::RefType(r2, m2)) => {
                if self.is_subtype(&*r1, &*r2)? {
                    if !*m1 && *m2 {
                        Ok(false)
                    } else {
                        Ok(true)
                    }
                } else {
                    Ok(false)
                }
            },
            (RType::TupleType(t1), RType::TupleType(t2)) => {
                if t1.len() == t2.len() {
                    for (t1, t2) in t1.iter().zip(t2) {
                        if !self.is_subtype(t1, t2)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            },
            (RType::FollowsType(t1), RType::FollowsType(t2)) => {
                // have to be no contracts in t2 that aren't in t1
                for c in &t2.entries {
                    if !t1.entries.contains(c) {
                        return Ok(false);
                    }
                }
                Ok(true)
            },
            (RType::RefType(r, _), RType::FollowsType(_)) => {
                self.is_subtype(&*r, second)
            },
            (RType::BasicType(name, ta1), RType::FollowsType(f)) => {
                let type_def = self.lookup_type(&name)?;
                let entries_tentative = 
                    TypeEnvironment::fill_in_follows_entry_from_typedef(
                        ta1.clone(), 
                        type_def.type_params.clone(), 
                        type_def.followed_contracts.clone()
                    );
                if entries_tentative.is_err() {
                    return Ok(false);
                }
                let entries = entries_tentative.unwrap();
                
                // have to be no contracts in f that aren't in b
                for c in &f.entries {
                    if !entries.contains(c) {
                        return Ok(false);
                    }
                }
                Ok(true)
            },
            (RType::OptionalType(o1), RType::OptionalType(o2)) => {
                self.is_subtype(o1, o2)
            },
            (RType::NoneType, RType::OptionalType(_)) => {
                Ok(true)
            },
            (other, RType::OptionalType(o)) => {
                self.is_subtype(other, &*o)
            },
            (RType::NoneType, RType::NoneType) => {
                Ok(true)
            },
            (RType::GenericType(n1), RType::GenericType(n2)) => {
                Ok(n1 == n2)
            },
            _ => Ok(false),
        }
    }
}
