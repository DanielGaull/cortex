use std::collections::{HashMap, HashSet};

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
                        // When there's only 1 type argument, we can try to combine it (ex. a span<i32?> with a span<i32>)
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
                    Ok(Some(RType::reference(res, m1 && m2)))
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
            (RType::FollowsType(f), RType::RefType(r, _)) |
            (RType::RefType(r, _), RType::FollowsType(f)) => {
                self.combine_types(RType::FollowsType(f), *r)
            },
            (RType::FollowsType(f), RType::BasicType(name, _)) |
            (RType::BasicType(name, _), RType::FollowsType(f)) => {
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
            (RType::FunctionType(tp1, pt1, rt1), RType::FunctionType(tp2, pt2, rt2)) => {
                let attempt1 = self.combine_fn_types(tp1.clone(), pt1.clone(), *rt1.clone(), tp2.clone(), pt2.clone(), *rt2.clone())?;
                if attempt1.is_some() {
                    Ok(attempt1)
                } else {
                    Ok(self.combine_fn_types(tp2, pt2, *rt2, tp1, pt1, *rt1)?)
                }
            },
            _ => Ok(None)
        }
    }

    // Since order doesn't matter when combining function types, but we do need distinct "first" and "second" types,
    // this function is to aid in combining them
    fn combine_fn_types(&self, tp1: Vec<TypeParam>, pt1: Vec<RType>, rt1: RType, tp2: Vec<TypeParam>, pt2: Vec<RType>, rt2: RType) -> Result<Option<RType>, CortexError> {
        if tp1.len() == tp2.len() && pt1.len() == pt2.len() {
            let mut mappings = HashMap::new();
            for (t1, t2) in tp1.iter().zip(tp2.iter()) {
                if t1.typ != t2.typ {
                    return Ok(None);
                }
                mappings.insert(t1.clone(), RTypeArg::Ty(RType::GenericType(t2.name.clone())));
            }

            let mut params = Vec::new();
            for (p1, p2) in pt1.into_iter().zip(pt2) {
                let p1 = TypeEnvironment::fill_type(p1.clone(), &mappings);
                if self.is_subtype(&p2, &p1)? {
                    params.push(p2);
                } else {
                    return Ok(None);
                }
            }
            let rt1 = TypeEnvironment::fill_type(rt1.clone(), &mappings);
            if self.is_subtype(&rt1, &rt2)? {
                Ok(Some(RType::FunctionType(tp2, params, Box::new(rt2))))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
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
            (RType::FunctionType(tp1, pt1, rt1), RType::FunctionType(tp2, pt2, rt2)) => {
                if tp1.len() == tp2.len() && pt1.len() == pt2.len() {
                    let mut mappings = HashMap::new();
                    for (t1, t2) in tp1.iter().zip(tp2) {
                        if t1.typ != t2.typ {
                            return Ok(false);
                        }
                        mappings.insert(t1.clone(), RTypeArg::Ty(RType::GenericType(t2.name.clone())));
                    }

                    for (p1, p2) in pt1.iter().zip(pt2) {
                        let p1 = TypeEnvironment::fill_type(p1.clone(), &mappings);
                        if !self.is_subtype(p2, &p1)? {
                            return Ok(false);
                        }
                    }
                    let r1 = TypeEnvironment::fill_type(*rt1.clone(), &mappings);

                    Ok(self.is_subtype(&r1, rt2)?)
                } else {
                    Ok(false)
                }
            },
            _ => Ok(false),
        }
    }
}
