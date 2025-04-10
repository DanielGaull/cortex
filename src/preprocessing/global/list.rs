use std::error::Error;

use thiserror::Error;

use crate::{constants::{INDEX_GET_FN_NAME, INDEX_SET_FN_NAME}, interpreting::value::CortexValue, parsing::ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Body, Extension, MemberFunction, ThisArg}, r#type::CortexType}, preprocessing::{module::Module, preprocessor::CortexPreprocessor}};

#[derive(Error, Debug)]
pub enum ListError {
    #[error("Expected arg {0} to be of type {1}")]
    InvalidArg(&'static str, &'static str),
    #[error("Invalid index {0} for list of length {1}")]
    InvalidIndex(f64, usize),
}

impl PartialEq for ListError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InvalidArg(l0, l1), Self::InvalidArg(r0, r1)) => *l0 == *r0 && *l1 == *r1,
            (Self::InvalidIndex(l0, l1), Self::InvalidIndex(r0, r1)) => (*l0 - *r0).abs() < f64::EPSILON && *l1 == *r1,
            _ => false,
        }
    }
}

impl CortexPreprocessor {
    pub(crate) fn add_list_funcs(global: &mut Module) -> Result<(), Box<dyn Error>> {
        global.add_extension(Extension {
            name: PathIdent::simple(String::from("list")),
            type_param_names: vec![String::from("T")],
            functions: vec![
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from(INDEX_GET_FN_NAME)), 
                    vec![
                        Parameter::named("index", CortexType::number(false))
                    ], 
                    CortexType::simple("T", false), 
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &*rheap.get(addr).borrow() {
                                if let CortexValue::Number(num) = env.get_value("index")? {
                                    let index = f64_to_usize(num).ok_or(ListError::InvalidIndex(num, items.len()))?;
                                    if let Some(item) = items.get(index) {
                                        Ok(item.clone())
                                    } else {
                                        Err(Box::new(ListError::InvalidIndex(num, items.len())))
                                    }
                                } else {
                                    Err(Box::new(ListError::InvalidArg("index", "number")))
                                }
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                        }
                    })), 
                    ThisArg::RefThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from(INDEX_SET_FN_NAME)), 
                    vec![
                        Parameter::named("index", CortexType::number(false)),
                        Parameter::named("value", CortexType::simple("T", false)),
                    ], 
                    CortexType::void(false), 
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &mut *rheap.get(addr).borrow_mut() {
                                if let CortexValue::Number(num) = env.get_value("index")? {
                                    let index = f64_to_usize(num).ok_or(ListError::InvalidIndex(num, items.len()))?;
                                    if index < items.len() {
                                        items[index] = env.get_value("value")?;
                                        Ok(CortexValue::Void)
                                    } else {
                                        Err(Box::new(ListError::InvalidIndex(num, items.len())))
                                    }
                                } else {
                                    Err(Box::new(ListError::InvalidArg("index", "number")))
                                }
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                        }
                    })), 
                    ThisArg::RefMutThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("len")), 
                    vec![
                    ], 
                    CortexType::number(true),
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &*rheap.get(addr).borrow() {
                                Ok(CortexValue::Number(items.len() as f64))
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                        }
                    })),
                    ThisArg::RefThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("find")), 
                    vec![
                        Parameter::named("item", CortexType::simple("T", false)),
                    ], 
                    CortexType::number(true),
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &*rheap.get(addr).borrow() {
                                let item = env.get_value("item")?;
                                let idx = items.iter().position(|i| *i == item);
                                if let Some(i) = idx {
                                    Ok(CortexValue::Number(i as f64))
                                } else {
                                    Ok(CortexValue::None)
                                }
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                        }
                    })),
                    ThisArg::RefThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("contains")), 
                    vec![
                        Parameter::named("item", CortexType::simple("T", false)),
                    ], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &*rheap.get(addr).borrow() {
                                let item = env.get_value("item")?;
                                Ok(CortexValue::Boolean(items.contains(&item)))
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                        }
                    })),
                    ThisArg::RefThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("add")), 
                    vec![
                        Parameter::named("item", CortexType::simple("T", false)),
                    ], 
                    CortexType::void(false),
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &mut *rheap.get(addr).borrow_mut() {
                                let item = env.get_value("item")?;
                                items.push(item);
                                Ok(CortexValue::Void)
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                        }
                    })),
                    ThisArg::RefMutThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("insert")), 
                    vec![
                        Parameter::named("index", CortexType::number(false)),
                        Parameter::named("item", CortexType::simple("T", false)),
                    ], 
                    CortexType::void(false),
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &mut *rheap.get(addr).borrow_mut() {
                                if let CortexValue::Number(num) = env.get_value("index")? {
                                    let index = f64_to_usize(num).ok_or(ListError::InvalidIndex(num, items.len()))?;
                                    if index <= items.len() {
                                        let item = env.get_value("item")?;
                                        items.insert(index, item);
                                        Ok(CortexValue::Void)
                                    } else {
                                        Err(Box::new(ListError::InvalidIndex(num, items.len())))
                                    }
                                } else {
                                    Err(Box::new(ListError::InvalidArg("index", "number")))
                                }
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                        }
                    })),
                    ThisArg::RefMutThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("remove")), 
                    vec![
                        Parameter::named("index", CortexType::number(false)),
                    ], 
                    CortexType::void(false),
                    Body::Native(Box::new(move |env, rheap| {
                        let list_ptr = env.get_value("this")?;
                        if let CortexValue::Reference(addr) = list_ptr {
                            if let CortexValue::List(items) = &mut *rheap.get(addr).borrow_mut() {
                                if let CortexValue::Number(num) = env.get_value("index")? {
                                    let index = f64_to_usize(num).ok_or(ListError::InvalidIndex(num, items.len()))?;
                                    if index < items.len() {
                                        items.remove(index);
                                        Ok(CortexValue::Void)
                                    } else {
                                        Err(Box::new(ListError::InvalidIndex(num, items.len())))
                                    }
                                } else {
                                    Err(Box::new(ListError::InvalidArg("index", "number")))
                                }
                            } else {
                                Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                            }
                        } else {
                            Err(Box::new(ListError::InvalidArg("this", "&mut list<T>")))
                        }
                    })),
                    ThisArg::RefMutThis, 
                    vec![]
                ),
            ]
        })?;

        Ok(())
    }
}

fn f64_to_usize(value: f64) -> Option<usize> {
    if value.is_finite() && value >= 0.0 && value.fract() == 0.0 && value <= usize::MAX as f64 {
        Some(value as usize)
    } else {
        None
    }
}
