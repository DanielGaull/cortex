use std::{cell::RefCell, error::Error, rc::Rc};

use thiserror::Error;

use crate::{constants::{INDEX_GET_FN_NAME, INDEX_SET_FN_NAME}, parsing::ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Bundle, Function}, r#type::CortexType}};

use super::{heap::Heap, interpreter::CortexInterpreter, module::Module, value::CortexValue};

#[derive(Error, Debug, PartialEq)]
pub enum ListError {
    #[error("Expected arg {0} to be of type {1}")]
    InvalidArg(&'static str, &'static str),
    #[error("Invalid index {0} for list of length {1}")]
    InvalidIndex(f64, usize),
}

impl CortexInterpreter {
    pub(crate) fn add_list_funcs<'a>(global: &'a mut Module, heap: Rc<RefCell<Heap>>) -> Result<(), Box<dyn Error>> {
        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from(INDEX_GET_FN_NAME))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), false)),
                Parameter::named("index", CortexType::number(false))
            ],
            CortexType::simple("T", false),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &*rheap.borrow().get(addr).borrow() {
                        if let CortexValue::Number(num) = env.get_value("index")? {
                            let index = f64_to_usize(num).ok_or(Box::new(ListError::InvalidIndex(num, items.len())))?;
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
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from(INDEX_SET_FN_NAME))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), false)),
                Parameter::named("index", CortexType::number(false)),
                Parameter::named("value", CortexType::simple("T", false)),
            ],
            CortexType::void(false),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &mut *rheap.borrow().get(addr).borrow_mut() {
                        if let CortexValue::Number(num) = env.get_value("index")? {
                            let index = f64_to_usize(num).ok_or(Box::new(ListError::InvalidIndex(num, items.len())))?;
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
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from("len"))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), false)),
            ],
            CortexType::number(false),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &*rheap.borrow().get(addr).borrow() {
                        Ok(CortexValue::Number(items.len() as f64))
                    } else {
                        Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                    }
                } else {
                    Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                }
            })),
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from("find"))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), false)),
                Parameter::named("item", CortexType::simple("T", false)),
            ],
            CortexType::number(true),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &*rheap.borrow().get(addr).borrow() {
                        let item = env.get_value("item")?;
                        let idx = items.iter().position(|i| *i == item);
                        if let Some(i) = idx {
                            Ok(CortexValue::Number(i as f64))
                        } else {
                            Ok(CortexValue::Null)
                        }
                    } else {
                        Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                    }
                } else {
                    Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                }
            })),
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from("contains"))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), false)),
                Parameter::named("item", CortexType::simple("T", false)),
            ],
            CortexType::boolean(true),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &*rheap.borrow().get(addr).borrow() {
                        let item = env.get_value("item")?;
                        Ok(CortexValue::Boolean(items.contains(&item)))
                    } else {
                        Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                    }
                } else {
                    Err(Box::new(ListError::InvalidArg("this", "&list<T>")))
                }
            })),
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from("add"))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), true)),
                Parameter::named("item", CortexType::simple("T", false)),
            ],
            CortexType::void(false),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &mut *rheap.borrow().get(addr).borrow_mut() {
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
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from("insert"))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), true)),
                Parameter::named("index", CortexType::number(false)),
                Parameter::named("item", CortexType::simple("T", false)),
            ],
            CortexType::void(false),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &mut *rheap.borrow().get(addr).borrow_mut() {
                        if let CortexValue::Number(num) = env.get_value("index")? {
                            let index = f64_to_usize(num).ok_or(Box::new(ListError::InvalidIndex(num, items.len())))?;
                            if index < items.len() {
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
            vec![String::from("T")],
        ))?;

        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(Bundle::get_bundle_func_name(&String::from("list"), &String::from("remove"))),
            vec![
                Parameter::named("this", CortexType::reference(CortexType::list(CortexType::simple("T", false), false), true)),
                Parameter::named("index", CortexType::number(false)),
            ],
            CortexType::void(false),
            Body::Native(Box::new(move |env| {
                let list_ptr = env.get_value("this")?;
                if let CortexValue::Reference(addr, _, _) = list_ptr {
                    if let CortexValue::List(items, _) = &mut *rheap.borrow().get(addr).borrow_mut() {
                        if let CortexValue::Number(num) = env.get_value("index")? {
                            let index = f64_to_usize(num).ok_or(Box::new(ListError::InvalidIndex(num, items.len())))?;
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
            vec![String::from("T")],
        ))?;

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
