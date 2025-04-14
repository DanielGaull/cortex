use std::error::Error;

use thiserror::Error;

use crate::{constants::INDEX_GET_FN_NAME, interpreting::{heap::Heap, value::CortexValue}, parsing::ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Body, Extension, MemberFunction, PFunction, ThisArg}, r#type::CortexType}, preprocessing::{module::Module, preprocessor::CortexPreprocessor}};

#[derive(Error, Debug)]
pub enum StringError {
    #[error("Expected arg {0} to be of type {1}")]
    InvalidArg(&'static str, &'static str),
    #[error("Invalid index {0} for string of length {1}")]
    InvalidIndex(f64, usize),
}
impl PartialEq for StringError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InvalidArg(l0, l1), Self::InvalidArg(r0, r1)) => *l0 == *r0 && *l1 == *r1,
            (Self::InvalidIndex(l0, l1), Self::InvalidIndex(r0, r1)) => (*l0 - *r0).abs() < f64::EPSILON && *l1 == *r1,
            _ => false,
        }
    }
}

impl CortexPreprocessor {
    pub(crate) fn add_string_funcs(global: &mut Module) -> Result<(), Box<dyn Error>> {
        global.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("toString")),
            vec![Parameter::named("item", CortexType::simple("T", false))],
            CortexType::string(false),
            Body::Native(Box::new(move |env, heap| {
                let item = env.get_value("item")?;
                Ok(CortexValue::String(to_string(item, heap)))
            })),
            vec![String::from("T")],
        ))?;

        global.add_extension(Extension {
            name: PathIdent::simple(String::from("string")),
            type_param_names: vec![],
            functions: vec![
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from(INDEX_GET_FN_NAME)), 
                    vec![
                        Parameter::named("index", CortexType::number(false))
                    ], 
                    CortexType::char(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::Number(num) = env.get_value("index")? {
                                let bytes = strval.as_bytes();
                                let index = f64_to_usize(num).ok_or(StringError::InvalidIndex(num, bytes.len()))?;
                                if index >= bytes.len() {
                                    return Err(Box::new(StringError::InvalidIndex(num, bytes.len())));
                                }
                                Ok(CortexValue::Char(bytes[index]))
                            } else {
                                Err(Box::new(StringError::InvalidArg("index", "number")))
                            }
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("len")), 
                    vec![
                    ], 
                    CortexType::number(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            let bytes = strval.as_bytes();
                            Ok(CortexValue::Number(bytes.len() as f64))
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("isEmpty")), 
                    vec![
                    ], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            let bytes = strval.as_bytes();
                            Ok(CortexValue::Boolean(bytes.len() <= 0))
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("startsWith")), 
                    vec![
                        Parameter::named("substring", CortexType::string(false))
                    ], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::String(substring) = env.get_value("substring")? {
                                Ok(CortexValue::Boolean(strval.starts_with(&substring)))
                            } else {
                                Err(Box::new(StringError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("endsWith")), 
                    vec![
                        Parameter::named("substring", CortexType::string(false))
                    ], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::String(substring) = env.get_value("substring")? {
                                Ok(CortexValue::Boolean(strval.ends_with(&substring)))
                            } else {
                                Err(Box::new(StringError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("contains")), 
                    vec![
                        Parameter::named("substring", CortexType::string(false))
                    ], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::String(substring) = env.get_value("substring")? {
                                Ok(CortexValue::Boolean(strval.contains(&substring)))
                            } else {
                                Err(Box::new(StringError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("indexOf")), 
                    vec![
                        Parameter::named("substring", CortexType::string(false))
                    ], 
                    CortexType::number(true),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::String(substring) = env.get_value("substring")? {
                                let idx = strval.find(&substring);
                                if let Some(i) = idx {
                                    Ok(CortexValue::Number(i as f64))
                                } else {
                                    Ok(CortexValue::None)
                                }
                            } else {
                                Err(Box::new(StringError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("trim")), 
                    vec![
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            Ok(CortexValue::String(String::from(strval.trim())))
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("replace")), 
                    vec![
                        Parameter::named("from", CortexType::string(false)),
                        Parameter::named("to", CortexType::string(false))
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::String(from) = env.get_value("from")? {
                                if let CortexValue::String(to) = env.get_value("to")? {
                                    let result = strval.replace(&from, &to);
                                    Ok(CortexValue::String(result))
                                } else {
                                    Err(Box::new(StringError::InvalidArg("to", "string")))
                                }
                            } else {
                                Err(Box::new(StringError::InvalidArg("from", "string")))
                            }
                        } else {
                            Err(Box::new(StringError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
            ],
        })?;

        Ok(())
    }
}

fn to_string(val: CortexValue, heap: &Heap) -> String {
    match val {
        CortexValue::Number(v) => v.to_string(),
        CortexValue::Boolean(v) => v.to_string(),
        CortexValue::String(s) => s,
        CortexValue::Void => String::from("void"),
        CortexValue::None => String::from("none"),
        CortexValue::Composite { field_values } => {
            let mut s = String::new();
            s.push_str("{");
            s.push_str(&field_values
                .iter()
                .map(|(f, v)| format!("{}: {}", f, to_string(v.borrow().clone(), heap)))
                .collect::<Vec<_>>()
                .join(", ")
            );
            s.push_str("}");
            s
        },
        CortexValue::Reference(addr) => {
            format!("&({})", to_string(heap.get(addr).borrow().clone(), heap))
        },
        CortexValue::List(items) => {
            format!("[{}]", items.iter().map(|i| to_string(i.clone(), heap)).collect::<Vec<_>>().join(", "))
        },
        CortexValue::Char(c) => (c as char).to_string(),
    }
}

fn f64_to_usize(value: f64) -> Option<usize> {
    if value.is_finite() && value >= 0.0 && value.fract() == 0.0 && value <= usize::MAX as f64 {
        Some(value as usize)
    } else {
        None
    }
}
