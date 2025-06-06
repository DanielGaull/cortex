use std::error::Error;

use crate::{constants::INDEX_GET_FN_NAME, interpreting::{heap::Heap, value::CortexValue}, parsing::ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Body, Extension, MemberFunction, PFunction, ThisArg}, r#type::CortexType}, preprocessing::{module::Module, preprocessor::CortexPreprocessor}};

use super::runtime_error::RuntimeError;

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
                                let index = f64_to_usize(num).ok_or(RuntimeError::InvalidIndex(num, bytes.len()))?;
                                if index >= bytes.len() {
                                    return Err(Box::new(RuntimeError::InvalidIndex(num, bytes.len())));
                                }
                                Ok(CortexValue::Char(bytes[index]))
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("index", "number")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                                Err(Box::new(RuntimeError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                                Err(Box::new(RuntimeError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                                Err(Box::new(RuntimeError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                                Err(Box::new(RuntimeError::InvalidArg("substring", "string")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
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
                                    Err(Box::new(RuntimeError::InvalidArg("to", "string")))
                                }
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("from", "string")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("reverse")), 
                    vec![
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            Ok(CortexValue::String(strval.chars().rev().collect()))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("padStart")), 
                    vec![
                        Parameter::named("width", CortexType::number(false)),
                        Parameter::named("c", CortexType::char(false)),
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::Number(widthval) = env.get_value("width")? {
                                if let CortexValue::Char(charval) = env.get_value("c")? {
                                    let width = f64_to_usize(widthval).ok_or(RuntimeError::ExpectedInteger(widthval))?;
                                    let s = pad_start(&strval, width, charval as char);
                                    Ok(CortexValue::String(String::from(s)))
                                } else {
                                    Err(Box::new(RuntimeError::InvalidArg("c", "char")))
                                }
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("width", "number")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("padEnd")), 
                    vec![
                        Parameter::named("width", CortexType::number(false)),
                        Parameter::named("c", CortexType::char(false)),
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::Number(widthval) = env.get_value("width")? {
                                if let CortexValue::Char(charval) = env.get_value("c")? {
                                    let width = f64_to_usize(widthval).ok_or(RuntimeError::ExpectedInteger(widthval))?;
                                    let s = pad_end(&strval, width, charval as char);
                                    Ok(CortexValue::String(String::from(s)))
                                } else {
                                    Err(Box::new(RuntimeError::InvalidArg("c", "char")))
                                }
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("width", "number")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("repeat")), 
                    vec![
                        Parameter::named("times", CortexType::number(false)),
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::Number(widthval) = env.get_value("times")? {
                                let width = f64_to_usize(widthval).ok_or(RuntimeError::ExpectedInteger(widthval))?;
                                let s = strval.repeat(width);
                                Ok(CortexValue::String(String::from(s)))
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("times", "number")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("split")), 
                    vec![
                        Parameter::named("delimiter", CortexType::string(false)),
                    ], 
                    CortexType::reference(CortexType::list(CortexType::string(false), false), false),
                    Body::Native(Box::new(move |env, heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::String(delimiter) = env.get_value("delimiter")? {
                                let split: Vec<&str> = strval.split(&delimiter).collect();
                                let split_list = CortexValue::List(split.into_iter().map(|s| CortexValue::String(String::from(s))).collect());
                                let addr = heap.allocate(split_list);
                                Ok(CortexValue::Reference(addr))
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("times", "number")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("substring")), 
                    vec![
                        Parameter::named("range", CortexType::range(false)),
                    ], 
                    CortexType::string(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::String(strval) = env.get_value("this")? {
                            if let CortexValue::Composite { field_values } = env.get_value("range")? {
                                let startv = field_values.get(&String::from("start")).unwrap().borrow().clone();
                                let endv = field_values.get(&String::from("end")).unwrap().borrow().clone();
                                let stepv = field_values.get(&String::from("step")).unwrap().borrow().clone();
                                let start = some_or(startv, 0usize, strval.len())?;
                                let end = some_or(endv, strval.len(), strval.len())?;
                                let step = some_or(stepv, 1usize, strval.len())?;
                                if start > strval.len() {
                                    return Err(Box::new(RuntimeError::InvalidIndex(start as f64, strval.len())));
                                }
                                if end > strval.len() {
                                    return Err(Box::new(RuntimeError::InvalidIndex(end as f64, strval.len())));
                                }
                                
                                if step == 1 {
                                    let substring = &strval[start..end];
                                    Ok(CortexValue::String(String::from(substring)))
                                } else {
                                    Err(Box::new(RuntimeError::InvalidArg("range", "range (step == 1)")))
                                }
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("range", "range")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "string")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
            ],
        })?;

        global.add_extension(Extension {
            name: PathIdent::simple(String::from("char")),
            type_param_names: vec![],
            functions: vec![
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("isAlpha")), 
                    vec![], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::Char(ch) = env.get_value("this")? {
                            let c = ch as char;
                            Ok(CortexValue::Boolean(c.is_alphabetic()))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "char")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("isDigit")), 
                    vec![], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::Char(ch) = env.get_value("this")? {
                            let c = ch as char;
                            Ok(CortexValue::Boolean(c.is_digit(10)))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "char")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("isWhitespace")), 
                    vec![], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::Char(ch) = env.get_value("this")? {
                            let c = ch as char;
                            Ok(CortexValue::Boolean(c.is_whitespace()))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "char")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("isAlphanumeric")), 
                    vec![], 
                    CortexType::boolean(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::Char(ch) = env.get_value("this")? {
                            let c = ch as char;
                            Ok(CortexValue::Boolean(c.is_alphanumeric()))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "char")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("toUpper")), 
                    vec![], 
                    CortexType::char(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::Char(ch) = env.get_value("this")? {
                            let c = ch as char;
                            Ok(CortexValue::Char(c.to_ascii_uppercase() as u8))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "char")))
                        }
                    })), 
                    ThisArg::DirectThis, 
                    vec![]
                ),
                MemberFunction::new(OptionalIdentifier::Ident(
                    String::from("toLower")), 
                    vec![], 
                    CortexType::char(false),
                    Body::Native(Box::new(move |env, _heap| {
                        if let CortexValue::Char(ch) = env.get_value("this")? {
                            let c = ch as char;
                            Ok(CortexValue::Char(c.to_ascii_lowercase() as u8))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("this", "char")))
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
        CortexValue::Fat(v, _) => to_string(*v, heap),
    }
}

fn f64_to_usize(value: f64) -> Option<usize> {
    if value.is_finite() && value >= 0.0 && value.fract() == 0.0 && value <= usize::MAX as f64 {
        Some(value as usize)
    } else {
        None
    }
}

fn pad_start(s: &str, width: usize, pad_char: char) -> String {
    let len = s.chars().count();
    if len >= width {
        s.to_string()
    } else {
        pad_char.to_string().repeat(width - len) + s
    }
}
fn pad_end(s: &str, width: usize, pad_char: char) -> String {
    let len = s.chars().count();
    if len >= width {
        s.to_string()
    } else {
        s.to_string() + &pad_char.to_string().repeat(width - len)
    }
}

fn some_or(v: CortexValue, other: usize, max: usize) -> Result<usize, Box<dyn Error>> {
    if let CortexValue::Number(n) = v {
        if let Some(v) = f64_to_usize(n) {
            Ok(v)
        } else {
            Err(Box::new(RuntimeError::InvalidIndex(n, max)))
        }
    } else {
        Ok(other)
    }
}
