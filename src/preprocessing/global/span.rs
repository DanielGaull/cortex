use crate::constants::INDEX_SET_FN_NAME;
use crate::interpreting::error::CortexError;
use crate::preprocessing::preprocessor::preprocessor::CortexPreprocessor;

use crate::r#type::r#type::{TypeArg, TypeParam, TypeParamType};
use crate::{
    constants::INDEX_GET_FN_NAME,
    interpreting::value::CortexValue,
    parsing::ast::{
        expression::{OptionalIdentifier, Parameter, PathIdent},
        top_level::{Body, Extension, MemberFunction, ThisArg},
    },
    preprocessing::module::Module,
    r#type::r#type::PType,
};

use super::runtime_error::RuntimeError;

impl CortexPreprocessor {
    pub(crate) fn add_span_funcs(global: &mut Module) -> Result<(), CortexError> {
        global.add_extension(Extension {
            name: PathIdent::simple(String::from("span")),
            type_params: vec![TypeParam::new("T", TypeParamType::Ty)],
            type_args: vec![TypeArg::Ty(PType::generic("T"))],
            follows_clause: None,
            functions: vec![
                MemberFunction::new(
                    OptionalIdentifier::Ident(String::from("new")),
                    vec![Parameter::named("size", PType::usz())],
                    PType::reference(PType::span(PType::generic("T")), true),
                    Body::Native(Box::new(move |env, heap| {
                        let size = env.get_value("size")?;
                        if let CortexValue::USZ(size) = size {
                            let mut values = Vec::new();
                            for _i in 0..size {
                                // NOTE: This makes it unsafe to access the initial values in this span
                                // Can only access values once they've been assigned
                                // TODO: maybe could have an "undefined" runtime type that is only used to
                                // validate this? Instead of None. "undefined" would be really bad to see,
                                // a clear sign something is wrong
                                values.push(CortexValue::None);
                            }

                            let addr = heap.allocate(CortexValue::Span(values));
                            Ok(CortexValue::Reference(addr))
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("size", "usz")))
                        }
                    })),
                    ThisArg::Static,
                    vec![],
                ),
                MemberFunction::new(
                    OptionalIdentifier::Ident(String::from(INDEX_GET_FN_NAME)),
                    vec![Parameter::named("index", PType::usz())],
                    PType::generic("T"),
                    Body::Native(Box::new(move |env, heap| {
                        let span = env.get_value("this")?;
                        let index = env.get_value("index")?;
                        if let (CortexValue::Reference(span_address), CortexValue::USZ(index)) =
                            (span, index)
                        {
                            let span = heap.get(span_address).borrow().clone();
                            if let CortexValue::Span(span) = span {
                                let value = span.get(index);
                                if let Some(result) = value {
                                    let true_result =
                                        CortexValue::AnonymousBox(Box::new(result.clone()));
                                    Ok(true_result)
                                } else {
                                    Err(Box::new(RuntimeError::InvalidIndex(index, span.len())))
                                }
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("usz", "index")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("usz", "index")))
                        }
                    })),
                    ThisArg::DirectThis,
                    vec![],
                ),
                MemberFunction::new(
                    OptionalIdentifier::Ident(String::from(INDEX_SET_FN_NAME)),
                    vec![
                        Parameter::named("index", PType::usz()),
                        Parameter::named("value", PType::generic("T")),
                    ],
                    PType::void(),
                    Body::Native(Box::new(move |env, heap| {
                        let span = env.get_value("inputSpan")?;
                        let index = env.get_value("index")?;
                        if let (CortexValue::Reference(span_address), CortexValue::USZ(index)) =
                            (span, index)
                        {
                            if let CortexValue::Span(items) =
                                &mut *heap.get(span_address).borrow_mut()
                            {
                                if index >= items.len() {
                                    items[index] = env.get_value("value")?;
                                    Ok(CortexValue::Void)
                                } else {
                                    Err(Box::new(RuntimeError::InvalidIndex(index, items.len())))
                                }
                            } else {
                                Err(Box::new(RuntimeError::InvalidArg("usz", "index")))
                            }
                        } else {
                            Err(Box::new(RuntimeError::InvalidArg("usz", "index")))
                        }
                    })),
                    ThisArg::DirectThis,
                    vec![],
                ),
            ],
        })?;

        Ok(())
    }
}
