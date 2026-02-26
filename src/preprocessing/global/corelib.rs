use std::error::Error;

use thiserror::Error;

use crate::{
    interpreting::value::CortexValue,
    parsing::ast::{
        expression::{OptionalIdentifier, Parameter, PathIdent},
        top_level::{Body, Contract, MemberFunctionSignature, PFunction, ThisArg},
    },
    preprocessing::{
        global::string::cortex_value_to_string, module::Module,
        preprocessor::preprocessor::CortexPreprocessor,
    },
    r#type::r#type::{PType, TypeParam, TypeParamType},
};

#[derive(Error, Debug)]
pub enum CoreLibError {
    #[error("corelib: mismatched types (expected {0})")]
    MismatchedTypes(String),
    #[error("corelib: Invalid index ({0})")]
    InvalidIndex(String),
}

impl CortexPreprocessor {
    // NOTE:
    // The intention is that calling corelib functions is generally unsafe for users to do
    // The stdlib relies on corelib code - that is because the stdlib enforces the necessary constraints
    // Calling corelib functions without following this constraints can result in undefined behavior
    // Therefore, while they are available to users, corelib functions should be considered dangerous
    // They can oftentimes use `anonbox` rather than true types (which acts as a void* pointer)
    // When they truly expect a certain type (ex. corelib used to circumvent rules around generic functions)
    pub(crate) fn add_corelib(global: &mut Module) -> Result<(), Box<dyn Error>> {
        let mut corelib = Module::new();

        corelib.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("spanIndexGetSingleAnonymous")),
            vec![
                Parameter::named("inputSpan", PType::anonbox()),
                Parameter::named("index", PType::usz()),
            ],
            PType::anonbox(),
            Body::Native(Box::new(|env, heap| {
                let span = env.get_value("inputSpan")?;
                let index = env.get_value("index")?;
                if let (CortexValue::Reference(span_address), CortexValue::USZ(index)) =
                    (span, index)
                {
                    let span = heap.get(span_address).borrow().clone();
                    if let CortexValue::Span(span) = span {
                        let value = span.get(index);
                        if let Some(result) = value {
                            let true_result = CortexValue::AnonymousBox(Box::new(result.clone()));
                            Ok(true_result)
                        } else {
                            Err(Box::new(CoreLibError::InvalidIndex(format!("{}", index))))
                        }
                    } else {
                        Err(Box::new(CoreLibError::MismatchedTypes(String::from(
                            "span<T>, usz",
                        ))))
                    }
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from(
                        "span<T>, usz",
                    ))))
                }
            })),
            vec![],
        ))?;
        corelib.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("spanIndexAssignSingleAnonymous")),
            vec![
                Parameter::named("inputSpan", PType::anonbox()),
                Parameter::named("index", PType::usz()),
                Parameter::named("value", PType::anonbox()),
            ],
            PType::void(),
            Body::Native(Box::new(|env, heap| {
                let span = env.get_value("inputSpan")?;
                let index = env.get_value("index")?;
                if let (CortexValue::Reference(span_address), CortexValue::USZ(index)) =
                    (span, index)
                {
                    if let CortexValue::Span(items) = &mut *heap.get(span_address).borrow_mut() {
                        if index >= items.len() {
                            items[index] = env.get_value("value")?;
                            Ok(CortexValue::Void)
                        } else {
                            Err(Box::new(CoreLibError::InvalidIndex(format!("{}", index))))
                        }
                    } else {
                        Err(Box::new(CoreLibError::MismatchedTypes(String::from(
                            "span<T>, usz",
                        ))))
                    }
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from(
                        "span<T>, usz",
                    ))))
                }
            })),
            vec![],
        ))?;
        corelib.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("spanAllocAnonymous")),
            vec![Parameter::named("size", PType::usz())],
            PType::reference(PType::span(PType::anonbox()), true),
            Body::Native(Box::new(|env, heap| {
                let size = env.get_value("size")?;
                if let CortexValue::USZ(size) = size {
                    let mut values = Vec::new();
                    for _i in 0..size {
                        values.push(CortexValue::AnonymousBox(Box::new(CortexValue::None)));
                    }

                    let addr = heap.allocate(CortexValue::Span(values));
                    Ok(CortexValue::Reference(addr))
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from("usz"))))
                }
            })),
            vec![],
        ))?;

        corelib.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("toStringAnonymous")),
            vec![Parameter::named("value", PType::anonbox())],
            PType::string(),
            Body::Native(Box::new(|env, heap| {
                let value = env.get_value("value")?;
                if let CortexValue::AnonymousBox(value) = value {
                    let str = cortex_value_to_string(*value, heap);
                    Ok(CortexValue::String(str))
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from("usz"))))
                }
            })),
            vec![],
        ))?;

        // TODO: shouldn't need to parameterize Result
        corelib.add_contract(Contract::new(
            "CoreFromSpan",
            vec![
                TypeParam::new("T", TypeParamType::Ty),
            ],
            vec![MemberFunctionSignature::new(
                OptionalIdentifier::Ident(String::from("fromSpan")),
                vec![
                    Parameter::named("collection", PType::span(PType::generic("T"))),
                    Parameter::named("length", PType::usz()),
                ],
                PType::void(),
                ThisArg::RefMutThis, // TODO: should be a static fn, rn it mutates the list
                vec![],
            )],
        ))?;

        global.add_module(&PathIdent::simple(String::from("corelib")), corelib)?;
        Ok(())
    }
}
