use std::error::Error;

use thiserror::Error;

use crate::{interpreting::value::CortexValue, parsing::ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Body, PFunction}}, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::PType};

#[derive(Error, Debug)]
pub enum CoreLibError {
    #[error("corelib: mismatched types (expected {0})")]
    MismatchedTypes(String),
    #[error("corelib: Expected integer ({0})")]
    ExpectedInteger(String),
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
            vec![Parameter::named("inputSpan", PType::anonbox()), Parameter::named("index", PType::usz())],
            PType::anonbox(),
            Body::Native(Box::new(|env, heap| {
                let span = env.get_value("inputSpan")?;
                let index = env.get_value("index")?;
                if let (CortexValue::Reference(span_address), CortexValue::USZ(index)) = (span, index) {
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
                        Err(Box::new(CoreLibError::MismatchedTypes(String::from("span<T>, usz"))))
                    }
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from("span<T>, usz"))))
                }
            })),
            vec![]
        ))?;
        corelib.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("spanIndexAssignSingleAnonymous")),
            vec![Parameter::named("inputSpan", PType::anonbox()), Parameter::named("index", PType::usz()), Parameter::named("value", PType::anonbox())],
            PType::void(),
            Body::Native(Box::new(|env, heap| {
                let span = env.get_value("inputSpan")?;
                let index = env.get_value("index")?;
                if let (CortexValue::Reference(span_address), CortexValue::USZ(index)) = (span, index) {
                    if let CortexValue::Span(items) = &mut *heap.get(span_address).borrow_mut() {
                        if index >= items.len() {
                            items[index] = env.get_value("value")?;
                            Ok(CortexValue::Void)
                        } else {
                            Err(Box::new(CoreLibError::InvalidIndex(format!("{}", index))))
                        }
                    } else {
                        Err(Box::new(CoreLibError::MismatchedTypes(String::from("span<T>, usz"))))
                    }
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from("span<T>, usz"))))
                }
            })),
            vec![]
        ))?;

        global.add_module(&PathIdent::simple(String::from("corelib")), corelib)?;
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
