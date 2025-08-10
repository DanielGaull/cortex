use std::error::Error;

use thiserror::Error;

use crate::{interpreting::value::CortexValue, parsing::ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{Body, PFunction}}, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::CortexType};

#[derive(Error, Debug)]
pub enum CoreLibError {
    #[error("corelib: mismatched types (expected {0})")]
    MismatchedTypes(String),
    #[error("corelib: Expected integer ({0})")]
    ExpectedInteger(String),
    #[error("corelib: Missing value at index ({0})")]
    MissingValueAtIndex(String),
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
            OptionalIdentifier::Ident(String::from("spanIndexSingleAnonymous")),
            vec![Parameter::named("inputSpan", CortexType::anonbox()), Parameter::named("index", CortexType::number())],
            CortexType::anonbox(),
            Body::Native(Box::new(|env, _heap| {
                let span = env.get_value("inputSpan")?;
                let index = env.get_value("index")?;
                if let (CortexValue::Span(span), CortexValue::Number(index)) = (span, index) {
                    if let Some(index) = f64_to_usize(index) {
                        let value = span.get(index);
                        if let Some(result) = value {
                            let true_result = CortexValue::AnonymousBox(Box::new(result.clone()));
                            Ok(true_result)
                        } else {
                            Err(Box::new(CoreLibError::MissingValueAtIndex(format!("{}", index))))
                        }
                    } else {
                        Err(Box::new(CoreLibError::ExpectedInteger(format!("{}", index))))
                    }
                } else {
                    Err(Box::new(CoreLibError::MismatchedTypes(String::from("span<T>, number"))))
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
