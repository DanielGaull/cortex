use crate::{
    interpreting::{error::CortexError, value::CortexValue},
    parsing::ast::{
        expression::{OptionalIdentifier, Parameter, PathIdent},
        top_level::{Body, PFunction},
    },
    preprocessing::{
        global::string::cortex_value_to_string, module::Module,
        preprocessor::preprocessor::CortexPreprocessor,
    },
    r#type::r#type::{PType, TypeParam},
};

impl CortexPreprocessor {
    // NOTE:
    // The intention is that calling corelib functions is generally unsafe for users to do
    // The stdlib relies on corelib code - that is because the stdlib enforces the necessary constraints
    // Calling corelib functions without following this constraints can result in undefined behavior
    // Therefore, while they are available to users, corelib functions should be considered dangerous
    // They can oftentimes use `anonbox` rather than true types (which acts as a void* pointer)
    // When they truly expect a certain type (ex. corelib used to circumvent rules around generic functions)
    pub(crate) fn add_corelib(global: &mut Module) -> Result<(), CortexError> {
        let mut corelib = Module::new();
        corelib.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("toString")),
            vec![Parameter::named("value", PType::generic("T"))],
            PType::string(),
            Body::Native(Box::new(|env, heap| {
                let value = env.get_value("value")?;
                let str = cortex_value_to_string(value, heap);
                Ok(CortexValue::String(str))
            })),
            vec![TypeParam::ty("T")],
        ))?;

        global.add_module(&PathIdent::simple(String::from("corelib")), corelib)?;
        Ok(())
    }
}
