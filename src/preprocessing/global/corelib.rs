use std::error::Error;

use crate::{parsing::ast::expression::PathIdent, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::CortexType};


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
        

        global.add_module(&PathIdent::simple(String::from("corelib")), corelib)?;
        Ok(())
    }
}
