use std::error::Error;

use crate::{parsing::ast::{expression::PathIdent, top_level::Struct}, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::CortexType};


impl CortexPreprocessor {
    pub(crate) fn add_corelib(global: &mut Module) -> Result<(), Box<dyn Error>> {
        let mut corelib = Module::new();
        

        global.add_module(&PathIdent::simple(String::from("corelib")), corelib)?;
        Ok(())
    }
}
