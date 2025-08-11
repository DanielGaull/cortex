use std::error::Error;

use crate::{parsing::ast::top_level::Struct, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::PType};


impl CortexPreprocessor {
    pub(crate) fn add_range_struct(global: &mut Module) -> Result<(), Box<dyn Error>> {
        global.add_struct(Struct::new(
            "range",
            vec![
                ("start", PType::usz()),
                ("end", PType::usz()),
                ("step", PType::usz()),
            ],
            vec![],
            vec![],
            None
        ))?;

        Ok(())
    }
}
