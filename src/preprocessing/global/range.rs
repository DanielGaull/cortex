use std::error::Error;

use crate::{parsing::ast::{top_level::Struct, r#type::CortexType}, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}};


impl CortexPreprocessor {
    pub(crate) fn add_range_funcs(global: &mut Module) -> Result<(), Box<dyn Error>> {
        global.add_struct(Struct::new(
            "range",
            vec![
                ("start", CortexType::number(true)),
                ("end", CortexType::number(true)),
                ("step", CortexType::number(true)),
            ],
            vec![],
            vec![],
            None
        ))?;

        Ok(())
    }
}
