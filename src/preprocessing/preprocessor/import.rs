use crate::{interpreting::error::CortexError, parsing::ast::top_level::Import};

use super::preprocessor::CortexPreprocessor;

impl CortexPreprocessor {
    pub(crate) fn handle_import(&mut self, import: Import) -> Result<(), CortexError> {
        Ok(())
    }
}
