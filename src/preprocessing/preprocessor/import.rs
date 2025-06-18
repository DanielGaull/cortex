use crate::{interpreting::error::CortexError, parsing::{ast::top_level::{Import, ImportEntry}, codegen::r#trait::SimpleCodeGen}, preprocessing::error::PreprocessingError};

use super::preprocessor::CortexPreprocessor;

impl CortexPreprocessor {
    pub(crate) fn handle_import(&mut self, import: Import) -> Result<(), CortexError> {
        for entry in import.entries {
            self.handle_import_entry(entry)?;
        }
        Ok(())
    }

    fn handle_import_entry(&mut self, entry: ImportEntry) -> Result<(), CortexError> {
        if let Some(alias) = entry.alias {
            if self.imported_aliases.contains_key(&alias) {
                return Err(Box::new(PreprocessingError::DuplicateAliasImport(alias)));
            }
            self.imported_aliases.insert(alias, entry.path);
        } else {
            if self.imported_paths.contains(&entry.path) {
                return Err(Box::new(PreprocessingError::DuplicatePathImport(entry.path.codegen(0))));
            }
            self.imported_paths.push(entry.path);
        }
        Ok(())
    }
}
