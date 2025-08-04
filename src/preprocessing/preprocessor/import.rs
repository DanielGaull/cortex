use std::collections::HashSet;

use crate::{interpreting::error::CortexError, parsing::{ast::{expression::PathIdent, top_level::{Import, ImportEntry}}, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::function_address::FunctionAddress, error::PreprocessingError}};

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
                return Err(Box::new(PreprocessingError::DuplicateSymbolImport(alias)));
            }
            let simple = PathIdent::simple(alias.clone());
            if self.has_struct(&simple) || self.has_contract(&simple) || self.has_function(&FunctionAddress::new(simple, None)) {
                return Err(Box::new(PreprocessingError::DuplicateSymbolImport(alias)));
            }
            self.imported_aliases.insert(alias, entry.path);
        } else {
            if self.imported_paths.contains(&entry.path) {
                return Err(Box::new(PreprocessingError::DuplicatePathImport(entry.path.codegen(0))));
            }
            self.imported_paths.push(entry.path);
            self.search_for_duplicate_symbols()?;
        }
        Ok(())
    }

    // Shoudl be run after a new path is added - searches the current context for duplicates
    fn search_for_duplicate_symbols(&self) -> Result<(), CortexError> {
        let mut seen_symbol_names = HashSet::new();
        for (alias, _) in &self.imported_aliases {
            if seen_symbol_names.contains(alias) {
                return Err(Box::new(PreprocessingError::DuplicateSymbolImport(alias.clone())));
            }
            seen_symbol_names.insert(alias.clone());
        }

        let mut prefixes = Vec::new();
        prefixes.push(self.current_context.clone());
        prefixes.extend(self.imported_paths.clone());
        for prefix in prefixes {
            for (address, _) in &self.function_signature_map {
                if address.target.is_none() && address.own_module_path.is_fully_prefixed_by(&prefix) {
                    let symbol = address.own_module_path.get_back()?.clone();
                    if seen_symbol_names.contains(&symbol) {
                        return Err(Box::new(PreprocessingError::DuplicateSymbolImport(symbol)));
                    }
                    seen_symbol_names.insert(symbol);
                }
            }

            for (path, _) in &self.contract_map {
                if path.is_fully_prefixed_by(&prefix) {
                    let symbol = path.get_back()?.clone();
                    if seen_symbol_names.contains(&symbol) {
                        return Err(Box::new(PreprocessingError::DuplicateSymbolImport(symbol)));
                    }
                    seen_symbol_names.insert(symbol);
                }
            }

            for (path, _) in &self.type_map {
                if path.is_fully_prefixed_by(&prefix) {
                    let symbol = path.get_back()?.clone();
                    if seen_symbol_names.contains(&symbol) {
                        return Err(Box::new(PreprocessingError::DuplicateSymbolImport(symbol)));
                    }
                    seen_symbol_names.insert(symbol);
                }
            }
        }

        Ok(())
    }
}
