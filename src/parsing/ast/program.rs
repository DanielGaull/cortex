use std::vec::IntoIter;

use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::top_level::{Import, TopLevel};

pub struct ModuleContent {
    pub(crate) imports: Vec<Import>,
    pub(crate) content: Vec<TopLevel>,
}
impl SimpleCodeGen for ModuleContent {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        for im in &self.imports {
            s.push_str(&im.codegen(indent));
            s.push_str("\n");
        }

        for c in &self.content {
            s.push_str(&c.codegen(indent));
        }
        s
    }
}
impl IntoIterator for ModuleContent {
    type Item = TopLevel;

    type IntoIter = IntoIter<TopLevel>;

    fn into_iter(self) -> Self::IntoIter {
        self.content.into_iter()
    }
}
