use std::vec::IntoIter;

use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::top_level::TopLevel;

pub struct Program {
    pub(crate) content: Vec<TopLevel>,
}
impl SimpleCodeGen for Program {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        for c in &self.content {
            s.push_str(&c.codegen(indent));
        }
        s
    }
}
impl IntoIterator for Program {
    type Item = TopLevel;

    type IntoIter = IntoIter<TopLevel>;

    fn into_iter(self) -> Self::IntoIter {
        self.content.into_iter()
    }
}
