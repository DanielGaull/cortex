use super::ast::{function::FunctionDict, statement::RStatement};

pub struct Program {
    pub(crate) functions: FunctionDict,
    pub(crate) code: Vec<RStatement>,
}

impl Program {
    pub(crate) fn new(functions: FunctionDict, code: Vec<RStatement>) -> Self {
        Program {
            functions,
            code,
        }
    }
}
