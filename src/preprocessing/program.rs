use super::ast::statement::RStatement;

pub struct Program {
    pub(crate) code: Vec<RStatement>,
}

impl Program {
    pub(crate) fn new(code: Vec<RStatement>) -> Self {
        Program {
            code,
        }
    }
}
