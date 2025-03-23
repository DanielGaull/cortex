use super::ast::function::RInterpretedBody;

pub struct Program {
    pub(crate) code: RInterpretedBody,
}

impl Program {
    pub(crate) fn new(code: RInterpretedBody) -> Self {
        Program {
            code,
        }
    }
}
