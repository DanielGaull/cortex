use super::typ::CType;

pub struct Expression {
    atom: Atom,
    tail: ExpressionTail,
}

pub enum Atom {
    Number(f64),
    Boolean(bool),
    Void,
    Null,
    String(String),
    PathIdent(PathIdent),
    Expression(Box<Expression>),
}

pub enum ExpressionTail {
    Call {
        args: Vec<Expression>,
        next: Box<ExpressionTail>,
    },

}

pub struct Parameter {
    name: String,
    typ: CType,
}

pub enum OptionalIdentifier {
    Ident(String), // A true identifier
    Ignore, // The ignore token, "~"
}

pub struct PathIdent {
    path: Vec<String>,
}
