use super::typ::CType;

pub enum Expression {

}

pub struct Parameter {
    name: String,
    typ: CType,
}

pub enum OptionalIdentifier {
    Ident(String), // A true identifier
    Ignore, // The ignore token, "~"
}
