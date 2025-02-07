use std::fmt::Display;

use crate::parsing::ast::r#type::CortexType;

#[derive(Clone, Debug, PartialEq)]
pub enum CortexValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Void,
    Null,
}

impl Display for CortexValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CortexValue::Number(v) => write!(f, "{}", v),
            CortexValue::Boolean(v) => write!(f, "{}", v),
            CortexValue::String(v) => write!(f, "\"{}\"", v),
            CortexValue::Void => write!(f, "void"),
            CortexValue::Null => write!(f, "null"),
        }
    }
}
impl CortexValue {
    pub fn get_type(&self) -> CortexType {
        match self {
            CortexValue::Number(_) => CortexType::number(false),
            CortexValue::Boolean(_) => CortexType::boolean(false),
            CortexValue::String(_) => CortexType::string(false),
            CortexValue::Void => CortexType::void(false),
            CortexValue::Null => CortexType::null(),
        }
    }
}
