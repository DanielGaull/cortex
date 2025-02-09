use std::{collections::HashMap, fmt::Display};

use crate::parsing::ast::{expression::PathIdent, r#type::CortexType};

#[derive(Clone, Debug, PartialEq)]
pub enum CortexValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Void,
    Null,
    Composite {
        struct_name: PathIdent,
        field_values: HashMap<String, CortexValue>,
    },
}

impl Display for CortexValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CortexValue::Number(v) => write!(f, "{}", v),
            CortexValue::Boolean(v) => write!(f, "{}", v),
            CortexValue::String(v) => write!(f, "\"{}\"", v),
            CortexValue::Void => write!(f, "void"),
            CortexValue::Null => write!(f, "null"),
            CortexValue::Composite { struct_name, field_values } => write!(f, "{:?}({:?})", struct_name, field_values),
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
            CortexValue::Composite { struct_name, field_values: _ } => CortexType::new(struct_name.clone(), false),
        }
    }
}
