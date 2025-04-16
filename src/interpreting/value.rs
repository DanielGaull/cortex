use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ValueError {
    #[error("Field {0} does not exist on struct {1}")]
    FieldDoesNotExist(String, String),
    #[error("You cannot modify fields on a non-composite value")]
    CannotModifyFieldOnNonComposite,
    #[error("You cannot access fields on a non-composite value (variant: {0})")]
    CannotAccessMemberOfNonComposite(&'static str),
    #[error("Member path cannot be empty")]
    MemberPathCannotBeEmpty,
    #[error("Cannot modify member of non-mutable reference")]
    CannotModifyNonMutableReference,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CortexValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Char(u8),
    Void,
    None,
    Composite {
        field_values: HashMap<String, Rc<RefCell<CortexValue>>>,
    },
    Reference(usize),
    List(Vec<CortexValue>),
}

impl Display for CortexValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CortexValue::Number(v) => write!(f, "{}", v),
            CortexValue::Boolean(v) => write!(f, "{}", v),
            CortexValue::String(v) => write!(f, "\"{}\"", v),
            CortexValue::Void => write!(f, "void"),
            CortexValue::None => write!(f, "none"),
            CortexValue::Composite { field_values } => {
                let mut s = String::new();
                for (name, val) in field_values {
                    s.push_str(name);
                    s.push_str(":");
                    s.push_str(&format!("{}", val.borrow()));
                    s.push_str(";");
                }
                write!(f, "{{ {} }}", s)
            },
            CortexValue::Reference(addr) => write!(f, "&0x{:x}", addr),
            CortexValue::List(list) => {
                let _ = write!(f, "[");
                for (i, item) in list.iter().enumerate() {
                    let _ = write!(f, "{}", item);
                    if i + 1 < list.len() {
                        let _ = write!(f, ", ");
                    }
                }
                write!(f, "]")
            },
            CortexValue::Char(v) => write!(f, "\'{}\'", *v as char),
        }
    }
}
impl CortexValue {
    pub fn new_composite(fields: Vec<(&str, CortexValue)>) -> Self {
        CortexValue::Composite { 
            field_values: fields
                .into_iter()
                .map(|(n, v)| (String::from(n), Rc::new(RefCell::new(v))))
                .collect(),
        }
    }

    fn get_variant_name(&self) -> &'static str {
        match self {
            CortexValue::Number(_) => "number",
            CortexValue::Boolean(_) => "bool",
            CortexValue::String(_) => "string",
            CortexValue::Void => "void",
            CortexValue::None => "none",
            CortexValue::Composite { field_values: _ } => "composite",
            CortexValue::Reference(_) => "pointer",
            CortexValue::List(_) => "list",
            CortexValue::Char(_) => "char",
        }
    }

    pub fn get_field(&self, field: &String) -> Result<Rc<RefCell<CortexValue>>, ValueError> {
        if let CortexValue::Composite { field_values } = self {
            if field_values.contains_key(field) {
                let val = field_values.get(field).unwrap().clone();
                Ok(val)
            } else {
                Err(ValueError::FieldDoesNotExist(field.clone(), String::from("<unknown>")))
            }
        } else {
            Err(ValueError::CannotAccessMemberOfNonComposite(self.get_variant_name()))
        }
    }
    pub fn set_field(&mut self, field: &String, value: CortexValue) -> Result<(), ValueError> {
        match self {
            CortexValue::Composite { field_values } => {
                if field_values.contains_key(field) {
                    field_values.insert(field.clone(), Rc::new(RefCell::new(value)));
                    Ok(())
                } else {
                    Err(ValueError::FieldDoesNotExist(field.clone(), String::from("<unknown>")))
                }
            },
            _ => Err(ValueError::CannotModifyFieldOnNonComposite),
        }
    }
}
