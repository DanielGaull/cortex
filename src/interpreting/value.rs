use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use thiserror::Error;

use crate::joint::vtable::VTable;

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

#[derive(Clone, Debug)]
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
    Fat(Box<CortexValue>, VTable),
}

impl PartialEq for CortexValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Composite { field_values: l_field_values }, Self::Composite { field_values: r_field_values }) => l_field_values == r_field_values,
            (Self::Reference(l0), Self::Reference(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Fat(l0, _), Self::Fat(r0, _)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
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
            CortexValue::Fat(cortex_value, _) => write!(f, "{}", cortex_value),
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

    // Should always return the underlying type. So for example with Fat pointers that wrap a value,
    // we should return the value underneath
    pub(crate) fn get_variant_name(&self) -> &'static str {
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
            CortexValue::Fat(cortex_value, _) => cortex_value.get_variant_name(),
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
