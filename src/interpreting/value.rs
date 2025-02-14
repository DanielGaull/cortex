use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::parsing::{ast::{expression::PathIdent, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};

#[derive(Error, Debug, PartialEq)]
pub enum ValueError {
    #[error("Field {0} does not exist on struct {1}")]
    FieldDoesNotExist(String, String),
    #[error("You cannot modify fields on a non-composite value")]
    CannotModifyFieldOnNonComposite,
    #[error("You cannot access fields on a non-composite value")]
    CannotAccessMemberOfNonComposite,
    #[error("Member path cannot be empty")]
    MemberPathCannotBeEmpty,
}

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
            CortexValue::Composite { struct_name, field_values } => {
                let mut s = String::new();
                for (name, val) in field_values {
                    s.push_str(name);
                    s.push_str(":");
                    s.push_str(&format!("{}", val));
                    s.push_str(";");
                }
                write!(f, "{}({})", struct_name.codegen(0), s)
            },
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

    pub fn get_field_path(&self, mut path: Vec<String>) -> Result<CortexValue, ValueError> {
        let first_option = path.get(0);
        if let Some(first) = first_option {
            if path.len() == 1{
                self.get_field(first)
            } else {
                let fname = first.clone();
                path.remove(0);
                self.get_field(&fname)?.get_field_path(path)
            }
        } else {
            Ok(self.clone())
        }
    }
    pub fn set_field_path(&mut self, mut path: Vec<String>, value: CortexValue) -> Result<(), ValueError> {
        let first_option = path.get(0);
        if let Some(first) = first_option {
            if path.len() == 1{
                self.set_field(first, value)
            } else {
                let fname = first.clone();
                path.remove(0);
                self.get_field_mut(&fname)?.set_field_path(path, value)
            }
        } else {
            Err(ValueError::MemberPathCannotBeEmpty)
        }
    }

    fn get_field_mut(&mut self, field: &String) -> Result<&mut CortexValue, ValueError> {
        if let CortexValue::Composite { struct_name, field_values } = self {
            if field_values.contains_key(field) {
                let val = field_values.get_mut(field).unwrap();
                Ok(val)
            } else {
                Err(ValueError::FieldDoesNotExist(field.clone(), struct_name.codegen(0)))
            }
        } else {
            Err(ValueError::CannotAccessMemberOfNonComposite)
        }
    }

    pub fn get_field(&self, field: &String) -> Result<CortexValue, ValueError> {
        if let CortexValue::Composite { struct_name, field_values } = self {
            if field_values.contains_key(field) {
                let val = field_values.get(field).unwrap().clone();
                Ok(val)
            } else {
                Err(ValueError::FieldDoesNotExist(field.clone(), struct_name.codegen(0)))
            }
        } else {
            Err(ValueError::CannotAccessMemberOfNonComposite)
        }
    }
    pub fn set_field(&mut self, field: &String, value: CortexValue) -> Result<(), ValueError> {
        match self {
            CortexValue::Composite { struct_name, field_values } => {
                if field_values.contains_key(field) {
                    field_values.insert(field.clone(), value);
                    Ok(())
                } else {
                    Err(ValueError::FieldDoesNotExist(field.clone(), struct_name.codegen(0)))
                }
            },
            _ => Err(ValueError::CannotModifyFieldOnNonComposite),
        }
    }
}
