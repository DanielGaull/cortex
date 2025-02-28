use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use thiserror::Error;

use crate::parsing::{ast::{expression::PathIdent, r#type::CortexType}, codegen::r#trait::SimpleCodeGen};

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
        field_values: HashMap<String, Rc<RefCell<CortexValue>>>,
    },
    Pointer(usize, CortexType),
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
                    s.push_str(&format!("{}", val.borrow()));
                    s.push_str(";");
                }
                write!(f, "{}({})", struct_name.codegen(0), s)
            },
            CortexValue::Pointer(addr, _) => write!(f, "&0x{:x}", addr),
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
            CortexValue::Pointer(_, typ) => typ.clone(),
        }
    }
    fn get_variant_name(&self) -> &'static str {
        match self {
            CortexValue::Number(_) => "number",
            CortexValue::Boolean(_) => "bool",
            CortexValue::String(_) => "string",
            CortexValue::Void => "void",
            CortexValue::Null => "null",
            CortexValue::Composite { struct_name: _, field_values: _ } => "composite",
            CortexValue::Pointer(_, _) => "pointer",
        }
    }

    pub fn get_field(&self, field: &String) -> Result<Rc<RefCell<CortexValue>>, ValueError> {
        if let CortexValue::Composite { struct_name, field_values } = self {
            if field_values.contains_key(field) {
                let val = field_values.get(field).unwrap().clone();
                Ok(val)
            } else {
                Err(ValueError::FieldDoesNotExist(field.clone(), struct_name.codegen(0)))
            }
        } else {
            Err(ValueError::CannotAccessMemberOfNonComposite(self.get_variant_name()))
        }
    }
    pub fn set_field(&mut self, field: &String, value: CortexValue) -> Result<(), ValueError> {
        match self {
            CortexValue::Composite { struct_name, field_values } => {
                if field_values.contains_key(field) {
                    field_values.insert(field.clone(), Rc::new(RefCell::new(value)));
                    Ok(())
                } else {
                    Err(ValueError::FieldDoesNotExist(field.clone(), struct_name.codegen(0)))
                }
            },
            _ => Err(ValueError::CannotModifyFieldOnNonComposite),
        }
    }
}
