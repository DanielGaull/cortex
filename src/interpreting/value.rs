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
    #[error("Cannot modify member of non-mutable reference")]
    CannotModifyNonMutableReference,
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
        // Type args are ordered!!
        type_arg_names: Vec<String>,
        type_args: Vec<CortexType>,
    },
    Reference(usize, CortexType, bool), // address, type, mutable flag
    List(Vec<CortexValue>, CortexType),
}

impl Display for CortexValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CortexValue::Number(v) => write!(f, "{}", v),
            CortexValue::Boolean(v) => write!(f, "{}", v),
            CortexValue::String(v) => write!(f, "\"{}\"", v),
            CortexValue::Void => write!(f, "void"),
            CortexValue::Null => write!(f, "null"),
            CortexValue::Composite { struct_name, field_values, type_arg_names: _, type_args: _ } => {
                let mut s = String::new();
                for (name, val) in field_values {
                    s.push_str(name);
                    s.push_str(":");
                    s.push_str(&format!("{}", val.borrow()));
                    s.push_str(";");
                }
                write!(f, "{}({})", struct_name.codegen(0), s)
            },
            CortexValue::Reference(addr, _, mutable) => write!(f, "&{}0x{:x}", if *mutable {"mut "} else {""}, addr),
            CortexValue::List(list, _) => {
                let _ = write!(f, "[");
                for (i, item) in list.iter().enumerate() {
                    let _ = write!(f, "{}", item);
                    if i + 1 < list.len() {
                        let _ = write!(f, ", ");
                    }
                }
                write!(f, "]")
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
            CortexValue::Composite { struct_name, field_values: _, type_arg_names: _, type_args } => CortexType::basic(struct_name.clone(), false, type_args.clone()),
            CortexValue::Reference(_, typ, mutable) => CortexType::reference(typ.clone(), *mutable),
            CortexValue::List(_, typ) => CortexType::list(typ.clone(), false),
        }
    }
    fn get_variant_name(&self) -> &'static str {
        match self {
            CortexValue::Number(_) => "number",
            CortexValue::Boolean(_) => "bool",
            CortexValue::String(_) => "string",
            CortexValue::Void => "void",
            CortexValue::Null => "null",
            CortexValue::Composite { struct_name: _, field_values: _, type_args: _, type_arg_names: _ } => "composite",
            CortexValue::Reference(_, _, _) => "pointer",
            CortexValue::List(_, _) => "list",
        }
    }

    pub fn get_field(&self, field: &String) -> Result<Rc<RefCell<CortexValue>>, ValueError> {
        if let CortexValue::Composite { struct_name, field_values, type_arg_names: _, type_args: _ } = self {
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
            CortexValue::Composite { struct_name, field_values, type_arg_names: _, type_args: _ } => {
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

    // If this is mutable, and the value is false, then returns a value with mutable = false
    // Only has an effect on references
    pub fn forward_mutability(self, value: bool) -> Self {
        match self  {
            Self::Reference(addr, typ, mutable) => {
                let new_mutable = !mutable || value;
                Self::Reference(addr, typ, new_mutable)
            },
            _ => self
        }
    }
}
