use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use thiserror::Error;

use crate::preprocessing::preprocessor::vtable::VTable;

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
    F32(f32),
    F64(f64),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    ISZ(isize),
    USZ(usize),
    Boolean(bool),
    String(String),
    Char(u8),
    Void,
    None,
    Composite {
        field_values: HashMap<String, Rc<RefCell<CortexValue>>>,
    },
    Reference(usize),
    Span(Vec<CortexValue>),
    Fat(Rc<RefCell<CortexValue>>, VTable),
    AnonymousBox(Box<CortexValue>),
    FunctionPointer(usize),
}
impl PartialEq for CortexValue {
    // Manually implemented purely because of fat pointers - the vtables do not have to be equal in that case
    // Otherwise, this is everything you'd get from #derive'ing PartialEq
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::F32(l0), Self::F32(r0)) => l0 == r0,
            (Self::F64(l0), Self::F64(r0)) => l0 == r0,
            (Self::I8(l0), Self::I8(r0)) => l0 == r0,
            (Self::U8(l0), Self::U8(r0)) => l0 == r0,
            (Self::I16(l0), Self::I16(r0)) => l0 == r0,
            (Self::U16(l0), Self::U16(r0)) => l0 == r0,
            (Self::I32(l0), Self::I32(r0)) => l0 == r0,
            (Self::U32(l0), Self::U32(r0)) => l0 == r0,
            (Self::I64(l0), Self::I64(r0)) => l0 == r0,
            (Self::U64(l0), Self::U64(r0)) => l0 == r0,
            (Self::ISZ(l0), Self::ISZ(r0)) => l0 == r0,
            (Self::USZ(l0), Self::USZ(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Composite { field_values: l_field_values }, Self::Composite { field_values: r_field_values }) => l_field_values == r_field_values,
            (Self::Reference(l0), Self::Reference(r0)) => l0 == r0,
            (Self::Span(l0), Self::Span(r0)) => l0 == r0,
            (Self::Fat(l0, _), Self::Fat(r0, _)) => l0 == r0,
            (Self::AnonymousBox(l0), Self::AnonymousBox(r0)) => l0 == r0,
            (Self::FunctionPointer(l0), Self::FunctionPointer(l1)) => l0 == l1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for CortexValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CortexValue::U8(v) => write!(f, "{}u8", v),
            CortexValue::I8(v) => write!(f, "{}i8", v),
            CortexValue::U16(v) => write!(f, "{}u16", v),
            CortexValue::I16(v) => write!(f, "{}i16", v),
            CortexValue::U32(v) => write!(f, "{}u32", v),
            CortexValue::I32(v) => write!(f, "{}", v),
            CortexValue::U64(v) => write!(f, "{}u64", v),
            CortexValue::I64(v) => write!(f, "{}i64", v),
            CortexValue::USZ(v) => write!(f, "{}usz", v),
            CortexValue::ISZ(v) => write!(f, "{}isz", v),
            CortexValue::F32(v) => write!(f, "{}f32", v),
            CortexValue::F64(v) => write!(f, "{}f64", v),
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
            CortexValue::Reference(addr) => write!(f, "ref(0x{:x})", addr),
            CortexValue::Span(list) => {
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
            CortexValue::Fat(v, _) => write!(f, "{}", *v.borrow()),
            CortexValue::AnonymousBox(v) => write!(f, "anonymous({})", *v),
            CortexValue::FunctionPointer(addr) => write!(f, "func(0x{:x})", addr),
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
            CortexValue::F32(_) => "f32",
            CortexValue::F64(_) => "f64",
            CortexValue::I8(_) => "i8",
            CortexValue::U8(_) => "u8",
            CortexValue::I16(_) => "i16",
            CortexValue::U16(_) => "u16",
            CortexValue::I32(_) => "i32",
            CortexValue::U32(_) => "u32",
            CortexValue::I64(_) => "i64",
            CortexValue::U64(_) => "u64",
            CortexValue::ISZ(_) => "isz",
            CortexValue::USZ(_) => "usz",
            CortexValue::Boolean(_) => "bool",
            CortexValue::String(_) => "string",
            CortexValue::Void => "void",
            CortexValue::None => "none",
            CortexValue::Composite { field_values: _ } => "composite",
            CortexValue::Reference(_) => "pointer",
            CortexValue::Span(_) => "list",
            CortexValue::Char(_) => "char",
            CortexValue::Fat(v, _) => v.borrow().get_variant_name(),
            CortexValue::AnonymousBox(..) => "anonymous box",
            CortexValue::FunctionPointer(..) => "function pointer",
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
