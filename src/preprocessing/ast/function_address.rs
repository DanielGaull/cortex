use std::fmt::Debug;

use crate::parsing::{ast::expression::{PathError, PathIdent}, codegen::r#trait::SimpleCodeGen};

use super::r#type::is_path_a_core_type;

/// Represents an address/path to a function: so the key in a map to function signatures
/// If an extension/member function, then target will be set to Some, and point to the calling type
/// Otherwise, target will be None
/// So example Geometry::Point has a getX function, it will have path = Geometry::getX and target = Geometry::Point
/// Removes need for special paths such as "Point`getX"
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct FunctionAddress {
    pub(crate) own_module_path: PathIdent,
    pub(crate) target: Option<PathIdent>,
}
impl Debug for FunctionAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.codegen(0))
    }
}
impl FunctionAddress {
    pub(crate) fn concat(prefix: &PathIdent, addr: &FunctionAddress) -> FunctionAddress {
        let target = addr.target.as_ref().map(|t| if is_path_a_core_type(t) {
            t.clone()
        } else {
            PathIdent::concat(prefix, t)
        });

        FunctionAddress {
            own_module_path: PathIdent::concat(prefix, &addr.own_module_path),
            target,
        }
    }
    pub(crate) fn simple(path: PathIdent) -> FunctionAddress {
        FunctionAddress {
            own_module_path: path,
            target: None,
        }
    }
    pub(crate) fn member_func(path: PathIdent, target: PathIdent) -> FunctionAddress {
        FunctionAddress {
            own_module_path: path,
            target: Some(target),
        }
    }
    pub(crate) fn new(own_module_path: PathIdent, target: Option<PathIdent>) -> FunctionAddress {
        FunctionAddress {
            own_module_path,
            target,
        }
    }

    pub(crate) fn without_last(&self) -> PathIdent {
        self.own_module_path.without_last()
    }
    pub(crate) fn get_back(self) -> Result<FunctionAddress, PathError> {
        let prefix = self.own_module_path.without_last();
        let target = self.target.map(|t| {
            if is_path_a_core_type(&t) {
                Ok(t)
            } else {
                Ok(t.subtract(&prefix)?)
            }
        }).transpose()?;

        Ok(
            FunctionAddress {
                own_module_path: PathIdent::simple(self.own_module_path.get_back()?.clone()),
                target,
            }
        )
    }
}
impl SimpleCodeGen for FunctionAddress {
    fn codegen(&self, indent: usize) -> String {
        if let Some(target) = &self.target {
            format!("{} (on type {})", self.own_module_path.codegen(indent), target.codegen(indent))
        } else {
            self.own_module_path.codegen(indent)
        }
    }
}
