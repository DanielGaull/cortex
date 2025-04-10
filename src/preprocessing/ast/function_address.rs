use crate::parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen};

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
impl FunctionAddress {
    pub(crate) fn concat(prefix: &PathIdent, addr: &FunctionAddress) -> FunctionAddress {
        FunctionAddress {
            own_module_path: PathIdent::concat(prefix, &addr.own_module_path),
            target: addr.target.as_ref().map(|t| PathIdent::concat(prefix, t)),
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

    pub(crate) fn without_last(&self) -> PathIdent {
        self.own_module_path.without_last()
    }
    pub(crate) fn get_back(self) -> FunctionAddress {
        FunctionAddress {
            own_module_path: PathIdent::simple(self.own_module_path.get_back().unwrap().clone()),
            target: self.target.map(|t| PathIdent::simple(t.get_back().unwrap().clone())),
        }
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
