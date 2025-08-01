use std::{collections::HashMap, rc::Rc};

use crate::{interpreting::{env::Environment, error::CortexError, heap::Heap, value::CortexValue}, preprocessing::module::ModuleError, r#type::r#type::TypeParam};

use super::{expression::RExpression, function_address::FunctionAddress, statement::RStatement, top_level::RParameter, r#type::RType};

pub enum RBody {
    Native(Box<dyn Fn(&Environment, &mut Heap) -> Result<CortexValue, CortexError>>),
    Interpreted(RInterpretedBody),
}

#[derive(Clone)]
pub(crate) struct RFunctionSignature {
    pub(crate) params: Vec<RParameter>,
    pub(crate) return_type: RType,
    pub(crate) type_params: Vec<TypeParam>,
}

pub struct RInterpretedBody {
    pub(crate) statements: Vec<RStatement>,
    pub(crate) result: Option<RExpression>,
}
impl RInterpretedBody {
    pub(crate) fn new(statements: Vec<RStatement>, result: Option<RExpression>) -> Self {
        RInterpretedBody {
            statements,
            result,
        }
    }
}

pub struct RFunction {
    pub(crate) params: Vec<String>,
    pub(crate) body: RBody,
}
impl RFunction {
    pub(crate) fn new(params: Vec<String>, body: RBody) -> Self {
        RFunction {
            params,
            body,
        }
    }

    pub fn num_params(&self) -> usize {
        self.params.len()
    }
    pub fn get_param(&self, index: usize) -> Option<&String> {
        self.params.get(index)
    }
}

pub struct FunctionDict {
    all_functions: HashMap<FunctionAddress, Rc<RFunction>>,
    name_to_id: HashMap<FunctionAddress, usize>,
    id_to_name: HashMap<usize, FunctionAddress>,
    next_id: usize,
}
impl FunctionDict {
    pub fn new() -> Self {
        Self {
            all_functions: HashMap::new(),
            name_to_id: HashMap::new(),
            id_to_name: HashMap::new(),
            next_id: 0,
        }
    }

    pub(crate) fn add_function(&mut self, name: FunctionAddress, function: RFunction) {
        self.all_functions.insert(name, Rc::new(function));
    }
    pub(crate) fn add_call(&mut self, name: FunctionAddress) -> Result<usize, ModuleError> {
        if let Some(id) = self.name_to_id.get(&name) {
            Ok(*id)
        } else {
            self.name_to_id.insert(name.clone(), self.next_id);
            self.id_to_name.insert(self.next_id, name);
            let result = self.next_id;
            self.next_id += 1;
            Ok(result)
        }
    }

    pub(crate) fn get(&self, id: usize) -> Option<&Rc<RFunction>> {
        let name = self.id_to_name.get(&id)?;
        let func = self.all_functions.get(name)?;
        Some(func)
    }
}
