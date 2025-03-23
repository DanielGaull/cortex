use std::{collections::HashMap, rc::Rc};

use crate::{interpreting::{env::Environment, error::CortexError, heap::Heap, value::CortexValue}, parsing::ast::expression::PathIdent};

use super::{expression::RExpression, statement::RStatement};

pub enum RBody {
    Native(Box<dyn Fn(&Environment, &mut Heap) -> Result<CortexValue, CortexError>>),
    Interpreted(RInterpretedBody),
}

pub struct RInterpretedBody {
    statements: Vec<RStatement>,
    result: Option<RExpression>,
}
impl RInterpretedBody {
    pub(crate) fn empty() -> Self {
        RInterpretedBody {
            statements: Vec::new(),
            result: None
        }
    }
    pub(crate) fn new(statements: Vec<RStatement>, result: Option<RExpression>) -> Self {
        RInterpretedBody {
            statements,
            result,
        }
    }
}

pub struct RFunction {
    params: Vec<String>,
    body: RBody,
}
impl RFunction {
    pub(crate) fn new(params: Vec<String>, body: RBody) -> Self {
        RFunction {
            params,
            body,
        }
    }
}

pub struct FunctionDict {
    all_functions: HashMap<PathIdent, RFunction>,
    name_to_id: HashMap<PathIdent, usize>,
    id_to_name: HashMap<usize, PathIdent>,
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

    pub(crate) fn add_function(&mut self, name: PathIdent, function: RFunction) {
        self.all_functions.insert(name, function);
    }
    pub(crate) fn add_call(&mut self, name: PathIdent) -> usize {
        if let Some(id) = self.name_to_id.get(&name) {
            *id
        } else {
            self.name_to_id.insert(name.clone(), self.next_id);
            self.id_to_name.insert(self.next_id, name);
            let result = self.next_id;
            self.next_id += 1;
            result
        }
    }

    pub(crate) fn referenced_functions(&self) -> Vec<PathIdent> {
        self.name_to_id.keys().cloned().collect()
    }

    pub(crate) fn get(&self, id: usize) -> Option<&RFunction> {
        let name = self.id_to_name.get(&id)?;
        let func = self.all_functions.get(name)?;
        Some(func)
    }
}
