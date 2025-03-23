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

pub struct FunctionDictBuilder {
    all_functions: HashMap<PathIdent, RFunction>,
    name_mappings: HashMap<PathIdent, usize>,
    next_id: usize,
}
impl FunctionDictBuilder {
    pub fn new() -> Self {
        Self {
            all_functions: HashMap::new(),
            name_mappings: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn add_function(&mut self, name: PathIdent, function: RFunction) {
        self.all_functions.insert(name, function);
    }
    pub fn add_call(&mut self, name: PathIdent) -> usize {
        if let Some(id) = self.name_mappings.get(&name) {
            *id
        } else {
            self.name_mappings.insert(name.clone(), self.next_id);
            let result = self.next_id;
            self.next_id += 1;
            result
        }
    }

    pub fn build(mut self) -> FunctionDict {
        FunctionDict::new(
            self.name_mappings
                .into_iter()
                .map(|(k, v)| (v, Rc::new(self.all_functions.remove(&k).unwrap())))
                .collect()
        )
    }
}

pub struct FunctionDict {
    functions: HashMap<usize, Rc<RFunction>>,
}
impl FunctionDict {
    pub fn new(map: HashMap<usize, Rc<RFunction>>) -> Self {
        Self {
            functions: map,
        }
    }

    pub fn get(&self, id: usize) -> Option<&Rc<RFunction>> {
        self.functions.get(&id)
    }
}
