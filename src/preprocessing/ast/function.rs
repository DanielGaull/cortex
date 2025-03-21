use std::{collections::HashMap, rc::Rc};

use crate::{interpreting::{env::Environment, error::CortexError, value::CortexValue}, parsing::ast::expression::PathIdent};

use super::{expression::RExpression, statement::RStatement};

pub enum RBody {
    Native(Box<dyn Fn(&Environment) -> Result<CortexValue, CortexError>>),
    Interpreted(RInterpretedBody),
}

pub struct RInterpretedBody {
    statements: Vec<RStatement>,
    result: Option<RExpression>,
}

pub struct RFunction {
    params: Vec<String>,
    body: RBody,
}

pub struct FunctionDictBuilder {
    functions: HashMap<usize, RFunction>,
    name_mappings: HashMap<PathIdent, usize>,
    next_id: usize,
}
impl FunctionDictBuilder {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            name_mappings: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn add(&mut self, name: PathIdent, function: RFunction) {
        self.functions.insert(self.next_id, function);
        self.name_mappings.insert(name, self.next_id);
        self.next_id += 1;
    }
    pub fn get(&self, name: &PathIdent) -> Option<usize> {
        let id = self.name_mappings.get(name)?;
        Some(*id)
    }
    pub fn add_if_not_exists(&mut self, name: PathIdent, function: RFunction) {
        if let None = self.get(&name) {
            self.add(name, function);
        }
    }

    pub fn build(self) -> FunctionDict {
        FunctionDict::new(
            self.functions
                .into_iter()
                .map(|(k, v)| (k, Rc::new(v)))
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
