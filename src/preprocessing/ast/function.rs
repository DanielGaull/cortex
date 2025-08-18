use std::{collections::HashMap, rc::Rc};

use crate::{interpreting::{env::Environment, error::CortexError, heap::Heap, value::CortexValue}, parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen}, preprocessing::{error::PreprocessingError, module::ModuleError}, r#type::r#type::TypeParam};

use super::{expression::RExpression, function_address::FunctionAddress, statement::RStatement, top_level::RParameter, r#type::{RType, RTypeArg}};

pub enum RBody {
    Extern(Box<dyn Fn(&Environment, &mut Heap) -> Result<CortexValue, CortexError>>),
    Defined(RDefinedBody),
}

#[derive(Clone)]
pub(crate) struct RFunctionSignature {
    pub(crate) params: Vec<RParameter>,
    pub(crate) return_type: RType,
    pub(crate) type_params: Vec<TypeParam>,
}

#[derive(Clone)]
pub struct RDefinedBody {
    pub(crate) statements: Vec<RStatement>,
    pub(crate) result: Option<RExpression>,
}
impl RDefinedBody {
    pub(crate) fn new(statements: Vec<RStatement>, result: Option<RExpression>) -> Self {
        RDefinedBody {
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

// A function that *must* have code for its body
#[derive(Clone)]
pub struct DefinedFunction {
    pub(crate) params: Vec<String>,
    pub(crate) body: RDefinedBody,
}
impl DefinedFunction {
    pub(crate) fn new(params: Vec<String>, body: RDefinedBody) -> Self {
        DefinedFunction {
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
    // These are "real" - they are the concrete implementations, non-generic
    // Note that all generic functions, when monomorphized, will be *automatically* added to this list
    concrete_functions: HashMap<FunctionAddress, Rc<RFunction>>,
    // Reference to all generic functions - these are copied for 
    generic_functions: HashMap<FunctionAddress, DefinedFunction>,
    name_to_id: HashMap<FunctionAddress, usize>,
    id_to_name: HashMap<usize, FunctionAddress>,
    next_id: usize,
}
impl FunctionDict {
    pub fn new() -> Self {
        Self {
            concrete_functions: HashMap::new(),
            generic_functions: HashMap::new(),
            name_to_id: HashMap::new(),
            id_to_name: HashMap::new(),
            next_id: 0,
        }
    }

    pub(crate) fn get(&self, id: usize) -> Option<&Rc<RFunction>> {
        let name = self.id_to_name.get(&id)?;
        let func = self.concrete_functions.get(name)?;
        Some(func)
    }
    pub(crate) fn exists(&self, addr: &FunctionAddress) -> bool {
        self.concrete_functions.contains_key(addr) || self.generic_functions.contains_key(addr)
    }
    pub(crate) fn exists_concrete(&self, addr: &FunctionAddress) -> bool {
        self.concrete_functions.contains_key(addr)
    }

    pub(crate) fn add_function(&mut self, name: FunctionAddress, function: RFunction, type_params: Vec<TypeParam>) -> Result<(), CortexError> {
        if !type_params.is_empty() {
            match function.body {
                RBody::Extern(_) => Err(Box::new(PreprocessingError::GenericFunctionMustHaveABody)),
                RBody::Defined(body) => {
                    self.generic_functions.insert(name, DefinedFunction::new(function.params, body));
                    Ok(())
                },
            }
        } else {
            self.concrete_functions.insert(name, Rc::new(function));
            Ok(())
        }
    }
    pub(crate) fn add_call(&mut self, name: FunctionAddress, validated_type_args: Vec<RTypeArg>) -> Result<usize, ModuleError> {
        if validated_type_args.is_empty() {
            Ok(self.add_simple_call(name)?)
        } else {
            Ok(self.add_generic_call(name, validated_type_args)?)
        }
    }

    fn add_simple_call(&mut self, name: FunctionAddress) -> Result<usize, ModuleError> {
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
    fn add_generic_call(&mut self, name: FunctionAddress, validated_type_args: Vec<RTypeArg>) -> Result<usize, ModuleError> {
        let mangled = self.mangle(name.clone(), validated_type_args);
        if let Some(id) = self.name_to_id.get(&mangled) {
            Ok(*id)
        } else {
            // Only unique part from simple call - we need to generate the full implementation and save it
            let generic = self.generic_functions.get(&name);
            if let Some(func) = generic {
                let concrete = Rc::new(RFunction::new(func.params.clone(), RBody::Defined(func.body.clone())));
                self.concrete_functions.insert(mangled.clone(), concrete);
            } else {
                return Err(ModuleError::FunctionDoesNotExist(name.codegen(0)));
            }

            // Same as a simple call - mark that the call exists, and save off all the necessary values
            self.name_to_id.insert(mangled.clone(), self.next_id);
            self.id_to_name.insert(self.next_id, mangled);
            let result = self.next_id;
            self.next_id += 1;
            Ok(result)
        }
    }

    fn mangle(&self, name: FunctionAddress, type_args: Vec<RTypeArg>) -> FunctionAddress {
        let name_prefix = name.own_module_path.without_last();
        let mut name_parts = vec![name.own_module_path.get_back().unwrap().clone()];
        name_parts.extend(type_args
            .into_iter()
            .map(|t| 
                format!("{}{}", 
                match &t {
                    RTypeArg::Ty(..) => "t",
                    RTypeArg::Int(..) => "i",
                }, 
                t.codegen(0)
            ))
        );
        let full_name = PathIdent::concat(&name_prefix, &PathIdent::simple(name_parts.join("$")));
        FunctionAddress::new(full_name, name.target)
    }
}
