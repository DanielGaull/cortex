// Here's how vtables are contructed:
// If a type is `follows X`, then the methods of X are added in order to the vtable
// If a type is `follows X + Y`, then the methods of Y will appear after the methods of X 
// (and so on for adding more entries to the follows clause)

use crate::{parsing::ast::expression::PathIdent, preprocessing::ast::{function_address::FunctionAddress, r#type::RTypeArg}, r#type::r#type::TypeParam};

#[derive(Debug, Clone)]
pub struct VTable {
    functions: Vec<usize>,
}
impl VTable {
    pub fn new(functions: Vec<usize>) -> Self {
        Self {
            functions
        }
    }

    pub fn get(&self, index: usize) -> Option<usize> {
        self.functions.get(index).copied()
    }

    pub fn add(&mut self, address: usize) {
        self.functions.push(address);
    }
}

#[derive(Clone, Debug)]
pub struct GlobalVTableGenericRow {
    pub member_name: String,
    pub address: FunctionAddress,
}
impl GlobalVTableGenericRow {
    pub fn new(member_name: String, address: FunctionAddress) -> Self {
        Self {
            member_name,
            address,
        }
    }
}

pub struct GlobalVTableConcreteRow {
    pub member_name: String,
    pub address: FunctionAddress,
    pub type_args: Vec<RTypeArg>,
}
impl GlobalVTableConcreteRow {
    pub fn new(member_name: String, address: FunctionAddress, type_args: Vec<RTypeArg>) -> Self {
        Self {
            member_name,
            address,
            type_args,
        }
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub struct GlobalVTableKey {
    pub type_params: Vec<TypeParam>,
    pub contract_name: PathIdent,
    pub contract_type_args: Vec<RTypeArg>,
    pub follower_name: PathIdent,
    pub follower_type_args: Vec<RTypeArg>,
}
impl GlobalVTableKey {
    pub fn new(type_params: Vec<TypeParam>, contract_name: PathIdent, contract_type_args: Vec<RTypeArg>, 
        follower_name: PathIdent, follower_type_args: Vec<RTypeArg>) -> Self {
        Self {
            type_params,
            contract_name,
            contract_type_args,
            follower_name,
            follower_type_args,
        }
    }
}
