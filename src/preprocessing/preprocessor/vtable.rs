// Here's how vtables are contructed:
// If a type is `follows X`, then the methods of X are added in order to the vtable
// If a type is `follows X + Y`, then the methods of Y will appear after the methods of X 
// (and so on for adding more entries to the follows clause)

use std::collections::HashMap;

use crate::{parsing::ast::expression::PathIdent, preprocessing::ast::{function_address::FunctionAddress, r#type::{RType, RTypeArg}}, r#type::{r#type::TypeParam, type_env::TypeEnvironment}};

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

/**
 * Why do we need a custom struct?
 * Because you can have GlobalVTableKey != GlobalVTableKey
 * even when they are logically equal; because type param names
 * differ. So, we need this to handle that special case
 */
pub struct GlobalVTableMap<T> {
    keys: Vec<GlobalVTableKey>,
    values: Vec<T>,
}
impl<T> GlobalVTableMap<T> {
    pub fn new() -> Self {
        Self {
            keys: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: GlobalVTableKey, value: T) {
        let index = self.index(&key);
        if let Some(i) = index {
            self.values[i] = value;
        } else {
            self.keys.push(key);
            self.values.push(value);
        }
    }
    pub fn get(&self, key: &GlobalVTableKey) -> Option<&T> {
        let index = self.index(key)?;
        self.values.get(index)
    }
    fn index(&self, key: &GlobalVTableKey) -> Option<usize> {
        for (i, my_key) in self.keys.iter().enumerate() {
            if are_keys_equal(my_key, key) {
                return Some(i);
            }
        }
        None
    }
    pub fn contains(&self, key: &GlobalVTableKey) -> bool {
        self.index(key).is_some()
    }
}

fn are_keys_equal(k1: &GlobalVTableKey, k2: &GlobalVTableKey) -> bool {
    if k1.contract_name != k2.contract_name || 
        k1.follower_name != k2.follower_name ||
        k1.type_params.len() != k2.type_params.len() ||
        k1.contract_type_args.len() != k2.contract_type_args.len() ||
        k1.follower_type_args.len() != k2.follower_type_args.len()
    {
        return false;
    }

    let mut mappings = HashMap::new();
    for (t1, t2) in k1.type_params.iter().zip(&k2.type_params) {
        if t1.typ != t2.typ {
            return false;
        }
        mappings.insert(t1.clone(), RTypeArg::Ty(RType::GenericType(t2.name.clone())));
    }

    // Arbitrarily chosen, just need to get one's type args in terms of the other's type args
    let k1_contract_type_args = TypeEnvironment::fill_type_args(k1.contract_type_args.clone(), &mappings);
    let k1_follower_type_args = TypeEnvironment::fill_type_args(k1.follower_type_args.clone(), &mappings);
    
    k1_contract_type_args == k2.contract_type_args && k1_follower_type_args == k2.follower_type_args
}

#[cfg(test)]
mod tests {
    use crate::{parsing::ast::expression::PathIdent, preprocessing::ast::r#type::{RType, RTypeArg}, r#type::r#type::{TypeParam, TypeParamType}};

    use super::{are_keys_equal, GlobalVTableKey};

    fn assert_keys_equal(k1: GlobalVTableKey, k2: GlobalVTableKey) {
        let result = are_keys_equal(&k1, &k2);
        assert_eq!(true, result);
    }
    fn assert_keys_not_equal(k1: GlobalVTableKey, k2: GlobalVTableKey) {
        let result = are_keys_equal(&k1, &k2);
        assert_eq!(false, result);
    }

    #[test]
    fn trivial() {
        let k1 = GlobalVTableKey {
            type_params: vec![],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![],
        };
        let k2 = GlobalVTableKey {
            type_params: vec![],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![],
        };
        assert_keys_equal(k1, k2);
    }

    #[test]
    fn single_same_type_arg() {
        let k1 = GlobalVTableKey {
            type_params: vec![TypeParam::new("T", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("T")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![],
        };
        let k2 = GlobalVTableKey {
            type_params: vec![TypeParam::new("T", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("T")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![],
        };
        assert_keys_equal(k1, k2);
    }

    #[test]
    fn single_different_type_arg() {
        let k1 = GlobalVTableKey {
            type_params: vec![TypeParam::new("A", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("A")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![],
        };
        let k2 = GlobalVTableKey {
            type_params: vec![TypeParam::new("B", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("B")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![],
        };
        assert_keys_equal(k1, k2);
    }

    #[test]
    fn complex_different_type_arg() {
        let k1 = GlobalVTableKey {
            type_params: vec![TypeParam::new("A", TypeParamType::Ty), TypeParam::new("B", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("A")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![RTypeArg::Ty(RType::span(RType::GenericType(String::from("B"))))],
        };
        let k2 = GlobalVTableKey {
            type_params: vec![TypeParam::new("B", TypeParamType::Ty), TypeParam::new("C", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("B")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![RTypeArg::Ty(RType::span(RType::GenericType(String::from("C"))))],
        };
        assert_keys_equal(k1, k2);
    }

    #[test]
    fn complex_different_type_arg_not_equal() {
        let k1 = GlobalVTableKey {
            type_params: vec![TypeParam::new("A", TypeParamType::Ty), TypeParam::new("B", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("A")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![RTypeArg::Ty(RType::span(RType::GenericType(String::from("B"))))],
        };
        let k2 = GlobalVTableKey {
            type_params: vec![TypeParam::new("B", TypeParamType::Ty), TypeParam::new("A", TypeParamType::Ty)],
            contract_name: PathIdent::new(vec!["test", "contract"]),
            contract_type_args: vec![RTypeArg::Ty(RType::GenericType(String::from("A")))],
            follower_name: PathIdent::new(vec!["following", "type"]),
            follower_type_args: vec![RTypeArg::Ty(RType::span(RType::GenericType(String::from("B"))))],
        };
        assert_keys_not_equal(k1, k2);
    }
}
