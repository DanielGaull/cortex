use std::{collections::HashMap, error::Error, rc::Rc};

use crate::{joint::vtable::VTable, parsing::{ast::{expression::{BinaryOperator, IdentExpression, OptionalIdentifier, PConditionBody, PExpression, PathIdent, UnaryOperator}, statement::{AssignmentName, DeclarationName, PStatement}, top_level::{BasicBody, Body, Contract, FunctionSignature, PFunction}}, codegen::r#trait::SimpleCodeGen}, r#type::{r#type::{CortexType, TupleType, TypeArg}, type_checking_env::TypeCheckingEnvironment, type_env::TypeEnvironment}};

use super::super::{ast::{expression::RExpression, function::{FunctionDict, RBody, RFunction, RInterpretedBody}, function_address::FunctionAddress, statement::{RConditionBody, RStatement}}, error::PreprocessingError, module::{Module, TypeDefinition}, program::Program};

type CortexError = Box<dyn Error>;
pub type CheckResult<T> = Result<(T, CortexType, Vec<RStatement>), CortexError>;

pub struct CortexPreprocessor {
    pub(super) current_env: Option<Box<TypeCheckingEnvironment>>,
    pub(super) current_context: PathIdent,
    pub(super) current_type_env: Option<Box<TypeEnvironment>>,
    pub(super) function_dict: FunctionDict,
    pub(super) function_signature_map: HashMap<FunctionAddress, FunctionSignature>,
    pub(super) type_map: HashMap<PathIdent, TypeDefinition>,
    loop_depth: u32,
    temp_num: usize,
    pub(super) contract_map: HashMap<PathIdent, Contract>,
    pub(super) imported_paths: Vec<PathIdent>,
    pub(super) imported_aliases: HashMap<String, PathIdent>,
}

impl CortexPreprocessor {
    pub fn new() -> Result<Self, CortexError> {
        let mut this = CortexPreprocessor {
            current_env: Some(Box::new(TypeCheckingEnvironment::base())),
            current_context: PathIdent::empty(),
            current_type_env: Some(Box::new(TypeEnvironment::base())),
            function_dict: FunctionDict::new(),
            function_signature_map: HashMap::new(),
            type_map: HashMap::new(),
            loop_depth: 0,
            temp_num: 0,
            contract_map: HashMap::new(),
            imported_paths: Vec::new(),
            imported_aliases: HashMap::new(),
        };

        macro_rules! add_core_type {
            ($name:literal) => {
                this.type_map.insert(PathIdent::simple(String::from($name)), TypeDefinition { fields: HashMap::new(), type_params: Vec::new(), followed_contracts: vec![] });
            }
        }

        add_core_type!("number");
        add_core_type!("bool");
        add_core_type!("string");
        add_core_type!("void");
        add_core_type!("none");

        let mut global_module = Module::new();
        Self::add_list_funcs(&mut global_module)?;
        Self::add_string_funcs(&mut global_module)?;
        Self::add_range_funcs(&mut global_module)?;

        this.register_module(&PathIdent::empty(), global_module)?;

        Ok(this)
    }

    pub(crate) fn get_function(&self, id: usize) -> Option<&Rc<RFunction>> {
        self.function_dict.get(id)
    }

    pub(crate) fn determine_type(&mut self, expr: PExpression) -> Result<CortexType, CortexError> {
        let (_, typ, _) = self.check_exp(expr, None)?;
        Ok(typ)
    }

    // When a value is assigned to a certain type, sometimes transformations are needed
    // (ex vtables); this is the function that takes care of that
    pub(super) fn assign_to(&mut self, base: RExpression, base_type: CortexType, typ: CortexType) -> Result<(RExpression, Vec<RStatement>), CortexError> {
        match (typ, base_type) {
            (CortexType::FollowsType(follows), ref other) if !matches!(other, CortexType::FollowsType(_)) => {
                let prefix = other.prefix();
                let mut func_addresses = Vec::new();
                for entry in follows.clause.contracts {
                    let contract = self.lookup_contract(&entry.name)?;
                    for sig in &contract.function_sigs {
                        if let OptionalIdentifier::Ident(fname) = &sig.name {
                            let addr = self.get_member_function_address(other, fname)?;
                            let extended_prefix = PathIdent::concat(&self.current_context, &prefix);
                            let full_path = FunctionAddress::concat(&extended_prefix, &addr);
                            func_addresses.push(full_path);
                        }
                    }
                }
    
                let mut vtable = VTable::new(vec![]);
                for addr in func_addresses {
                    let id = self.function_dict.add_call(addr)?;
                    vtable.add(id);
                }
    
                Ok((RExpression::MakeFat(Box::new(base), vtable), vec![]))
            },
            (CortexType::TupleType(t), base_type) => {
                let mut statements = Vec::new();
                let mut tuple_entries = Vec::new();
                let temp_tup = self.next_temp();
                self.current_env.as_mut().unwrap().add(temp_tup.clone(), base_type, true)?;
                statements.push(RStatement::VariableDeclaration {
                    name: temp_tup.clone(),
                    is_const: true,
                    initial_value: base,
                });
                for (i, entry) in t.types.into_iter().enumerate() {
                    let member_access_exp = PExpression::MemberAccess(
                        Box::new(PExpression::PathIdent(PathIdent::simple(temp_tup.clone()))),
                        format!("t{}", i)
                    );
                    let (member_access_exp, member_access_typ, st) = self.check_exp(member_access_exp, None)?;
                    statements.extend(st);
                    let (value, st) = self.assign_to(member_access_exp, member_access_typ, entry)?;
                    statements.extend(st);
                    tuple_entries.push(value);
                }
                Ok((RExpression::Tuple(tuple_entries), statements))
            },
            (_, _) => {
                Ok((base, vec![]))
            }
        }
    }

    pub fn preprocess(&mut self, body: BasicBody) -> Result<Program, CortexError> {
        let (body, _) = self.check_body(body, None)?;
        Ok(Program { code: body })
    }

    pub fn preprocess_function(&mut self, function: PFunction) -> Result<RFunction, CortexError> {
        let parent_env = self.current_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        let mut new_env = TypeCheckingEnvironment::new(*parent_env);
        let mut params = Vec::new();
        for p in function.params {
            let param_type = self.clean_type(p.typ.clone().with_prefix_if_not_core(&self.current_context))?;
            new_env.add(p.name.clone(), param_type, false)?;
            params.push(p.name);
        }
        self.current_env = Some(Box::new(new_env));

        let final_fn_body;
        match function.body {
            Body::Basic(body) => {
                let (new_body, body_type) = self.check_body(body, Some(function.return_type.clone()))?;
                let return_type = self.clean_type(function.return_type.clone().with_prefix_if_not_core(&self.current_context))?;
                if !body_type.is_subtype_of(&return_type, &self.type_map)? {
                    return Err(Box::new(PreprocessingError::ReturnTypeMismatch(return_type.codegen(0), body_type.codegen(0))));
                }
                final_fn_body = RBody::Interpreted(new_body);
            },
            Body::Native(native_body) => {
                final_fn_body = RBody::Native(native_body);
            },
        }

        self.current_env = Some(Box::new(self.current_env.take().unwrap().exit()?));

        Ok(RFunction::new(params, final_fn_body))
    }

    fn check_statement(&mut self, statement: PStatement) -> Result<Vec<RStatement>, CortexError> {
        let st_str = statement.codegen(0);
        match statement {
            PStatement::Expression(expression) => {
                let (exp, _, mut statements) = self.check_exp(expression, None)?;
                statements.push(RStatement::Expression(exp));
                Ok(statements)
            },
            PStatement::Throw(expression) => {
                if let Some(ex) = expression {
                    let (exp, _, mut statements) = self.check_exp(ex, None)?;
                    statements.push(RStatement::Throw(Some(exp)));
                    Ok(statements)
                } else {
                    Ok(vec![RStatement::Throw(None)])
                }
            },
            PStatement::VariableDeclaration { name, is_const, typ, initial_value } => {
                Ok(self.check_declaration_recursive(name, typ, is_const, initial_value, &st_str)?)
            },
            PStatement::Assignment { name, value } => {
                Ok(self.check_assignment_recursive(name, value, &st_str)?)
            },
            PStatement::WhileLoop(condition_body) => {
                let (cond, cond_type, mut cond_statements) = self.check_exp(condition_body.condition, Some(CortexType::boolean()))?;
                if !cond_type.is_subtype_of(&CortexType::boolean(), &self.type_map)? {
                    return Err(
                        Box::new(
                            PreprocessingError::MismatchedType(
                                String::from("bool"),
                                cond_type.codegen(0),
                                String::from("while condition"),
                                st_str,
                            )
                        )
                    );
                }

                self.loop_depth += 1;
                let (body, body_type) = self.check_body_and_handle_env(condition_body.body, Some(CortexType::void()))?;
                if !body_type.is_subtype_of(&CortexType::void(), &self.type_map)? {
                    return Err(
                        Box::new(
                            PreprocessingError::LoopCannotHaveReturnValue
                        )
                    );
                }
                self.loop_depth -= 1;

                cond_statements.push(RStatement::WhileLoop(RConditionBody::new(cond, body)));
                Ok(cond_statements)
            },
            PStatement::Break => {
                if self.loop_depth <= 0 {
                    Err(Box::new(PreprocessingError::BreakUsedInNonLoopContext))
                } else {
                    Ok(vec![RStatement::Break])
                }
            },
            PStatement::Continue => {
                if self.loop_depth <= 0 {
                    Err(Box::new(PreprocessingError::ContinueUsedInNonLoopContext))
                } else {
                    Ok(vec![RStatement::Continue])
                }
            },
        }
    }

    fn check_declaration(&mut self, name: OptionalIdentifier, typ: Option<CortexType>, is_const: bool, initial_value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            OptionalIdentifier::Ident(ident) => {
                let (assigned_exp, assigned_type, mut statements) = self.check_exp(initial_value, typ.clone())?;
                let type_of_var = if let Some(mut declared_type) = typ {
                    declared_type = self.clean_type(declared_type.with_prefix_if_not_core(&self.current_context))?;
                    if !assigned_type.is_subtype_of(&declared_type, &self.type_map)? {
                        return Err(
                            Box::new(
                                PreprocessingError::MismatchedType(
                                    declared_type.codegen(0),
                                    assigned_type.codegen(0),
                                    ident.clone(),
                                    st_str.clone(),
                                )
                            )
                        );
                    }
                    declared_type.clone()
                } else {
                    assigned_type.clone()
                };

                self.current_env.as_mut().unwrap().add(ident.clone(), type_of_var.clone(), is_const)?;

                let (initial_value, assign_st) = self.assign_to(assigned_exp, assigned_type, type_of_var)?;
                statements.extend(assign_st);
                statements.push(RStatement::VariableDeclaration { name: ident, is_const: is_const, initial_value, });
                Ok(statements)
            },
            OptionalIdentifier::Ignore => {
                let (exp, _, mut statements) = self.check_exp(initial_value, typ)?;
                statements.push(RStatement::Expression(exp));
                Ok(statements)
            },
        }
    }
    fn check_declaration_recursive(&mut self, name: DeclarationName, typ: Option<CortexType>, is_const: bool, initial_value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            DeclarationName::Single(name) => {
                Ok(self.check_declaration(name, typ, is_const, initial_value, &st_str)?)
            },
            DeclarationName::Tuple(names) => {
                let mut result = Vec::new();
                // We want to save off the initial expression in case it is an expensive calculation
                let temp_name = self.next_temp();
                // We need to run through this so that the preprocessor registers that the temp var was created
                let var_dec = self.check_statement(PStatement::VariableDeclaration {
                    name: DeclarationName::Single(OptionalIdentifier::Ident(temp_name.clone())),
                    is_const: true,
                    typ,
                    initial_value,
                })?;
                result.extend(var_dec);
                let temp_expr = PExpression::PathIdent(PathIdent::simple(temp_name));
                for (i, name) in names.into_iter().enumerate() {
                    let new_value = PExpression::MemberAccess(Box::new(temp_expr.clone()), format!("t{}", i));
                    result.extend(self.check_declaration_recursive(name, None, is_const, new_value, &st_str)?);
                }
                Ok(result)
            },
        }
    }

    fn check_assignment(&mut self, name: IdentExpression, value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        let type_of_var;
        let assigned_exp;
        let assigned_type;
        let mut statements: Vec<RStatement>;
        if name.is_simple() {
            let var_name = &name.base;
            let var_type = &self.current_env.as_ref().unwrap().get(var_name)?.clone();
            type_of_var = var_type.clone();
            let (aassigned_exp, aassigned_type, astatements) = self.check_exp(value, Some(type_of_var.clone()))?;
            assigned_exp = aassigned_exp;
            assigned_type = aassigned_type;
            statements = astatements;
            if !assigned_type.is_subtype_of(var_type, &self.type_map)? {
                return Err(
                    Box::new(
                        PreprocessingError::MismatchedType(
                            var_type.codegen(0),
                            assigned_type.codegen(0),
                            var_name.clone(),
                            st_str.clone(),
                        )
                    )
                );
            }
            
            if self.current_env.as_ref().unwrap().is_const(var_name)? {
                return Err(
                    Box::new(
                        PreprocessingError::CannotModifyConst(var_name.clone())
                    )
                )
            }
        } else {
            let source_expr = name.clone().without_last()?.to_member_access_expr();
            let (_, source_type, _) = self.check_exp(source_expr, None)?;
            if let CortexType::RefType(r) = source_type {
                if !r.mutable {
                    return Err(
                        Box::new(
                            PreprocessingError::CannotModifyFieldOnImmutableReference(r.contained.codegen(0))
                        )
                    );
                }
            }

            let name_expr = name.clone().to_member_access_expr();
            let (_, var_type, _) = self.check_exp(name_expr, None)?;
            type_of_var = var_type.clone();
            let (aassigned_exp, aassigned_type, astatements) = self.check_exp(value, Some(type_of_var.clone()))?;
            assigned_exp = aassigned_exp;
            assigned_type = aassigned_type;
            statements = astatements;
            if !assigned_type.is_subtype_of(&var_type, &self.type_map)? {
                return Err(
                    Box::new(
                        PreprocessingError::MismatchedType(
                            var_type.codegen(0),
                            assigned_type.codegen(0),
                            name.codegen(0),
                            st_str.clone(),
                        )
                    )
                );
            }
        }

        let (assigned_exp, assign_st) = self.assign_to(assigned_exp, assigned_type, type_of_var)?;
        statements.extend(assign_st);
        statements.push(RStatement::Assignment { name: name.into(), value: assigned_exp });
        Ok(statements)
    }
    fn check_assignment_recursive(&mut self, name: AssignmentName, value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            AssignmentName::Single(name) => {
                Ok(self.check_assignment(name, value, &st_str)?)
            },
            AssignmentName::Ignore => {
                Ok(self.check_statement(PStatement::Expression(value))?)
            },
            AssignmentName::Tuple(names) => {
                let mut result = Vec::new();
                // We want to save off the initial expression in case it is an expensive calculation
                let temp_name = self.next_temp();
                // We need to run through this so that the preprocessor registers that the temp var was created
                let var_dec = self.check_statement(PStatement::VariableDeclaration {
                    name: DeclarationName::Single(OptionalIdentifier::Ident(temp_name.clone())),
                    is_const: true,
                    typ: None,
                    initial_value: value,
                })?;
                result.extend(var_dec);
                let temp_expr = PExpression::PathIdent(PathIdent::simple(temp_name));
                for (i, name) in names.into_iter().enumerate() {
                    let new_value = PExpression::MemberAccess(Box::new(temp_expr.clone()), format!("t{}", i));
                    result.extend(self.check_assignment_recursive(name, new_value, &st_str)?);
                }
                Ok(result)
            },
        }
    }

    pub(super) fn check_exp(&mut self, exp: PExpression, expected_type: Option<CortexType>) -> CheckResult<RExpression> {
        let st_str = exp.codegen(0);
        match exp {
            PExpression::Number(v) => Ok((RExpression::Number(v), CortexType::number(), vec![])),
            PExpression::Boolean(v) => Ok((RExpression::Boolean(v), CortexType::boolean(), vec![])),
            PExpression::Void => Ok((RExpression::Void, CortexType::void(), vec![])),
            PExpression::None => Ok((RExpression::None, CortexType::none(), vec![])),
            PExpression::String(v) => Ok((RExpression::String(v), CortexType::string(), vec![])),
            PExpression::Char(v) => Ok((RExpression::Char(v), CortexType::char(), vec![])),
            PExpression::PathIdent(path_ident) => Ok((RExpression::Identifier(path_ident.get_back()?.clone()), self.get_variable_type(&path_ident)?, vec![])),
            PExpression::Call { name: addr, args: arg_exps, type_args } => {
                let prefix = addr.without_last();
                let result = self.check_call(
                    addr.get_back()?,
                    arg_exps, 
                    type_args,
                    prefix,
                    &st_str
                );
                result
            },
            PExpression::Construction { name, type_args, assignments } => {
                self.check_construction(name, type_args, assignments, &st_str)
            },
            PExpression::IfStatement { first, conds, last } => {
                self.check_if_statement(*first, conds, last.map(|b| *b), &st_str)
            },
            PExpression::UnaryOperation { op, exp } => {
                match op {
                    UnaryOperator::Negate => {
                        let (exp, typ, statements) = self.check_exp(*exp, expected_type)?;
                        if typ == CortexType::number() {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Negate, exp: Box::new(exp) }, CortexType::number(), statements))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("number", "-", typ.codegen(0))))
                        }
                    },
                    UnaryOperator::Invert => {
                        let (exp, typ, statements) = self.check_exp(*exp, expected_type)?;
                        if typ == CortexType::boolean() {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Invert, exp: Box::new(exp) }, CortexType::boolean(), statements))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("bool", "!", typ.codegen(0))))
                        }
                    },
                    UnaryOperator::Deref => {
                        let sub_expected_type = expected_type.map(|t| CortexType::reference(t, false));
                        let (exp, typ, statements) = self.check_exp(*exp, sub_expected_type)?;
                        if let CortexType::RefType(r) = typ {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Deref, exp: Box::new(exp) }, *r.contained, statements))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("&T", "@", typ.codegen(0))))
                        }
                    },
                }
            },
            PExpression::ListLiteral(items) => {
                let mut expected_internal = None;
                if let Some(expected) = expected_type {
                    if let CortexType::RefType(r) = expected {
                        if let CortexType::BasicType(mut b) = *r.contained {
                            if b.name.is_final() && b.name.get_back().unwrap() == "list" {
                                let type_arg = b.type_args.remove(0);
                                if let TypeArg::Ty(t) = type_arg {
                                    expected_internal = Some(t);
                                }
                            }
                        }
                    }
                }

                let mut contained_type = None;
                let mut new_items = Vec::new();
                let mut statements = Vec::new();
                for item in items {
                    let (item_exp, item_type, exp_st) = self.check_exp(item, expected_internal.clone())?;
                    statements.extend(exp_st);
                    let item_type_str = item_type.codegen(0);
                    if let None = contained_type {
                        contained_type = Some(item_type.clone());
                    } else if let Some(typ) = contained_type {
                        let typ_str = typ.codegen(0);
                        contained_type = Some(
                            typ
                                .combine_with(item_type, &self.type_map)
                                .ok_or(PreprocessingError::CannotDetermineListLiteralType(typ_str, item_type_str))?
                            );
                    }
                    new_items.push(item_exp);
                }
                if let Some(expected) = expected_internal {
                    if let Some(contained) = contained_type {
                        if contained.is_subtype_of(&expected, &self.type_map)? {
                            let true_type = CortexType::reference(CortexType::list(expected), true);
                            Ok((RExpression::ListLiteral(new_items), true_type, statements))
                        } else {
                            Err(Box::new(PreprocessingError::CannotDetermineType(st_str)))
                        }
                    } else {
                        let true_type = CortexType::reference(CortexType::list(expected), true);
                        Ok((RExpression::ListLiteral(new_items), true_type, statements))
                    }
                } else if let Some(contained) = contained_type {
                    let true_type = CortexType::reference(CortexType::list(contained), true);
                    Ok((RExpression::ListLiteral(new_items), true_type, statements))
                } else {
                    Err(Box::new(PreprocessingError::CannotDetermineType(st_str)))
                }
            },
            PExpression::Bang(inner) => {
                let (exp, typ, statements) = self.check_exp(*inner, expected_type.map(|t| t.to_optional()))?;
                Ok((RExpression::Bang(Box::new(exp)), typ.to_non_optional(), statements))
            },
            PExpression::MemberAccess(inner, member) => {
                let inner_as_string = inner.codegen(0);
                let (atom_exp, atom_type, mut statements) = self.check_exp(*inner, None)?;
                match &atom_type {
                    CortexType::BasicType(_) |
                    CortexType::RefType(_) | 
                    CortexType::GenericType(_) => {
                        if atom_type.is_non_composite() {
                            return Err(Box::new(PreprocessingError::CannotAccessMemberOfNonComposite));
                        }
                        if atom_type.optional() {
                            return Err(Box::new(PreprocessingError::CannotAccessMemberOfOptional(inner_as_string)));
                        }
                        let (ex, ty, st) = self.check_composite_member_access(atom_exp, atom_type, member)?;
                        statements.extend(st);
                        Ok((ex, ty, statements))
                    },
                    CortexType::TupleType(t) => {
                        let (ex, ty, st) = self.check_tuple_member_access(atom_exp, t, member)?;
                        statements.extend(st);
                        Ok((ex, ty, statements))
                    },
                    CortexType::FollowsType(_) => Err(Box::new(PreprocessingError::CannotAccessMemberOfFollowsType)),
                    CortexType::OptionalType(_) => Err(Box::new(PreprocessingError::CannotAccessMemberOfOptional(inner_as_string))),
                    CortexType::NoneType => Err(Box::new(PreprocessingError::CannotAccessMemberOfNonComposite)),
                }
            },
            PExpression::MemberCall { callee, member, args, type_args } => {
                let (_, atom_type, mut statements) = self.check_exp(*callee.clone(), None)?;
                
                if let CortexType::FollowsType(f) = atom_type {
                    let (ex, ty, st) = self.check_fat_member_call(f, callee, member, args, type_args, st_str)?;
                    statements.extend(st);
                    Ok((ex, ty, statements))
                } else {
                    let (ex, ty, st) = self.check_direct_member_call(atom_type, args, callee, member, type_args, st_str, expected_type)?;
                    statements.extend(st);
                    Ok((ex, ty, statements))
                }
            },
            PExpression::BinaryOperation { left, op, right } => {
                let (left_exp, left_type, st_left) = self.check_exp(*left, None)?;
                let (right_exp, right_type, st_right) = self.check_exp(*right, None)?;
                let op_type = self.check_operator(left_type, &op, right_type)?;
                let mut statements = Vec::new();
                statements.extend(st_left);
                statements.extend(st_right);
                Ok((RExpression::BinaryOperation { left: Box::new(left_exp), op: op, right: Box::new(right_exp) }, op_type, statements))
            },
            PExpression::Tuple(items) => {
                let expected_types;
                if let Some(CortexType::TupleType(t)) = expected_type {
                    if t.types.len() == items.len() {
                        expected_types = t.types.into_iter().map(|t| Some(t)).collect();
                    } else {
                        expected_types = vec![None; items.len()];
                    }
                } else {
                    expected_types = vec![None; items.len()];
                }
                
                let results = items
                    .into_iter()
                    .zip(expected_types)
                    .map(|(exp, expected)| self.check_exp(exp, expected))
                    .collect::<Result<Vec<_>, _>>()?;
                let (exps, types, statements): (Vec<RExpression>, Vec<CortexType>, Vec<Vec<RStatement>>) = unzip3(results);
                Ok((RExpression::Tuple(exps), CortexType::tuple(types), statements.into_iter().flatten().collect()))
            },
            PExpression::Range { start, end, step } => {
                fn otov(o: Option<f64>) -> RExpression {
                    match o {
                        Some(v) => RExpression::Number(v),
                        None => RExpression::None,
                    }
                }
                let construction = RExpression::Construction {
                    assignments: vec![
                        (String::from("start"), otov(start)),
                        (String::from("end"), otov(end)),
                        (String::from("step"), otov(step)),
                    ],
                };
                Ok((construction, CortexType::range(), vec![]))
            },
            PExpression::HeapAlloc(exp) => {
                let mut typ = None;
                if let Some(CortexType::RefType(contained)) = expected_type {
                    typ = Some(*contained.contained);
                }

                let (sub, typ, st) = self.check_exp(*exp, typ)?;

                Ok((RExpression::HeapAlloc(Box::new(sub)), CortexType::reference(typ, true), st))
            },
            PExpression::DerefFat(inner) => {
                let (exp, typ, st) = self.check_exp(*inner, expected_type)?;
                Ok((RExpression::DerefFat(Box::new(exp)), typ, st))
            }
        }
    }

    fn check_composite_member_access(&mut self, atom_exp: RExpression, atom_type: CortexType, member: String) -> CheckResult<RExpression> {
        let is_mutable;
        match &atom_type {
            CortexType::RefType(r) => {
                is_mutable = r.mutable;
            },
            _ => {
                is_mutable = true;
            },
        }
        let typedef = self.lookup_type(&atom_type.name()?.clone().subtract(&self.current_context)?)?;
        if !typedef.fields.contains_key(&member) {
            Err(Box::new(PreprocessingError::FieldDoesNotExist(member.clone(), atom_type.codegen(0))))
        } else {
            let mut member_type = typedef.fields.get(&member).unwrap().clone();
            let bindings = Self::get_bindings(&typedef.type_params, &atom_type)?;
            let prefix = atom_type.prefix();
            member_type = TypeEnvironment::fill_type(member_type, 
                &bindings
                    .into_iter()
                    .map(|(k, v)| (k, v.subtract_if_possible(&prefix)))
                    .collect::<HashMap<_, _>>()
                )?;
            member_type = member_type.with_prefix_if_not_core(&prefix);
            member_type = member_type.forward_immutability(is_mutable);
            Ok((RExpression::MemberAccess(Box::new(atom_exp), member), member_type, vec![]))
        }
    }
    fn check_tuple_member_access(&mut self, atom_exp: RExpression, atom_type: &TupleType, member: String) -> CheckResult<RExpression> {
        fn strip_t(s: &str) -> Option<usize> {
            s.strip_prefix('t')?.parse().ok()
        }

        let index = strip_t(&member).ok_or(PreprocessingError::TupleMemberSyntaxInvalid(member))?;
        if index > atom_type.types.len() {
            return Err(Box::new(PreprocessingError::TupleIndexValueInvalid(atom_type.types.len(), index)));
        }

        let member_type = atom_type.types.get(index).unwrap().clone();
        
        Ok((RExpression::MemberAccess(Box::new(atom_exp), format!("t{}", index)), member_type, vec![]))
    }

    fn check_construction(&mut self, name: PathIdent, type_args: Vec<TypeArg>, assignments: Vec<(String, PExpression)>, st_str: &String) -> CheckResult<RExpression> {
        let typedef = self.lookup_type(&name)?;
        let base_type = CortexType::basic(name.clone(), type_args.clone()).with_prefix_if_not_core(&self.current_context);

        if type_args.len() != typedef.type_params.len() {
            return Err(Box::new(PreprocessingError::MismatchedTypeArgCount(name.codegen(0), typedef.type_params.len(), type_args.len())));
        }
        let mut fields_to_assign = Vec::new();
        for k in typedef.fields.keys() {
            fields_to_assign.push(k.clone());
        }

        let fields = typedef.fields.clone();

        let bindings = TypeEnvironment::create_bindings(&typedef.type_params, &type_args);
        let bindings = bindings
            .iter()
            .map(|(k, v)| (k.clone(), v.clone().subtract_if_possible(&name.without_last())))
            .collect::<HashMap<_, _>>();
        let mut new_assignments = Vec::new();
        let mut statements = Vec::new();
        for (fname, fvalue) in assignments {
            let opt_typ = fields
                .get(&fname)
                .map(|t| t.clone());
            if let Some(typ) = opt_typ {
                let field_type = TypeEnvironment::fill_type(typ, &bindings)?
                    .with_prefix_if_not_core(&self.current_context)
                    .with_prefix_if_not_core(&name.without_last());
                let (exp, assigned_type, st) = self.check_exp(fvalue, Some(field_type.clone()))?;
                statements.extend(st);
                if !assigned_type.is_subtype_of(&field_type, &self.type_map)? {
                    return Err(
                        Box::new(
                            PreprocessingError::MismatchedType(
                                field_type.codegen(0),
                                assigned_type.codegen(0),
                                fname.clone(),
                                st_str.clone(),
                            )
                        )
                    );
                }

                let (exp, st) = self.assign_to(exp, assigned_type, field_type)?;
                statements.extend(st);

                new_assignments.push((fname.clone(), exp));

                let index_opt = fields_to_assign.iter().position(|x| *x == *fname);
                if let Some(index) = index_opt {
                    fields_to_assign.remove(index);
                } else {
                    return Err(Box::new(PreprocessingError::MultipleFieldAssignment(fname.clone())));
                }
            } else {
                return Err(Box::new(PreprocessingError::FieldDoesNotExist(fname.clone(), name.codegen(0))));
            }
        }

        if fields_to_assign.is_empty() {
            Ok((RExpression::Construction { assignments: new_assignments }, base_type, statements))
        } else {
            Err(Box::new(PreprocessingError::NotAllFieldsAssigned(name.codegen(0), fields_to_assign.join(","))))
        }
    }
    fn check_if_statement(&mut self, first: PConditionBody, conds: Vec<PConditionBody>, last: Option<BasicBody>, st_str: &String) -> CheckResult<RExpression> {
        let (cond_exp, cond_typ, mut pre_statements) = self.check_exp(first.condition, Some(CortexType::boolean()))?;
        if cond_typ != CortexType::boolean() {
            return Err(
                Box::new(
                    PreprocessingError::MismatchedType(
                        String::from("bool"),
                        cond_typ.codegen(0),
                        String::from("if condition"),
                        st_str.clone(),
                    )
                )
            );
        }
        let (first_body, mut the_type) = self.check_body_and_handle_env(first.body, None)?;

        let mut condition_bodies = Vec::<RConditionBody>::new();
        for c in conds {
            let (cond, cond_typ, cond_st) = self.check_exp(c.condition, Some(CortexType::boolean()))?;
            pre_statements.extend(cond_st);
            if !cond_typ.is_subtype_of(&CortexType::boolean(), &self.type_map)? {
                return Err(
                    Box::new(
                        PreprocessingError::MismatchedType(
                            String::from("bool"),
                            cond_typ.codegen(0),
                            String::from("else-if condition"),
                            st_str.clone(),
                        )
                    )
                );
            }
            let (body, typ) = self.check_body_and_handle_env(c.body, Some(the_type.clone()))?;
            let the_type_str = the_type.codegen(0);
            let typ_str = typ.codegen(0);
            let next = the_type.combine_with(typ, &self.type_map);
            if let Some(t) = next {
                the_type = t;
                condition_bodies.push(RConditionBody::new(cond, body));
            } else {
                return Err(Box::new(PreprocessingError::IfArmsDoNotMatch(the_type_str, typ_str)));
            }
        }
        let mut final_body = None;
        if let Some(fin) = last {
            let (body, typ) = self.check_body_and_handle_env(fin, Some(the_type.clone()))?;
            final_body = Some(Box::new(body));
            let the_type_str = the_type.codegen(0);
            let typ_str = typ.codegen(0);
            let next = the_type.combine_with(typ, &self.type_map);
            if let Some(t) = next {
                the_type = t;
            } else {
                return Err(Box::new(PreprocessingError::IfArmsDoNotMatch(the_type_str, typ_str)));
            }
        } else if the_type != CortexType::void() {
            return Err(Box::new(PreprocessingError::IfRequiresElseBlock));
        }

        Ok((RExpression::IfStatement { 
            first: Box::new(RConditionBody::new(cond_exp, first_body)),
            conds: condition_bodies,
            last: final_body,
        }, the_type, pre_statements))
    }

    fn check_body_and_handle_env(&mut self, body: BasicBody, expected_return_value: Option<CortexType>) -> Result<(RInterpretedBody, CortexType), CortexError> {
        let parent_env = self.current_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        self.current_env = Some(Box::new(TypeCheckingEnvironment::new(*parent_env)));

        let result = self.check_body(body, expected_return_value);

        self.current_env = Some(Box::new(self.current_env.take().unwrap().exit()?));
        result
    }

    fn check_body(&mut self, body: BasicBody, expected_return_value: Option<CortexType>) -> Result<(RInterpretedBody, CortexType), CortexError> {
        let mut statements = Vec::new();
        for st in body.statements {
            let s = self.check_statement(st)?;
            statements.extend(s);
        }
        if let Some(exp) = body.result {
            let (exp, typ, st) = self.check_exp(exp, expected_return_value)?;
            statements.extend(st);
            Ok((RInterpretedBody::new(statements, Some(exp)), typ))
        } else {
            Ok((RInterpretedBody::new(statements, None), CortexType::void()))
        }
    }

    fn check_operator(&self, first: CortexType, op: &BinaryOperator, second: CortexType) -> Result<CortexType, CortexError> {
        let number = CortexType::number();
        let string = CortexType::string();
        let boolean = CortexType::boolean();
        match op {
            BinaryOperator::Add => {
                if first == number && second == number {
                    Ok(number)
                } else if first == string && second == string {
                    Ok(string)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number, string", "number, string", "+", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::Subtract => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", "-", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::Multiply => {
                if first == number && second == number {
                    Ok(number)
                } else if first == number && second == string {
                    Ok(string)
                } else if first == string && second == number {
                    Ok(string)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "string", "*", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::Divide => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", "/", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::Remainder => {
                if first == number && second == number {
                    Ok(number)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", "%", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::LogicAnd => {
                if first == boolean && second == boolean {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("boolean", "boolean", "&&", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::LogicOr => {
                if first == boolean && second == boolean {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("boolean", "boolean", "||", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::IsEqual => {
                Ok(boolean)
            },
            BinaryOperator::IsNotEqual => {
                Ok(boolean)
            },
            BinaryOperator::IsLessThan => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", "<", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::IsGreaterThan => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", ">", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::IsLessThanOrEqualTo => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", "<=", first.codegen(0), second.codegen(0))))
                }
            },
            BinaryOperator::IsGreaterThanOrEqualTo => {
                if first == number && second == number {
                    Ok(boolean)
                } else {
                    Err(Box::new(PreprocessingError::InvalidOperator("number", "number", ">=", first.codegen(0), second.codegen(0))))
                }
            },
        }
    }

    // "Cleans" type, for example replacing type arguments
    pub(super) fn clean_type(&self, typ: CortexType) -> Result<CortexType, CortexError> {
        Ok(self.current_type_env.as_ref().unwrap().fill_in(typ)?)
    }

    fn get_variable_type(&self, path: &PathIdent) -> Result<CortexType, CortexError> {
        if path.is_final() {
            // Search in our environment for it
            let front = path.get_front()?;
            Ok(self.current_env.as_ref().unwrap().get(front)?.clone())
        } else {
            Err(Box::new(PreprocessingError::ValueNotFound(path.codegen(0))))
        }
    }

    fn next_temp(&mut self) -> String {
        let res = format!("$temp{}", self.temp_num);
        self.temp_num += 1;
        res
    }
}

fn unzip3<A, B, C>(input: Vec<(A, B, C)>) -> (Vec<A>, Vec<B>, Vec<C>) {
    let mut vec_a = Vec::with_capacity(input.len());
    let mut vec_b = Vec::with_capacity(input.len());
    let mut vec_c = Vec::with_capacity(input.len());

    for (a, b, c) in input {
        vec_a.push(a);
        vec_b.push(b);
        vec_c.push(c);
    }

    (vec_a, vec_b, vec_c)
}
