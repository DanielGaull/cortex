use std::{collections::HashMap, error::Error, rc::Rc};

use crate::{joint::vtable::VTable, parsing::{ast::{expression::{BinaryOperator, IdentExpression, OptionalIdentifier, PConditionBody, PExpression, PathIdent, UnaryOperator}, statement::{AssignmentName, DeclarationName, PStatement}, top_level::{BasicBody, Body, PFunction}}, codegen::r#trait::SimpleCodeGen}, preprocessing::ast::{function::RFunctionSignature, top_level::RContract, r#type::{RFollowsClause, RFollowsEntry, RType, RTypeArg}}, r#type::{r#type::{CortexType, FollowsClause, FollowsEntry, FollowsType, TypeArg, TypeParam}, type_checking_env::TypeCheckingEnvironment, type_env::TypeEnvironment}};

use super::super::{ast::{expression::RExpression, function::{FunctionDict, RBody, RFunction, RDefinedBody}, function_address::FunctionAddress, statement::{RConditionBody, RStatement}}, error::PreprocessingError, module::{Module, TypeDefinition}, program::Program};

type CortexError = Box<dyn Error>;
pub type CheckResult<T> = Result<(T, RType, Vec<RStatement>), CortexError>;

pub struct CortexPreprocessor {
    pub(super) current_env: Option<Box<TypeCheckingEnvironment<RType>>>,
    pub(super) current_context: PathIdent,
    pub(super) current_type_env: Option<Box<TypeEnvironment>>,
    pub(super) function_dict: FunctionDict,
    pub(super) function_signature_map: HashMap<FunctionAddress, RFunctionSignature>,
    pub(super) type_map: HashMap<PathIdent, TypeDefinition>,
    loop_depth: u32,
    temp_num: usize,
    pub(super) contract_map: HashMap<PathIdent, RContract>,
    pub(super) imported_paths: Vec<PathIdent>,
    pub(super) imported_aliases: HashMap<String, PathIdent>,

    pub(super) stubbed_functions: HashMap<FunctionAddress, Vec<TypeParam>>,
    pub(super) stubbed_structs: HashMap<PathIdent, Vec<TypeParam>>,
    pub(super) stubbed_contracts: HashMap<PathIdent, Vec<TypeParam>>,
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
            stubbed_functions: HashMap::new(),
            stubbed_structs: HashMap::new(),
            stubbed_contracts: HashMap::new(),
        };

        macro_rules! add_core_type {
            ($name:literal) => {
                let path = PathIdent::simple(String::from($name));
                this.type_map.insert(path.clone(), TypeDefinition { fields: HashMap::new(), type_params: Vec::new(), followed_contracts: vec![] });
                this.stubbed_structs.insert(path, vec![]);
            }
        }

        add_core_type!("number");
        add_core_type!("bool");
        add_core_type!("string");
        add_core_type!("void");
        add_core_type!("none");
        add_core_type!("char");
        add_core_type!("anonbox");
        // NOTE: range is added by Self::add_range_funcs (since it can operate as a struct... maybe add to stdlib?)

        let span_path = PathIdent::simple(String::from("span"));
        let span_type_params = vec![TypeParam::ty("T")];
        this.type_map.insert(span_path.clone(), TypeDefinition {
            fields: HashMap::new(),
            type_params: span_type_params.clone(),
            followed_contracts: vec![],
        });
        this.stubbed_structs.insert(span_path, span_type_params);

        let list_path = PathIdent::simple(String::from("list"));
        let list_type_params = vec![TypeParam::ty("T")];
        this.type_map.insert(list_path.clone(), TypeDefinition {
            fields: HashMap::new(),
            type_params: list_type_params.clone(),
            followed_contracts: vec![],
        });
        this.stubbed_structs.insert(list_path, list_type_params);

        let mut global_module = Module::new();
        Self::add_corelib(&mut global_module)?;
        // Self::add_list_funcs(&mut global_module)?;
        Self::add_string_funcs(&mut global_module)?;
        Self::add_range_struct(&mut global_module)?;

        this.register_module(&PathIdent::empty(), global_module)?;

        Ok(this)
    }

    pub(crate) fn get_function(&self, id: usize) -> Option<&Rc<RFunction>> {
        self.function_dict.get(id)
    }

    pub(crate) fn determine_type(&mut self, expr: PExpression) -> Result<RType, CortexError> {
        let (_, typ, _) = self.check_exp(expr, None)?;
        Ok(typ)
    }

    pub(crate) fn reset_imports(&mut self) {
        self.imported_aliases.clear();
        self.imported_paths.clear();
    }

    // When a value is assigned to a certain type, sometimes transformations are needed
    // (ex vtables); this is the function that takes care of that
    pub(super) fn assign_to(&mut self, base: RExpression, base_type: RType, typ: RType) -> Result<(RExpression, Vec<RStatement>), CortexError> {
        match (typ, base_type) {
            (RType::FollowsType(follows), ref other) if !matches!(other, RType::FollowsType(_)) => {
                let mut func_addresses = Vec::new();
                for entry in follows.entries {
                    let contract = self.lookup_contract(&entry.name)?;
                    for sig in &contract.function_sigs {
                        let addr = self.get_member_function_address(other, &sig.name)?;
                        func_addresses.push(addr);
                    }
                }
    
                let mut vtable = VTable::new(vec![]);
                for addr in func_addresses {
                    let id = self.function_dict.add_call(addr, todo!())?;
                    vtable.add(id);
                }
    
                Ok((RExpression::MakeFat(Box::new(base), vtable), vec![]))
            },
            (RType::TupleType(t), base_type) => {
                let mut statements = Vec::new();
                let mut tuple_entries = Vec::new();
                let temp_tup = self.next_temp();
                self.current_env.as_mut().unwrap().add(temp_tup.clone(), base_type, true)?;
                statements.push(RStatement::VariableDeclaration {
                    name: temp_tup.clone(),
                    is_const: true,
                    initial_value: base,
                });
                for (i, entry) in t.into_iter().enumerate() {
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
            let validated_type = self.validate_type(p.typ.clone())?;
            let param_type = self.clean_type(validated_type)?;
            new_env.add(p.name.clone(), param_type, false)?;
            params.push(p.name);
        }
        self.current_env = Some(Box::new(new_env));

        let final_fn_body;
        match function.body {
            Body::Basic(body) => {
                let return_type = self.validate_type(function.return_type.clone())?;
                let (new_body, body_type) = self.check_body(body, Some(return_type.clone()))?;
                let return_type = self.clean_type(return_type)?;
                if !self.is_subtype(&body_type, &return_type)? {
                    return Err(Box::new(PreprocessingError::ReturnTypeMismatch(return_type.codegen(0), body_type.codegen(0))));
                }
                final_fn_body = RBody::Defined(new_body);
            },
            Body::Native(native_body) => {
                final_fn_body = RBody::Extern(native_body);
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
                let typ = if let Some(typ) = typ {
                    Some(self.validate_type(typ)?)
                } else {
                    None
                };
                Ok(self.check_declaration_recursive(name, typ, is_const, initial_value, &st_str)?)
            },
            PStatement::Assignment { name, value } => {
                Ok(self.check_assignment_recursive(name, value, &st_str)?)
            },
            PStatement::WhileLoop(condition_body) => {
                let (cond, cond_type, mut cond_statements) = self.check_exp(condition_body.condition, Some(RType::boolean()))?;
                if !self.is_subtype(&cond_type, &RType::boolean())? {
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
                let (body, body_type) = self.check_body_and_handle_env(condition_body.body, Some(RType::void()))?;
                if !self.is_subtype(&body_type, &RType::void())? {
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

    fn check_declaration(&mut self, name: OptionalIdentifier, typ: Option<RType>, is_const: bool, initial_value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
        match name {
            OptionalIdentifier::Ident(ident) => {
                let (assigned_exp, assigned_type, mut statements) = self.check_exp(initial_value, typ.clone())?;
                let type_of_var = if let Some(mut declared_type) = typ {
                    declared_type = self.clean_type(declared_type)?;
                    if !self.is_subtype(&assigned_type, &declared_type)? {
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
    fn check_declaration_recursive(&mut self, name: DeclarationName, typ: Option<RType>, is_const: bool, initial_value: PExpression, st_str: &String) -> Result<Vec<RStatement>, CortexError> {
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
                    typ: typ.map(|t| self.devalidate_type(t)),
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
            if !self.is_subtype(&assigned_type, var_type)? {
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
            if let RType::RefType(r, mutable) = source_type {
                if !mutable {
                    return Err(
                        Box::new(
                            PreprocessingError::CannotModifyFieldOnImmutableReference(r.codegen(0))
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
            if !self.is_subtype(&assigned_type, &var_type)? {
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

    pub(super) fn check_exp(&mut self, exp: PExpression, expected_type: Option<RType>) -> CheckResult<RExpression> {
        let st_str = exp.codegen(0);
        match exp {
            PExpression::Number(v) => Ok((RExpression::Number(v), RType::number(), vec![])),
            PExpression::Boolean(v) => Ok((RExpression::Boolean(v), RType::boolean(), vec![])),
            PExpression::Void => Ok((RExpression::Void, RType::void(), vec![])),
            PExpression::None => Ok((RExpression::None, RType::none(), vec![])),
            PExpression::String(v) => Ok((RExpression::String(v), RType::string(), vec![])),
            PExpression::Char(v) => Ok((RExpression::Char(v), RType::char(), vec![])),
            PExpression::PathIdent(path_ident) => Ok((RExpression::Identifier(path_ident.get_back()?.clone()), self.get_variable_type(&path_ident)?, vec![])),
            PExpression::Call { name: addr, args: arg_exps, type_args } => {
                let result = self.check_call(
                    addr,
                    arg_exps, 
                    type_args,
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
                        if typ == RType::number() {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Negate, exp: Box::new(exp) }, RType::number(), statements))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("number", "-", typ.codegen(0))))
                        }
                    },
                    UnaryOperator::Invert => {
                        let (exp, typ, statements) = self.check_exp(*exp, expected_type)?;
                        if typ == RType::boolean() {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Invert, exp: Box::new(exp) }, RType::boolean(), statements))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("bool", "!", typ.codegen(0))))
                        }
                    },
                    UnaryOperator::Deref => {
                        let sub_expected_type = expected_type.map(|t| RType::reference(t, false));
                        let (exp, typ, statements) = self.check_exp(*exp, sub_expected_type)?;
                        if let RType::RefType(r, ..) = typ {
                            Ok((RExpression::UnaryOperation { op: UnaryOperator::Deref, exp: Box::new(exp) }, *r, statements))
                        } else {
                            Err(Box::new(PreprocessingError::InvalidOperatorUnary("&T", "@", typ.codegen(0))))
                        }
                    },
                }
            },
            PExpression::CollectionLiteral(items) => {
                let mut expected_internal = None;
                if let Some(expected) = expected_type {
                    if let RType::RefType(r, ..) = expected {
                        if let RType::BasicType(name, mut type_args) = *r {
                            if name.is_final() && name.get_back().unwrap() == "span" {
                                let type_arg = type_args.remove(0);
                                if let RTypeArg::Ty(t) = type_arg {
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
                            self.combine_types(typ, item_type)?
                                .ok_or(PreprocessingError::CannotDetermineCollectionLiteralType(typ_str, item_type_str))?
                            );
                    }
                    new_items.push(item_exp);
                }
                if let Some(expected) = expected_internal {
                    if let Some(contained) = contained_type {
                        if self.is_subtype(&contained, &expected)? {
                            let true_type = RType::span(expected);
                            Ok((RExpression::CollectionLiteral(new_items), true_type, statements))
                        } else {
                            Err(Box::new(PreprocessingError::CannotDetermineType(st_str)))
                        }
                    } else {
                        let true_type = RType::span(expected);
                        Ok((RExpression::CollectionLiteral(new_items), true_type, statements))
                    }
                } else if let Some(contained) = contained_type {
                    let true_type = RType::span(contained);
                    Ok((RExpression::CollectionLiteral(new_items), true_type, statements))
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
                    RType::BasicType(..) |
                    RType::RefType(..) | 
                    RType::GenericType(..) => {
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
                    RType::TupleType(t) => {
                        let (ex, ty, st) = self.check_tuple_member_access(atom_exp, t, member)?;
                        statements.extend(st);
                        Ok((ex, ty, statements))
                    },
                    RType::FollowsType(..) => Err(Box::new(PreprocessingError::CannotAccessMemberOfFollowsType)),
                    RType::OptionalType(..) => Err(Box::new(PreprocessingError::CannotAccessMemberOfOptional(inner_as_string))),
                    RType::NoneType => Err(Box::new(PreprocessingError::CannotAccessMemberOfNonComposite)),
                }
            },
            PExpression::MemberCall { callee, member, args, type_args } => {
                let (_, atom_type, mut statements) = self.check_exp(*callee.clone(), None)?;
                
                if let RType::FollowsType(f) = atom_type {
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
                if let Some(RType::TupleType(t)) = expected_type {
                    if t.len() == items.len() {
                        expected_types = t.into_iter().map(|t| Some(t)).collect();
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
                let (exps, types, statements): (Vec<RExpression>, Vec<RType>, Vec<Vec<RStatement>>) = unzip3(results);
                Ok((RExpression::Tuple(exps), RType::TupleType(types), statements.into_iter().flatten().collect()))
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
                Ok((construction, RType::range(), vec![]))
            },
            PExpression::HeapAlloc(exp) => {
                let mut typ = None;
                if let Some(RType::RefType(contained, _)) = expected_type {
                    typ = Some(*contained);
                }

                let (sub, typ, st) = self.check_exp(*exp, typ)?;

                Ok((RExpression::HeapAlloc(Box::new(sub)), RType::reference(typ, true), st))
            },
            PExpression::DerefFat(inner) => {
                let (exp, typ, st) = self.check_exp(*inner, expected_type)?;
                Ok((RExpression::DerefFat(Box::new(exp)), typ, st))
            },
            PExpression::MakeAnon(inner) => {
                let (inner, _, st) = self.check_exp(*inner, None)?;
                Ok((RExpression::MakeAnon(Box::new(inner)), RType::anonbox(), st))
            },
        }
    }

    fn check_composite_member_access(&mut self, atom_exp: RExpression, atom_type: RType, member: String) -> CheckResult<RExpression> {
        let is_mutable;
        match &atom_type {
            RType::RefType(_, mutable) => {
                is_mutable = *mutable;
            },
            _ => {
                is_mutable = true;
            },
        }
        // We can look up the type directly here, we will always have the full name already!
        let full_path = atom_type.name()?.clone();
        let typedef = if let Some(c) = self.type_map.get(&full_path) {
            Ok(c)
        } else {
            Err(Box::new(PreprocessingError::TypeDoesNotExist(full_path.codegen(0))))
        }?;
        if !typedef.fields.contains_key(&member) {
            Err(Box::new(PreprocessingError::FieldDoesNotExist(member.clone(), atom_type.codegen(0))))
        } else {
            let mut member_type = typedef.fields.get(&member).unwrap().clone();
            let bindings = Self::get_bindings(&typedef.type_params, &atom_type)?;
            member_type = TypeEnvironment::fill_type(member_type, &bindings)?;
            member_type = member_type.forward_immutability(is_mutable);
            Ok((RExpression::MemberAccess(Box::new(atom_exp), member), member_type, vec![]))
        }
    }
    fn check_tuple_member_access(&mut self, atom_exp: RExpression, atom_type: &Vec<RType>, member: String) -> CheckResult<RExpression> {
        fn strip_t(s: &str) -> Option<usize> {
            s.strip_prefix('t')?.parse().ok()
        }

        let index = strip_t(&member).ok_or(PreprocessingError::TupleMemberSyntaxInvalid(member))?;
        if index > atom_type.len() {
            return Err(Box::new(PreprocessingError::TupleIndexValueInvalid(atom_type.len(), index)));
        }

        let member_type = atom_type.get(index).unwrap().clone();
        
        Ok((RExpression::MemberAccess(Box::new(atom_exp), format!("t{}", index)), member_type, vec![]))
    }

    fn check_construction(&mut self, name: PathIdent, type_args: Vec<TypeArg>, assignments: Vec<(String, PExpression)>, st_str: &String) -> CheckResult<RExpression> {
        let (type_params, true_path) = self.get_struct_stub(&name).ok_or(PreprocessingError::TypeDoesNotExist(name.codegen(0)))?;
        let type_args = self.validate_type_args(&type_params, type_args, true_path.codegen(0), "Type")?;
        let base_type = RType::basic(true_path.clone(), type_args.clone());
        let typedef = self.lookup_type(&name)?;

        let mut fields_to_assign = Vec::new();
        for k in typedef.fields.keys() {
            fields_to_assign.push(k.clone());
        }

        let fields = typedef.fields.clone();

        let bindings = TypeEnvironment::create_bindings(&typedef.type_params, &type_args);
        let mut new_assignments = Vec::new();
        let mut statements = Vec::new();
        for (fname, fvalue) in assignments {
            let opt_typ = fields
                .get(&fname)
                .map(|t| t.clone());
            if let Some(typ) = opt_typ {
                let field_type = TypeEnvironment::fill_type(typ, &bindings)?;
                let (exp, assigned_type, st) = self.check_exp(fvalue, Some(field_type.clone()))?;
                statements.extend(st);
                if !self.is_subtype(&assigned_type, &field_type)? {
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
        let (cond_exp, cond_typ, mut pre_statements) = self.check_exp(first.condition, Some(RType::boolean()))?;
        if cond_typ != RType::boolean() {
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
            let (cond, cond_typ, cond_st) = self.check_exp(c.condition, Some(RType::boolean()))?;
            pre_statements.extend(cond_st);
            if !self.is_subtype(&cond_typ, &RType::boolean())? {
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
            let next = self.combine_types(the_type, typ)?;
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
            let next = self.combine_types(the_type, typ)?;
            if let Some(t) = next {
                the_type = t;
            } else {
                return Err(Box::new(PreprocessingError::IfArmsDoNotMatch(the_type_str, typ_str)));
            }
        } else if the_type != RType::void() {
            return Err(Box::new(PreprocessingError::IfRequiresElseBlock));
        }

        Ok((RExpression::IfStatement { 
            first: Box::new(RConditionBody::new(cond_exp, first_body)),
            conds: condition_bodies,
            last: final_body,
        }, the_type, pre_statements))
    }

    fn check_body_and_handle_env(&mut self, body: BasicBody, expected_return_value: Option<RType>) -> Result<(RDefinedBody, RType), CortexError> {
        let parent_env = self.current_env.take().ok_or(PreprocessingError::NoParentEnv)?;
        self.current_env = Some(Box::new(TypeCheckingEnvironment::new(*parent_env)));

        let result = self.check_body(body, expected_return_value);

        self.current_env = Some(Box::new(self.current_env.take().unwrap().exit()?));
        result
    }

    fn check_body(&mut self, body: BasicBody, expected_return_value: Option<RType>) -> Result<(RDefinedBody, RType), CortexError> {
        let mut statements = Vec::new();
        for st in body.statements {
            let s = self.check_statement(st)?;
            statements.extend(s);
        }
        if let Some(exp) = body.result {
            let (exp, typ, st) = self.check_exp(exp, expected_return_value)?;
            statements.extend(st);
            Ok((RDefinedBody::new(statements, Some(exp)), typ))
        } else {
            Ok((RDefinedBody::new(statements, None), RType::void()))
        }
    }

    fn check_operator(&self, first: RType, op: &BinaryOperator, second: RType) -> Result<RType, CortexError> {
        let number = RType::number();
        let string = RType::string();
        let boolean = RType::boolean();
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
    pub(super) fn clean_type(&self, typ: RType) -> Result<RType, CortexError> {
        Ok(self.current_type_env.as_ref().unwrap().fill_in(typ)?)
    }

    fn get_variable_type(&self, path: &PathIdent) -> Result<RType, CortexError> {
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

    pub fn validate_type(&self, typ: CortexType) -> Result<RType, CortexError> {
        match typ {
            CortexType::BasicType(b) => {
                let result = self.get_struct_stub(&b.name);
                if result.is_none() {
                    return Err(Box::new(PreprocessingError::TypeDoesNotExist(b.name.codegen(0))));
                }
                let (type_params, path) = result.unwrap();
                let type_args = self.validate_type_args(&type_params, b.type_args, path.codegen(0), "Type")?;

                Ok(RType::BasicType(
                    path,
                    type_args,
                ))
            },
            CortexType::RefType(r) => Ok(RType::RefType(
                Box::new(self.validate_type(*r.contained)?),
                r.mutable,
            )),
            CortexType::TupleType(t) => {
                let mut types = Vec::new();
                for ty in t.types {
                    types.push(self.validate_type(ty)?);
                }
                Ok(RType::TupleType(types))
            },
            CortexType::FollowsType(f) => {
                let mut entries = Vec::new();
                for entry in f.clause.contracts {
                    let result = self.get_contract_stub(&entry.name);
                    if result.is_none() {
                        return Err(Box::new(PreprocessingError::ContractDoesNotExist(entry.name.codegen(0))));
                    }
                    let (contract_type_params, path) = result.unwrap();
                    let type_args = self.validate_type_args(contract_type_params, entry.type_args, path.codegen(0), "Contract")?;
                    entries.push(RFollowsEntry {
                        name: path,
                        type_args,
                    })
                }
                Ok(RType::FollowsType(RFollowsClause {
                    entries,
                }))
            },
            CortexType::OptionalType(inner) => Ok(RType::OptionalType(Box::new(self.validate_type(*inner)?))),
            CortexType::NoneType => Ok(RType::NoneType),
            CortexType::GenericType(name) => Ok(RType::GenericType(name)),
        }
    }
    pub(crate) fn validate_type_args(&self, type_params: &Vec<TypeParam>, type_args: Vec<TypeArg>, type_name: String, object_name: &'static str) -> Result<Vec<RTypeArg>, CortexError> {
        if type_params.len() != type_args.len() {
            return Err(Box::new(PreprocessingError::MismatchedTypeArgCount(type_name, type_params.len(), type_args.len(), object_name)));
        }
        let mut result = Vec::new();
        for (tparam, targ) in type_params.iter().zip(type_args) {
            match (&tparam.typ, targ) {
                (crate::r#type::r#type::TypeParamType::Ty, TypeArg::Ty(t)) => {
                    result.push(RTypeArg::Ty(self.validate_type(t)?));
                },
                (crate::r#type::r#type::TypeParamType::Int, TypeArg::Int(v)) => {
                    result.push(RTypeArg::Int(v));
                },
                (first, second) => {
                    return Err(Box::new(PreprocessingError::MismatchedTypeArgument(second.codegen(0), first.codegen(0))));
                },
            }
        }
        Ok(result)
    }

    pub(crate) fn devalidate_type(&self, ty: RType) -> CortexType {
        match ty {
            RType::BasicType(name, type_args) => CortexType::basic(name, self.devalidate_type_args(type_args)),
            RType::RefType(inner, mutable) => CortexType::reference(self.devalidate_type(*inner), mutable),
            RType::TupleType(types) => CortexType::tuple(types.into_iter().map(|t| self.devalidate_type(t)).collect()),
            RType::FollowsType(follows) => CortexType::FollowsType(FollowsType {
                clause: self.devalidate_follows_clause(follows)
            }),
            RType::OptionalType(inner) => self.devalidate_type(inner.to_optional()),
            RType::NoneType => CortexType::NoneType,
            RType::GenericType(name) => CortexType::GenericType(name),
        }
    }
    pub(crate) fn devalidate_type_args(&self, ta: Vec<RTypeArg>) -> Vec<TypeArg> {
        ta
            .into_iter()
            .map(|t| {
                match t {
                    RTypeArg::Ty(t) => TypeArg::Ty(self.devalidate_type(t)),
                    RTypeArg::Int(v) => TypeArg::Int(v),
                }
            })
            .collect()
    }
    pub(crate) fn devalidate_follows_clause(&self, f: RFollowsClause) -> FollowsClause {
        FollowsClause {
            contracts: f.entries.into_iter().map(|e| FollowsEntry {
                name: e.name,
                type_args: self.devalidate_type_args(e.type_args),
            }).collect(),
        }
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
