use std::collections::HashMap;

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

use crate::{constants::{INDEX_GET_FN_NAME, INDEX_SET_FN_NAME}, preprocessing::ast::function_address::FunctionAddress, r#type::r#type::{CortexType, FollowsClause, FollowsEntry, FollowsType, TypeArg, TypeParam, TypeParamType}};

use super::ast::{expression::{BinaryOperator, IdentExpression, OptionalIdentifier, PConditionBody, PExpression, Parameter, PathIdent, UnaryOperator}, program::ModuleContent, statement::{AssignmentName, DeclarationName, PStatement}, top_level::{BasicBody, Body, Contract, Extension, Import, ImportEntry, MemberFunction, MemberFunctionSignature, PFunction, Struct as Struct, ThisArg, TopLevel}};

#[derive(Parser)]
#[grammar = "grammar.pest"] // relative to src
struct PestCortexParser;

pub struct CortexParser;

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Failed to parse statement '{0}'")]
    FailStatement(String),
    #[error("Failed to parse expression '{0}'")]
    FailExpression(String),
    #[error("Failed to parse atom '{0}'")]
    FailAtom(String),
    #[error("Failed to parse expression tail '{0}'")]
    FailTail(String),
    #[error("Failed to parse path '{0}'")]
    FailPath(String),
    #[error("Failed to parse identifier '{0}'")]
    FailOptionalIdentifier(String),
    #[error("Failed to parse type '{0}'")]
    FailType(String),
    #[error("Failed to parse type '{0}' (atom)")]
    FailTypeAtom(String),
    #[error("Failed to parse type '{0}' (tail)")]
    FailTypeTail(String),
    #[error("Failed to parse top level declaration '{0}'")]
    FailTopLevel(String),
    #[error("Failed to parse type argument '{0}'")]
    FailTypeArg(String),
    #[error("Failed to parse type parameter '{0}'")]
    FailTypeParam(String),
    #[error("Failed to parse import '{0}'")]
    FailImport(String),

    #[error("Failed to parse program")]
    FailProgram,
    #[error("Failed to parse {0}: {1}")]
    ParseFailure(String, String),
    #[error("Operator does not exist '{0}'")]
    OperatorDoesNotExist(String),

    #[error("Invalid this-arg: {0}")]
    InvalidThisArg(String),
    #[error("Composite {0} contains multiple fields named \"{1}\"")]
    CompositeContainsDuplicateFields(String, String),
}

impl CortexParser {
    pub fn parse_statement(input: &str) -> Result<PStatement, ParseError> {
        let pair = PestCortexParser::parse(Rule::statement, input);
        match pair {
            Ok(mut v) => Self::parse_stmt_pair(v.next().unwrap(), Vec::new()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("statement"), String::from(e.line())))
            },
        }
    }
    pub fn parse_expression(input: &str) -> Result<PExpression, ParseError> {
        let pair = PestCortexParser::parse(Rule::expr, input);
        match pair {
            Ok(mut v) => Self::parse_expr_pair(v.next().unwrap(), Vec::new()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("expression"), String::from(e.line())))
            },
        }
    }
    pub fn parse_type(input: &str) -> Result<CortexType, ParseError> {
        let pair = PestCortexParser::parse(Rule::typ, input);
        match pair {
            Ok(mut v) => Self::parse_type_pair(v.next().unwrap(), Vec::new()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("type"), String::from(e.line())))
            },
        }
    }
    pub fn parse_function(input: &str) -> Result<PFunction, ParseError> {
        let pair = PestCortexParser::parse(Rule::function, input);
        match pair {
            Ok(mut v) => Self::parse_func_pair(v.next().unwrap(), Vec::new()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("function"), String::from(e.line())))
            },
        }
    }
    pub fn parse_struct(input: &str) -> Result<Struct, ParseError> {
        let pair = PestCortexParser::parse(Rule::r#struct, input);
        match pair {
            Ok(mut v) => Self::parse_struct_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("struct"), String::from(e.line())))
            },
        }
    }
    pub fn parse_top_level(input: &str) -> Result<TopLevel, ParseError> {
        let pair = PestCortexParser::parse(Rule::topLevel, input);
        match pair {
            Ok(mut v) => Self::parse_toplevel_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("top level"), String::from(e.line())))
            },
        }
    }
    pub fn parse_import(input: &str) -> Result<Import, ParseError> {
        let pair = PestCortexParser::parse(Rule::import, input);
        match pair {
            Ok(mut v) => Self::parse_import_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("import"), String::from(e.line())))
            },
        }
    }
    pub fn parse_path(input: &str) -> Result<PathIdent, ParseError> {
        let pair = PestCortexParser::parse(Rule::pathIdent, input);
        match pair {
            Ok(mut v) => Self::parse_path_ident(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("path"), String::from(e.line())))
            },
        }
    }
    pub fn parse_program(input: &str) -> Result<ModuleContent, ParseError> {
        let pair = PestCortexParser::parse(Rule::program, input);
        match pair {
            Ok(mut v) => {
                let mut pairs = v.next().unwrap().into_inner();
                let next = pairs.next().unwrap();
                Self::parse_module_content(next)
            },
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("program"), String::from(e.line())))
            },
        }
    }

    fn parse_module_content(pair: Pair<Rule>) -> Result<ModuleContent, ParseError> {
        let pairs = pair.into_inner();
        let mut content = Vec::new();
        let mut imports = Vec::new();
        for p in pairs {
            if p.as_rule() == Rule::import {
                let im = Self::parse_import_pair(p)?;
                imports.push(im);
            } else if p.as_rule() == Rule::topLevel {
                let t = Self::parse_toplevel_pair(p)?;
                content.push(t);
            }
        }
        Ok(ModuleContent {
            content,
            imports,
        })
    }

    fn parse_toplevel_pair(mut pair: Pair<Rule>) -> Result<TopLevel, ParseError> {
        pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::function => {
                Ok(TopLevel::Function(Self::parse_func_pair(pair, Vec::new())?))
            },
            Rule::r#struct => {
                Ok(TopLevel::Struct(Self::parse_struct_pair(pair)?))
            },
            Rule::module => {
                let mut pairs = pair.into_inner();
                let name = pairs.next().unwrap().as_str();
                let contents = Self::parse_module_content(pairs.next().unwrap())?;
                Ok(TopLevel::Module { name: String::from(name), contents: contents })
            },
            Rule::extension => {
                Ok(TopLevel::Extension(Self::parse_extension_pair(pair)?))
            },
            Rule::contract => {
                Ok(TopLevel::Contract(Self::parse_contract_pair(pair)?))
            },
            _ => Err(ParseError::FailTopLevel(String::from(pair.as_str()))),
        }
    }

    fn parse_import_pair(pair: Pair<Rule>) -> Result<Import, ParseError> {
        let pairs = pair.into_inner();
        let mut entries = Vec::new();
        for p in pairs {
            entries.push(Self::parse_import_entry(p)?);
        }
        Ok(Import { entries })
    }
    fn parse_import_entry(pair: Pair<Rule>) -> Result<ImportEntry, ParseError> {
        let mut pairs = pair.into_inner();
        let path = Self::parse_path_ident(pairs.next().unwrap())?;
        let alias;
        if let Some(next) = pairs.next() {
            alias = Some(String::from(next.as_str()));
        } else {
            alias = None;
        }

        Ok(ImportEntry { path, alias })
    }

    fn parse_stmt_pair(mut pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PStatement, ParseError> {
        pair = pair.into_inner().next().unwrap();
        let orig = pair.as_str();
        match pair.as_rule() {
            Rule::expr => {
                let expression = Self::parse_expr_pair(pair, active_generics)?;
                Ok(PStatement::Expression(expression))
            },
            Rule::throw => {
                let mut pairs = pair.into_inner();
                let mut expression = None;
                if let Some(pair) = pairs.next() {
                    expression = Some(Self::parse_expr_pair(pair, active_generics)?);
                }
                Ok(PStatement::Throw(expression))
            },
            Rule::varDec => {
                let mut pairs = pair.into_inner();
                let is_const = match pairs.next().unwrap().as_rule() {
                    Rule::r#let => false,
                    Rule::r#const => true,
                    _ => {
                        return Err(ParseError::FailStatement(String::from(orig)));
                    }
                };
                let name = Self::parse_opt_ident(pairs.next().unwrap())?;
                let third_pair = pairs.next().unwrap();
                let mut typ: Option<CortexType> = None;
                let init_value = 
                    if third_pair.as_rule() == Rule::typ {
                        typ = Some(Self::parse_type_pair(third_pair, active_generics.clone())?);
                        Self::parse_expr_pair(pairs.next().unwrap(), active_generics)?
                    } else {
                        Self::parse_expr_pair(third_pair, active_generics)?
                    };
                Ok(PStatement::VariableDeclaration { 
                    name: DeclarationName::Single(name),
                    is_const: is_const,
                    typ: typ,
                    initial_value: init_value,
                })
            },
            Rule::tupleVarDec => {
                let mut pairs = pair.into_inner();
                let is_const = match pairs.next().unwrap().as_rule() {
                    Rule::r#let => false,
                    Rule::r#const => true,
                    _ => {
                        return Err(ParseError::FailStatement(String::from(orig)));
                    }
                };
                let name = Self::parse_declaration_name(pairs.next().unwrap())?;
                let third_pair = pairs.next().unwrap();
                let mut typ: Option<CortexType> = None;
                let init_value = 
                    if third_pair.as_rule() == Rule::typ {
                        typ = Some(Self::parse_type_pair(third_pair, active_generics.clone())?);
                        Self::parse_expr_pair(pairs.next().unwrap(), active_generics)?
                    } else {
                        Self::parse_expr_pair(third_pair, active_generics)?
                    };

                Ok(PStatement::VariableDeclaration { 
                    name,
                    is_const,
                    typ,
                    initial_value: init_value,
                })
            },
            Rule::varAssign => {
                let mut pairs = pair.into_inner();
                let left = Self::parse_ident_expr(pairs.next().unwrap())?;
                let value;
                let next = pairs.next().unwrap();
                match next.as_rule() {
                    Rule::arithLogicBinOp => {
                        let left_as_expr = left.clone().to_member_access_expr();
                        let op = Self::parse_binop(next.into_inner().next().unwrap())?;
                        let right = Self::parse_expr_pair(pairs.next().unwrap(), active_generics)?;
                        value = PExpression::BinaryOperation { 
                            left: Box::new(left_as_expr),
                            op,
                            right: Box::new(right),
                        };
                    },
                    Rule::expr => {
                        value = Self::parse_expr_pair(next, active_generics)?;
                    },
                    _ => return Err(ParseError::FailStatement(String::from(orig)))
                }

                Ok(PStatement::Assignment { 
                    name: AssignmentName::Single(left),
                    value: value,
                })
            },
            Rule::indexVarAssign => {
                let mut pairs = pair.into_inner();
                let left = Self::parse_ident_expr(pairs.next().unwrap())?;
                let mut args = Self::parse_expr_list(pairs.next().unwrap(), active_generics.clone())?;
                let next = pairs.next().unwrap();
                let value;
                match next.as_rule() {
                    Rule::arithLogicBinOp => {
                        let left_as_expr = left.clone().to_member_access_expr();
                        let full_left_expr = PExpression::MemberCall { 
                            callee: Box::new(left_as_expr),
                            member: String::from(INDEX_GET_FN_NAME), 
                            args: args.clone(),
                            type_args: None,
                        };
                        let op = Self::parse_binop(next.into_inner().next().unwrap())?;
                        let right = Self::parse_expr_pair(pairs.next().unwrap(), active_generics)?;
                        value = PExpression::BinaryOperation { 
                            left: Box::new(full_left_expr), 
                            op, 
                            right: Box::new(right), 
                        };
                    },
                    Rule::expr => {
                        value = Self::parse_expr_pair(next, active_generics)?;
                    },
                    _ => return Err(ParseError::FailStatement(String::from(orig)))
                }
                
                args.push(value);

                Ok(PStatement::Expression(
                    PExpression::MemberCall { 
                        callee: Box::new(left.to_member_access_expr()), 
                        member: String::from(INDEX_SET_FN_NAME), 
                        args, 
                        type_args: None,
                    }
                ))
            },
            Rule::tupleVarAssign => {
                let mut pairs = pair.into_inner();
                let left_pair = pairs.next().unwrap();
                let right_pair = pairs.next().unwrap();
                let left = Self::parse_assignment_name(left_pair)?;
                let right = Self::parse_expr_pair(right_pair, active_generics)?;

                Ok(PStatement::Assignment {
                    name: left,
                    value: right,
                })
            },
            Rule::r#while => {
                let mut pairs = pair.into_inner();
                let cond = Self::parse_expr_pair(pairs.next().unwrap(), active_generics.clone())?;
                let body = Self::parse_body(pairs.next().unwrap(), active_generics)?;
                Ok(PStatement::WhileLoop(PConditionBody { condition: cond, body: body }))
            },
            Rule::r#break => {
                Ok(PStatement::Break)
            },
            Rule::r#continue => {
                Ok(PStatement::Continue)
            },
            _ => Err(ParseError::FailStatement(String::from(pair.as_str()))),
        }
    }

    fn parse_assignment_name(pair: Pair<Rule>) -> Result<AssignmentName, ParseError> {
        match pair.as_rule() {
            Rule::identExpr => {
                Ok(AssignmentName::Single(Self::parse_ident_expr(pair)?))
            },
            Rule::tupleAssign => {
                let pairs = pair.into_inner();
                let mut collection = Vec::new();
                for p in pairs {
                    collection.push(Self::parse_assignment_name(p.into_inner().next().unwrap())?);
                }
                Ok(AssignmentName::Tuple(collection))
            },
            Rule::ignoredIdentifier => {
                Ok(AssignmentName::Ignore)
            },
            _ => Err(ParseError::FailStatement(String::from(pair.as_str())))
        }
    }
    fn parse_declaration_name(pair: Pair<Rule>) -> Result<DeclarationName, ParseError> {
        match pair.as_rule() {
            Rule::optIdentifier => {
                Ok(DeclarationName::Single(Self::parse_opt_ident(pair)?))
            },
            Rule::tupleVarDecName => {
                let pairs = pair.into_inner();
                let mut collection = Vec::new();
                for p in pairs {
                    collection.push(Self::parse_declaration_name(p.into_inner().next().unwrap())?);
                }
                Ok(DeclarationName::Tuple(collection))
            },
            _ => Err(ParseError::FailStatement(String::from(pair.as_str())))
        }
    }

    fn parse_expr_pair(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        Self::parse_logic_result(pair, active_generics)
    }
    fn parse_logic_result(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_eq_result(pairs.next().unwrap(), active_generics.clone())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_eq_result(pairs.next().unwrap(), active_generics.clone())?;
            result = PExpression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }
    fn parse_eq_result(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_sum_result(pairs.next().unwrap(), active_generics.clone())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_sum_result(pairs.next().unwrap(), active_generics.clone())?;
            result = PExpression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }
    fn parse_sum_result(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_mul_result(pairs.next().unwrap(), active_generics.clone())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_mul_result(pairs.next().unwrap(), active_generics.clone())?;
            result = PExpression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }
    fn parse_mul_result(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_primary(pairs.next().unwrap(), active_generics.clone())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_primary(pairs.next().unwrap(), active_generics.clone())?;
            result = PExpression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }

    fn parse_primary(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        let mut pairs = pair.into_inner();
        let atom_pair = pairs.next().unwrap();
        let atom = Self::parse_atom_pair(atom_pair.into_inner().next().unwrap(), active_generics.clone())?;
        let next = pairs.next();
        if next.is_some() {
            let expr_tail_pair = next.unwrap();
            Ok(Self::handle_expr_tail_pair(expr_tail_pair, atom, active_generics)?)
        } else {
            Ok(atom)
        }
    }

    fn parse_atom_pair(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        match pair.as_rule() {
            Rule::number => {
                let value: f64 = pair.as_str().parse().unwrap();
                Ok(PExpression::Number(value))
            },
            Rule::boolean => {
                let value: bool = pair.as_str().trim().parse().unwrap();
                Ok(PExpression::Boolean(value))
            },
            Rule::string => {
                Ok(PExpression::String(String::from(pair.into_inner().next().unwrap().as_str())))
            },
            Rule::r#char => {
                let s = pair.into_inner().next().unwrap().as_str();
                Ok(PExpression::Char(unescape(s)))
            },
            Rule::none => {
                Ok(PExpression::None)
            },
            Rule::void => {
                Ok(PExpression::Void)
            },
            Rule::pathIdent => {
                Ok(PExpression::PathIdent(Self::parse_path_ident(pair)?))
            },
            Rule::expr => {
                Ok(Self::parse_expr_pair(pair, active_generics)?)
            },
            Rule::call => {
                let mut pairs = pair.into_inner().peekable();
                let first_pair = pairs.next().unwrap();
                let name = Self::parse_path_ident(first_pair)?;
                let next_pair = pairs.peek().unwrap();
                let type_args;
                if let Rule::typeArgList = next_pair.as_rule() {
                    type_args = Some(Self::parse_type_arg_list(pairs.next().unwrap(), active_generics.clone())?);
                } else {
                    type_args = None;
                }
                let args_pair = pairs.next().unwrap();
                let args = Self::parse_expr_list(args_pair, active_generics)?;
                Ok(PExpression::Call { name: FunctionAddress::simple(name), args, type_args })
            },
            Rule::construction => {
                let mut pairs = pair.into_inner().peekable();
                let name = Self::parse_path_ident(pairs.next().unwrap())?;
                let type_args;
                if pairs.peek().is_some() && pairs.peek().unwrap().as_rule() == Rule::typeArgList {
                    type_args = Self::parse_type_arg_list(pairs.next().unwrap(), active_generics.clone())?;
                } else {
                    type_args = vec![];
                }
                let mut assignments = Vec::new();
                for p in pairs {
                    let mut member_init = p.into_inner();
                    let name = member_init.next().unwrap().as_str();
                    let expr = Self::parse_expr_pair(member_init.next().unwrap(), active_generics.clone())?;
                    assignments.push((String::from(name), expr));
                }
                Ok(PExpression::Construction { name, assignments, type_args })
            },
            Rule::r#if => {
                let mut pairs = pair.into_inner();
                let first_cond = Self::parse_expr_pair(pairs.next().unwrap(), active_generics.clone())?;
                let first_body = Self::parse_body(pairs.next().unwrap(), active_generics.clone())?;
                let elifs_pair = pairs.next().unwrap();
                let elifs_pairs = elifs_pair.into_inner();
                let mut elifs = Vec::<PConditionBody>::new();
                for p in elifs_pairs {
                    elifs.push(Self::parse_condition_body(p, active_generics.clone())?);
                }
                let op_else_pair = pairs.next();
                let else_body = 
                    if let Some(else_pair) = op_else_pair {
                        Some(Box::new(Self::parse_body(else_pair.into_inner().next().unwrap(), active_generics)?))
                    } else {
                        None
                    };

                Ok(PExpression::IfStatement { 
                    first: Box::new(PConditionBody { condition: first_cond, body: first_body }),
                    conds: elifs,
                    last: else_body,
                })
            },
            Rule::unOpAtom => {
                let mut pairs = pair.into_inner();
                let unop = Self::parse_unop(pairs.next().unwrap())?;
                let expr = Self::parse_expr_pair(pairs.next().unwrap(), active_generics)?;
                Ok(PExpression::UnaryOperation { op: unop, exp: Box::new(expr) })
            },
            Rule::listLiteral => {
                let mut pairs = pair.into_inner();
                let items = Self::parse_expr_list(pairs.next().unwrap(), active_generics)?;
                Ok(PExpression::ListLiteral(items))
            },
            Rule::tuple => {
                let items = Self::parse_expr_list(pair, active_generics)?;
                Ok(PExpression::Tuple(items))
            },
            Rule::range => {
                fn parse_range_val(pair: Pair<Rule>) -> Option<f64> {
                    let p = pair.into_inner().next();
                    if let Some(p) = p {
                        match p.as_rule() {
                            Rule::int => Some(p.as_str().parse().unwrap()),
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                let mut pairs = pair.into_inner().peekable();
                let start = parse_range_val(pairs.next().unwrap());
                let end = parse_range_val(pairs.next().unwrap());
                let step;
                if let Some(_) = pairs.peek() {
                    step = parse_range_val(pairs.next().unwrap());
                } else {
                    step = None;
                }

                Ok(PExpression::Range { start, end, step })
            },
            Rule::heapExp => {
                let inner = pair.into_inner().next().unwrap();
                let inner = Self::parse_expr_pair(inner, active_generics)?;
                Ok(PExpression::HeapAlloc(Box::new(inner)))
            },
            _ => Err(ParseError::FailAtom(String::from(pair.as_str()))),
        }
    }

    fn handle_expr_tail_pair(pair: Pair<Rule>, exp: PExpression, active_generics: Vec<String>) -> Result<PExpression, ParseError> {
        match pair.as_rule() {
            Rule::exprTail => {
                if let Some(tail_pair) = pair.into_inner().next() {
                    match tail_pair.as_rule() {
                        Rule::postfixBangTail => {
                            Ok(Self::handle_expr_tail_pair(
                                tail_pair.into_inner().next().unwrap(),
                                PExpression::Bang(Box::new(exp)),
                                active_generics
                            )?)
                        },
                        Rule::memberAccessTail => {
                            let mut pairs = tail_pair.into_inner();
                            let member = pairs.next().unwrap().as_str();
                            Ok(Self::handle_expr_tail_pair(
                                pairs.next().unwrap(),
                                PExpression::MemberAccess(Box::new(exp), String::from(member)),
                                active_generics
                            )?)
                        },
                        Rule::memberCallTail => {
                            let mut pairs = tail_pair.into_inner().peekable();
                            let member = pairs.next().unwrap().as_str();
                            let next_pair = pairs.peek().unwrap();
                            let type_args;
                            if let Rule::typeArgList = next_pair.as_rule() {
                                type_args = Some(Self::parse_type_arg_list(pairs.next().unwrap(), active_generics.clone())?);
                            } else {
                                type_args = None;
                            }
                            let args_pair = pairs.next().unwrap();
                            let args = Self::parse_expr_list(args_pair, active_generics.clone())?;
                            Ok(Self::handle_expr_tail_pair(
                                pairs.next().unwrap(),
                                PExpression::MemberCall { callee: Box::new(exp), member: String::from(member), args, type_args },
                                active_generics
                            )?)
                        },
                        Rule::indexTail => {
                            let mut pairs = tail_pair.into_inner();
                            let args_pair = pairs.next().unwrap();
                            let args = Self::parse_expr_list(args_pair, active_generics.clone())?;
                            Ok(Self::handle_expr_tail_pair(
                                pairs.next().unwrap(),
                                PExpression::MemberCall { callee: Box::new(exp), member: String::from(INDEX_GET_FN_NAME), args, type_args: None, },
                                active_generics
                            )?)
                        },
                        _ => Err(ParseError::FailTail(String::from(tail_pair.as_str()))),
                    }
                } else {
                    Ok(exp)
                }
            }
            _ => Err(ParseError::FailTail(String::from(pair.as_str()))),
        }
    }

    fn parse_condition_body(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<PConditionBody, ParseError> {
        let mut pairs = pair.into_inner();
        let cond = Self::parse_expr_pair(pairs.next().unwrap(), active_generics.clone())?;
        let body = Self::parse_body(pairs.next().unwrap(), active_generics)?;
        Ok(PConditionBody {
            condition: cond,
            body: body,
        })
    }

    fn parse_binop(pair: Pair<Rule>) -> Result<BinaryOperator, ParseError> {
        match pair.as_rule() {
            Rule::add => Ok(BinaryOperator::Add),
            Rule::sub => Ok(BinaryOperator::Subtract),
            Rule::mul => Ok(BinaryOperator::Multiply),
            Rule::div => Ok(BinaryOperator::Divide),
            Rule::rem => Ok(BinaryOperator::Remainder),
            Rule::and => Ok(BinaryOperator::LogicAnd),
            Rule::or => Ok(BinaryOperator::LogicOr),
            Rule::eq => Ok(BinaryOperator::IsEqual),
            Rule::neq => Ok(BinaryOperator::IsNotEqual),
            Rule::lt => Ok(BinaryOperator::IsLessThan),
            Rule::lte => Ok(BinaryOperator::IsLessThanOrEqualTo),
            Rule::gt => Ok(BinaryOperator::IsGreaterThan),
            Rule::gte => Ok(BinaryOperator::IsGreaterThanOrEqualTo),
            _ => Err(ParseError::OperatorDoesNotExist(String::from(pair.as_str()))),
        }
    }
    fn parse_unop(pair: Pair<Rule>) -> Result<UnaryOperator, ParseError> {
        match pair.as_rule() {
            Rule::negate => Ok(UnaryOperator::Negate),
            Rule::invert => Ok(UnaryOperator::Invert),
            Rule::deref => Ok(UnaryOperator::Deref),
            _ => Err(ParseError::OperatorDoesNotExist(String::from(pair.as_str()))),
        }
    }

    fn parse_path_ident(pair: Pair<Rule>) -> Result<PathIdent, ParseError> {
        let mut names = Vec::<String>::new();
        for p in pair.into_inner() {
            let name = String::from(p.as_str());
            names.push(name);
        }
        Ok(PathIdent {
            path: names,
        })
    }

    fn parse_ident_expr(pair: Pair<Rule>) -> Result<IdentExpression, ParseError> {
        let mut pairs = pair.into_inner();
        let first = pairs.next().unwrap().as_str();
        let mut chain = Vec::new();
        for p in pairs {
            chain.push(String::from(p.as_str()));
        }
        Ok(IdentExpression {
            base: String::from(first),
            chain: chain,
        })
    }

    fn parse_type_arg(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<TypeArg, ParseError> {
        match pair.as_rule() {
            Rule::typ => Ok(TypeArg::Ty(Self::parse_type_pair(pair, active_generics)?)),
            Rule::int => Ok(TypeArg::Int(pair.as_str().parse().unwrap())),
            _ => Err(ParseError::FailTypeArg(String::from(pair.as_str())))
        }
    }

    fn parse_type_param(pair: Pair<Rule>) -> Result<TypeParam, ParseError> {
        let as_str = pair.as_str();
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap().as_str();
        let mut type_param_type = TypeParamType::Ty;
        if let Some(p) = pairs.next() {
            match p.as_rule() {
                Rule::typeParamTy => type_param_type = TypeParamType::Ty,
                Rule::typeParamInt => type_param_type = TypeParamType::Int,
                _ => return Err(ParseError::FailTypeParam(String::from(as_str))),
            }
        }

        Ok(TypeParam::new(name, type_param_type))
    }

    fn parse_type_pair(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<CortexType, ParseError> {
        let mut pairs = pair.into_inner();
        let atom = Self::parse_type_atom(pairs.next().unwrap(), active_generics)?;
        let next = pairs.next();
        if next.is_some() {
            let tail_pair = next.unwrap();
            Ok(Self::parse_type_tail(tail_pair, atom)?)
        } else {
            Ok(atom)
        }
    }
    fn parse_type_atom(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<CortexType, ParseError> {
        match pair.as_rule() {
            Rule::basicType => {
                let mut pairs = pair.into_inner();
                let ident = Self::parse_path_ident(pairs.next().unwrap())?;
                let mut type_args = Vec::new();
                if let Some(type_args_top_pair) = pairs.next() {
                    let type_arg_pairs = type_args_top_pair.into_inner();
                    for typ_pair in type_arg_pairs {
                        type_args.push(Self::parse_type_arg(typ_pair, active_generics.clone())?);
                    }
                }
                if ident.is_final() && ident.get_back().unwrap() == "none" && type_args.len() == 0 {
                    Ok(CortexType::none())
                } else if ident.is_final() && type_args.len() == 0 && active_generics.contains(ident.get_back().unwrap()) {
                    Ok(CortexType::GenericType(ident.get_back().unwrap().clone()))
                } else {
                    Ok(CortexType::basic(ident, type_args))
                }
            },
            Rule::refType => {
                let main_str = pair.as_str();
                let typ = Self::parse_type_pair(pair.into_inner().next().unwrap(), active_generics)?;
                let mutable = main_str.starts_with("&mut");
                Ok(CortexType::reference(typ, mutable))
            },
            Rule::tupleType => {
                let pairs = pair.into_inner();
                let mut types = Vec::new();
                for p in pairs {
                    types.push(Self::parse_type_pair(p, active_generics.clone())?);
                }
                Ok(CortexType::tuple(types))
            },
            Rule::followsType => {
                let mut pairs = pair.into_inner();
                let clause = Self::parse_follows_clause(pairs.next().unwrap(), active_generics)?;
                Ok(CortexType::FollowsType(FollowsType {
                    clause,
                }))
            },
            Rule::typ => {
                Ok(Self::parse_type_pair(pair, active_generics)?)
            },
            _ => Err(ParseError::FailTypeAtom(String::from(pair.as_str())))
        }
    }
    fn parse_type_tail(pair: Pair<Rule>, base: CortexType) -> Result<CortexType, ParseError> {
        match pair.as_rule() {
            Rule::typeTail => {
                if let Some(tail_pair) = pair.into_inner().next() {
                    match tail_pair.as_rule() {
                        Rule::optionalTail => {
                            Ok(CortexType::OptionalType(Box::new(base)))
                        },
                        _ => Err(ParseError::FailTail(String::from(tail_pair.as_str()))),
                    }
                } else {
                    Ok(base)
                }
            }
            _ => Err(ParseError::FailTypeTail(String::from(pair.as_str()))),
        }
    }

    fn parse_opt_ident(pair: Pair<Rule>) -> Result<OptionalIdentifier, ParseError> {
        if pair.as_str() == "~" {
            Ok(OptionalIdentifier::Ignore)
        } else {
            Ok(OptionalIdentifier::Ident(String::from(pair.as_str())))
        }
    }

    fn parse_struct_pair(pair: Pair<Rule>) -> Result<Struct, ParseError> {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap().as_str();
        let mut follows_clause = None;
        let mut type_params = Vec::new();
        let mut next = pairs.next().unwrap();
        let field_params;
        if matches!(next.as_rule(), Rule::typeParamList) {
            type_params = Self::parse_type_param_list(next)?;
            next = pairs.next().unwrap();
        }
        let active_generics = Self::convert_to_active_generics(&type_params);

        if matches!(next.as_rule(), Rule::followsClause) {
            let clause = Self::parse_follows_clause(next, active_generics.clone())?;
            follows_clause = Some(clause);
            next = pairs.next().unwrap();
        }

        field_params = Self::parse_param_list(next, active_generics.clone())?;
        let functions = Self::parse_member_func_list(pairs.next().unwrap(), active_generics)?;

        let mut fields = HashMap::new();
        for p in field_params {
            if fields.contains_key(&p.name) {
                return Err(ParseError::CompositeContainsDuplicateFields(String::from(name), p.name.clone()));
            }
            fields.insert(p.name, p.typ);
        }
        
        Ok(
            Struct { 
                name: String::from(name),
                fields: fields,
                functions: functions,
                type_params,
                follows_clause,
            }
        )
    }
    fn parse_extension_pair(pair: Pair<Rule>) -> Result<Extension, ParseError> {
        let mut pairs = pair.into_inner();
        let name = Self::parse_path_ident(pairs.next().unwrap())?;
        let mut follows_clause = None;
        let mut type_params = Vec::new();
        let mut next = pairs.next().unwrap();
        if matches!(next.as_rule(), Rule::typeParamList) {
            type_params = Self::parse_type_param_list(next)?;
            next = pairs.next().unwrap();
        }
        let active_generics = Self::convert_to_active_generics(&type_params);
        
        if matches!(next.as_rule(), Rule::followsClause) {
            let clause = Self::parse_follows_clause(next, active_generics.clone())?;
            follows_clause = Some(clause);
            next = pairs.next().unwrap();
        }

        let functions = Self::parse_member_func_list(next, active_generics)?;
        
        Ok(
            Extension { 
                name: name,
                functions: functions,
                type_params,
                follows_clause,
            }
        )
    }
    fn parse_contract_pair(pair: Pair<Rule>) -> Result<Contract, ParseError> {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap().as_str();
        let mut type_params = Vec::new();
        let next = pairs.next();
        let functions;
        if let Some(next) = next {
            if matches!(next.as_rule(), Rule::typeParamList) {
                type_params = Self::parse_type_param_list(next)?;
                let mut active_generics = Self::convert_to_active_generics(&type_params);
                
                let mut sigs = Vec::new();
                while let Some(p) = pairs.next() {
                    sigs.push(Self::parse_member_function_signature(p, &mut active_generics)?);
                }
                functions = sigs;
            } else {
                let mut sigs = Vec::new();
                let mut active_generics = Vec::new();
                sigs.push(Self::parse_member_function_signature(next, &mut active_generics)?);
                
                while let Some(p) = pairs.next() {
                    // Recreate-every time, since we don't want the active generics from one member function
                    // to carry on to the next
                    let mut active_generics = Vec::new();
                    sigs.push(Self::parse_member_function_signature(p, &mut active_generics)?);
                }
                functions = sigs;
            }
        } else {
            functions = Vec::new();
        }
        
        Ok(
            Contract { 
                name: String::from(name),
                function_sigs: functions,
                type_params,
            }
        )
    }

    fn parse_follows_clause(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<FollowsClause, ParseError> {
        let pairs = pair.into_inner();
        let mut paths = Vec::new();
        for p in pairs {
            paths.push(Self::parse_follows_entry(p, active_generics.clone())?);
        }
        Ok(
            FollowsClause {
                contracts: paths,
            }
        )
    }
    fn parse_follows_entry(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<FollowsEntry, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let name = Self::parse_path_ident(pairs.next().unwrap())?;
        let mut type_args = Vec::new();
        if let Some(next) = pairs.next() {
            type_args = Self::parse_type_arg_list(next, active_generics)?;
        }
        Ok(
            FollowsEntry {
                name,
                type_args,
            }
        )
    }

    fn parse_func_pair(pair: Pair<Rule>, mut active_generics: Vec<String>) -> Result<PFunction, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;

        let mut type_params = Vec::new();
        let next = pairs.next().unwrap();
        let params;
        if matches!(next.as_rule(), Rule::typeParamList) {
            type_params = Self::parse_type_param_list(next)?;
            active_generics.extend(type_params.iter().map(|t| t.name.clone()));
            params = Self::parse_param_list(pairs.next().unwrap(), active_generics.clone())?;
        } else {
            params = Self::parse_param_list(next, active_generics.clone())?;
        }

        let return_type = if matches!(pairs.peek().unwrap().as_rule(), Rule::typ) {
            Self::parse_type_pair(pairs.next().unwrap(), active_generics.clone())?
        } else {
            CortexType::void()
        };
        let body = Self::parse_body(pairs.next().unwrap(), active_generics)?;
        Ok(PFunction {
            name: name,
            params: params,
            return_type: return_type,
            body: Body::Basic(body),
            type_params,
        })
    }
    fn parse_member_function(pair: Pair<Rule>, mut active_generics: Vec<String>) -> Result<MemberFunction, ParseError> {
        let mut pairs = pair.into_inner().peekable();

        let signature = Self::parse_member_function_signature(pairs.next().unwrap(), &mut active_generics)?;
        let body = Self::parse_body(pairs.next().unwrap(), active_generics)?;
        Ok(MemberFunction {
            signature,
            body: Body::Basic(body),
        })
    }
    fn parse_member_function_signature(pair: Pair<Rule>, active_generics: &mut Vec<String>) -> Result<MemberFunctionSignature, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;

        let mut type_params = Vec::new();
        let next = pairs.next().unwrap();
        let this_arg;
        if matches!(next.as_rule(), Rule::typeParamList) {
            type_params = Self::parse_type_param_list(next)?;
            active_generics.extend(type_params.iter().map(|t| t.name.clone()));
            this_arg = Self::parse_this_arg(pairs.next().unwrap())?;
        } else {
            this_arg = Self::parse_this_arg(next)?;
        }
        
        let params = Self::parse_param_list(pairs.next().unwrap(), active_generics.clone())?;
        
        let return_type = if matches!(pairs.peek(), Some(_)) && matches!(pairs.peek().unwrap().as_rule(), Rule::typ) {
            Self::parse_type_pair(pairs.next().unwrap(), active_generics.clone())?
        } else {
            CortexType::void()
        };
        Ok(MemberFunctionSignature {
            name: name,
            params: params,
            return_type: return_type,
            this_arg: this_arg,
            type_params,
        })
    }

    fn parse_this_arg(pair: Pair<Rule>) -> Result<ThisArg, ParseError> {
        if matches!(pair.as_rule(), Rule::refThis) {
            Ok(ThisArg::RefThis)
        } else if matches!(pair.as_rule(), Rule::refMutThis) {
            Ok(ThisArg::RefMutThis)
        } else if matches!(pair.as_rule(), Rule::directThis) {
            Ok(ThisArg::DirectThis)
        } else {
            Err(ParseError::InvalidThisArg(String::from(pair.as_str())))
        }
    }

    fn parse_member_func_list(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<Vec<MemberFunction>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_member_function(p, active_generics.clone())?);
        }
        Ok(result)
    }

    fn parse_expr_list(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<Vec<PExpression>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_expr_pair(p, active_generics.clone())?);
        }
        Ok(result)
    }

    fn parse_type_arg_list(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<Vec<TypeArg>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_type_arg(p, active_generics.clone())?);
        }
        Ok(result)
    }

    fn parse_type_param_list(pair: Pair<Rule>) -> Result<Vec<TypeParam>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_type_param(p)?);
        }
        Ok(result)
    }

    fn parse_param_list(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<Vec<Parameter>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_param(p, active_generics.clone())?);
        }
        Ok(result)
    }
    fn parse_param(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<Parameter, ParseError> {
        let mut pairs = pair.into_inner();
        let ident = pairs.next().unwrap().as_str();
        let typ = Self::parse_type_pair(pairs.next().unwrap(), active_generics)?;
        Ok(
            Parameter {
                name: String::from(ident),
                typ: typ,
            }
        )
    }

    fn parse_body(pair: Pair<Rule>, active_generics: Vec<String>) -> Result<BasicBody, ParseError> {
        let mut result: Option<PExpression> = None;
        let mut statements = Vec::<PStatement>::new();

        let mut iter = pair.into_inner().peekable();
        while let Some(p) = iter.next() {
            let is_last = iter.peek().is_none();
            if is_last {
                if p.as_rule() == Rule::expr {
                    result = Some(Self::parse_expr_pair(p, active_generics.clone())?);
                } else {
                    statements.push(Self::parse_stmt_pair(p, active_generics.clone())?);
                }
            } else {
                statements.push(Self::parse_stmt_pair(p, active_generics.clone())?);
            }
        }
        Ok(
            BasicBody {
                statements: statements,
                result: result,
            }
        )
    }

    fn convert_to_active_generics(type_params: &Vec<TypeParam>) -> Vec<String> {
        type_params.iter().map(|t| t.name.clone()).collect()
    }
}

fn unescape(s: &str) -> u8 {
    match s {
        "\\n" => '\n' as u8,
        "\\t" => '\t' as u8,
        "\\\\" => '\\' as u8,
        "\\r" => '\r' as u8,
        "\\0" => '\0' as u8,
        "\\\"" => '\"' as u8,
        "\\\'" => '\'' as u8,
        _ => s.as_bytes()[0],
    }
}