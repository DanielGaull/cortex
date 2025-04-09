use std::collections::HashMap;

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

use crate::constants::{INDEX_SET_FN_NAME, INDEX_GET_FN_NAME};

use super::ast::{expression::{Expression, BinaryOperator, ConditionBody, IdentExpression, OptionalIdentifier, Parameter, PathIdent, UnaryOperator}, program::Program, statement::Statement, top_level::{BasicBody, Body, Bundle, Function, Struct, ThisArg, TopLevel}, r#type::CortexType};

#[derive(Parser)]
#[grammar = "grammar.pest"] // relative to src
struct PestCortexParser;

pub struct CortexParser;

#[derive(Error, Debug)]
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
    #[error("Failed to parse top level declaration '{0}'")]
    FailTopLevel(String),
    #[error("Operator does not exist '{0}'")]
    OperatorDoesNotExist(String),
    #[error("Failed to parse program")]
    FailProgram,
    #[error("Failed to parse {0}: {1}")]
    ParseFailure(String, String),
    #[error("Invalid this-arg: {0}")]
    InvalidThisArg(String),
}

impl CortexParser {
    pub fn parse_statement(input: &str) -> Result<Statement, ParseError> {
        let pair = PestCortexParser::parse(Rule::statement, input);
        match pair {
            Ok(mut v) => Self::parse_stmt_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("statement"), String::from(e.line())))
            },
        }
    }
    pub fn parse_expression(input: &str) -> Result<Expression, ParseError> {
        let pair = PestCortexParser::parse(Rule::expr, input);
        match pair {
            Ok(mut v) => Self::parse_expr_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("expression"), String::from(e.line())))
            },
        }
    }
    pub fn parse_type(input: &str) -> Result<CortexType, ParseError> {
        let pair = PestCortexParser::parse(Rule::typ, input);
        match pair {
            Ok(mut v) => Self::parse_type_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("type"), String::from(e.line())))
            },
        }
    }
    pub fn parse_function(input: &str) -> Result<Function, ParseError> {
        let pair = PestCortexParser::parse(Rule::function, input);
        match pair {
            Ok(mut v) => Self::parse_func_pair(v.next().unwrap()),
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
    pub fn parse_bundle(input: &str) -> Result<Bundle, ParseError> {
        let pair = PestCortexParser::parse(Rule::bundle, input);
        match pair {
            Ok(mut v) => Self::parse_bundle_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("bundle"), String::from(e.line())))
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
    pub fn parse_path(input: &str) -> Result<PathIdent, ParseError> {
        let pair = PestCortexParser::parse(Rule::pathIdent, input);
        match pair {
            Ok(mut v) => Self::parse_path_ident(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("path"), String::from(e.line())))
            },
        }
    }
    pub fn parse_program(input: &str) -> Result<Program, ParseError> {
        let pair = PestCortexParser::parse(Rule::program, input);
        match pair {
            Ok(mut v) => Self::parse_program_pair(v.next().unwrap()),
            Err(e) => {
                Err(ParseError::ParseFailure(String::from("program"), String::from(e.line())))
            },
        }
    }
    
    fn parse_program_pair(pair: Pair<Rule>) -> Result<Program, ParseError> {
        let pairs = pair.into_inner();
        let mut content = Vec::<TopLevel>::new();
        for p in pairs {
            if p.as_rule() != Rule::EOI {
                let t = Self::parse_toplevel_pair(p)?;
                content.push(t);
            }
        }
        Ok(Program { content: content })
    }

    fn parse_toplevel_pair(mut pair: Pair<Rule>) -> Result<TopLevel, ParseError> {
        pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::function => {
                Ok(TopLevel::Function(Self::parse_func_pair(pair)?))
            },
            Rule::r#struct => {
                Ok(TopLevel::Struct(Self::parse_struct_pair(pair)?))
            },
            Rule::bundle => {
                Ok(TopLevel::Bundle(Self::parse_bundle_pair(pair)?))
            },
            Rule::import => {
                let name: &str;
                let mut is_string = false;
                let p = pair.into_inner().next().unwrap();
                if p.as_rule() == Rule::string {
                    name = p.into_inner().next().unwrap().as_str();
                    is_string = true;
                } else {
                    name = p.as_str();
                }
                Ok(TopLevel::Import { name: String::from(name), is_string_import: is_string })
            },
            Rule::module => {
                let mut pairs = pair.into_inner();
                let name = pairs.next().unwrap().as_str();
                let mut contents = Vec::<TopLevel>::new();
                for p in pairs {
                    let toplevel = Self::parse_toplevel_pair(p)?;
                    contents.push(toplevel);
                }
                Ok(TopLevel::Module { name: String::from(name), contents: contents })
            },
            _ => Err(ParseError::FailTopLevel(String::from(pair.as_str()))),
        }
    }

    fn parse_stmt_pair(mut pair: Pair<Rule>) -> Result<Statement, ParseError> {
        pair = pair.into_inner().next().unwrap();
        let orig = pair.as_str();
        match pair.as_rule() {
            Rule::expr => {
                let expression = Self::parse_expr_pair(pair)?;
                Ok(Statement::Expression(expression))
            },
            Rule::throw => {
                let expression = Self::parse_expr_pair(pair.into_inner().next().unwrap())?;
                Ok(Statement::Throw(expression))
            },
            Rule::varDec => {
                let is_const = pair.as_str().starts_with("const");
                let mut pairs = pair.into_inner();
                let name = Self::parse_opt_ident(pairs.next().unwrap())?;
                let third_pair = pairs.next().unwrap();
                let mut typ: Option<CortexType> = None;
                let init_value = 
                    if third_pair.as_rule() == Rule::typ {
                        typ = Some(Self::parse_type_pair(third_pair)?);
                        Self::parse_expr_pair(pairs.next().unwrap())?
                    } else {
                        Self::parse_expr_pair(third_pair)?
                    };
                Ok(Statement::VariableDeclaration { 
                    name: name,
                    is_const: is_const,
                    typ: typ,
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
                        let right = Self::parse_expr_pair(pairs.next().unwrap())?;
                        value = Expression::BinaryOperation { 
                            left: Box::new(left_as_expr),
                            op,
                            right: Box::new(right),
                        };
                    },
                    Rule::expr => {
                        value = Self::parse_expr_pair(next)?;
                    },
                    _ => return Err(ParseError::FailStatement(String::from(orig)))
                }

                Ok(Statement::Assignment { 
                    name: left,
                    value: value,
                })
            },
            Rule::indexVarAssign => {
                let mut pairs = pair.into_inner();
                let left = Self::parse_ident_expr(pairs.next().unwrap())?;
                let mut args = Self::parse_expr_list(pairs.next().unwrap())?;
                let next = pairs.next().unwrap();
                let value;
                match next.as_rule() {
                    Rule::arithLogicBinOp => {
                        let left_as_expr = left.clone().to_member_access_expr();
                        let full_left_expr = Expression::MemberCall { 
                            callee: Box::new(left_as_expr),
                            member: String::from(INDEX_GET_FN_NAME), 
                            args: args.clone(),
                            type_args: None,
                        };
                        let op = Self::parse_binop(next.into_inner().next().unwrap())?;
                        let right = Self::parse_expr_pair(pairs.next().unwrap())?;
                        value = Expression::BinaryOperation { 
                            left: Box::new(full_left_expr), 
                            op, 
                            right: Box::new(right), 
                        };
                    },
                    Rule::expr => {
                        value = Self::parse_expr_pair(next)?;
                    },
                    _ => return Err(ParseError::FailStatement(String::from(orig)))
                }
                
                args.push(value);

                Ok(Statement::Expression(
                    Expression::MemberCall { 
                        callee: Box::new(left.to_member_access_expr()), 
                        member: String::from(INDEX_SET_FN_NAME), 
                        args, 
                        type_args: None,
                    }
                ))
            },
            Rule::r#while => {
                let mut pairs = pair.into_inner();
                let cond = Self::parse_expr_pair(pairs.next().unwrap())?;
                let body = Self::parse_body(pairs.next().unwrap())?;
                Ok(Statement::WhileLoop(ConditionBody { condition: cond, body: body }))
            },
            Rule::r#break => {
                Ok(Statement::Break)
            },
            Rule::r#continue => {
                Ok(Statement::Continue)
            },
            _ => Err(ParseError::FailStatement(String::from(pair.as_str()))),
        }
    }

    fn parse_expr_pair(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        Self::parse_logic_result(pair)
    }
    fn parse_logic_result(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_eq_result(pairs.next().unwrap())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_eq_result(pairs.next().unwrap())?;
            result = Expression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }
    fn parse_eq_result(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_sum_result(pairs.next().unwrap())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_sum_result(pairs.next().unwrap())?;
            result = Expression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }
    fn parse_sum_result(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_mul_result(pairs.next().unwrap())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_mul_result(pairs.next().unwrap())?;
            result = Expression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }
    fn parse_mul_result(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let mut result = Self::parse_primary(pairs.next().unwrap())?;
        while pairs.peek() != None {
            let op = Self::parse_binop(pairs.next().unwrap())?;
            let right = Self::parse_primary(pairs.next().unwrap())?;
            result = Expression::BinaryOperation { 
                left: Box::new(result), 
                op, 
                right: Box::new(right),
            };
        }
        Ok(result)
    }

    fn parse_primary(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner();
        let atom_pair = pairs.next().unwrap();
        let atom = Self::parse_atom_pair(atom_pair.into_inner().next().unwrap())?;
        let next = pairs.next();
        if next.is_some() {
            let expr_tail_pair = next.unwrap();
            Ok(Self::handle_expr_tail_pair(expr_tail_pair, atom)?)
        } else {
            Ok(atom)
        }
    }

    fn parse_atom_pair(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        match pair.as_rule() {
            Rule::number => {
                let value: f64 = pair.as_str().parse().unwrap();
                Ok(Expression::Number(value))
            },
            Rule::boolean => {
                let value: bool = pair.as_str().parse().unwrap();
                Ok(Expression::Boolean(value))
            },
            Rule::string => {
                Ok(Expression::String(String::from(pair.into_inner().next().unwrap().as_str())))
            },
            Rule::none => {
                Ok(Expression::None)
            },
            Rule::void => {
                Ok(Expression::Void)
            },
            Rule::pathIdent => {
                Ok(Expression::PathIdent(Self::parse_path_ident(pair)?))
            },
            Rule::expr => {
                Ok(Self::parse_expr_pair(pair)?)
            },
            Rule::call => {
                let mut pairs = pair.into_inner().peekable();
                let first_pair = pairs.next().unwrap();
                let name = Self::parse_path_ident(first_pair)?;
                let next_pair = pairs.peek().unwrap();
                let type_args;
                if let Rule::typeList = next_pair.as_rule() {
                    type_args = Some(Self::parse_type_list(pairs.next().unwrap())?);
                } else {
                    type_args = None;
                }
                let args_pair = pairs.next().unwrap();
                let args = Self::parse_expr_list(args_pair)?;
                Ok(Expression::Call { name, args, type_args })
            },
            Rule::structConstruction => {
                let mut pairs = pair.into_inner().peekable();
                let name = Self::parse_path_ident(pairs.next().unwrap())?;
                let type_args;
                if pairs.peek().unwrap().as_rule() == Rule::typeList {
                    type_args = Self::parse_type_list(pairs.next().unwrap())?;
                } else {
                    type_args = vec![];
                }
                let mut assignments = Vec::new();
                for p in pairs {
                    let mut member_init = p.into_inner();
                    let name = member_init.next().unwrap().as_str();
                    let expr = Self::parse_expr_pair(member_init.next().unwrap())?;
                    assignments.push((String::from(name), expr));
                }
                Ok(Expression::Construction { name: name, assignments: assignments, type_args: type_args })
            },
            Rule::r#if => {
                let mut pairs = pair.into_inner();
                let first_cond = Self::parse_expr_pair(pairs.next().unwrap())?;
                let first_body = Self::parse_body(pairs.next().unwrap())?;
                let elifs_pair = pairs.next().unwrap();
                let elifs_pairs = elifs_pair.into_inner();
                let mut elifs = Vec::<ConditionBody>::new();
                for p in elifs_pairs {
                    elifs.push(Self::parse_condition_body(p)?);
                }
                let op_else_pair = pairs.next();
                let else_body = 
                    if let Some(else_pair) = op_else_pair {
                        Some(Box::new(Self::parse_body(else_pair.into_inner().next().unwrap())?))
                    } else {
                        None
                    };

                Ok(Expression::IfStatement { 
                    first: Box::new(ConditionBody { condition: first_cond, body: first_body }),
                    conds: elifs,
                    last: else_body,
                })
            },
            Rule::unOpAtom => {
                let mut pairs = pair.into_inner();
                let unop = Self::parse_unop(pairs.next().unwrap())?;
                let expr = Self::parse_expr_pair(pairs.next().unwrap())?;
                Ok(Expression::UnaryOperation { op: unop, exp: Box::new(expr) })
            },
            Rule::listLiteral => {
                let mut pairs = pair.into_inner();
                let items = Self::parse_expr_list(pairs.next().unwrap())?;
                Ok(Expression::ListLiteral(items))
            },
            Rule::tuple => {
                let items = Self::parse_expr_list(pair)?;
                Ok(Expression::Tuple(items))
            },
            _ => Err(ParseError::FailAtom(String::from(pair.as_str()))),
        }
    }

    fn handle_expr_tail_pair(pair: Pair<Rule>, exp: Expression) -> Result<Expression, ParseError> {
        match pair.as_rule() {
            Rule::exprTail => {
                if let Some(tail_pair) = pair.into_inner().next() {
                    match tail_pair.as_rule() {
                        Rule::postfixBangTail => {
                            Ok(Self::handle_expr_tail_pair(
                                tail_pair.into_inner().next().unwrap(),
                                Expression::Bang(Box::new(exp))
                            )?)
                        },
                        Rule::memberAccessTail => {
                            let mut pairs = tail_pair.into_inner();
                            let member = pairs.next().unwrap().as_str();
                            Ok(Self::handle_expr_tail_pair(
                                pairs.next().unwrap(),
                                Expression::MemberAccess(Box::new(exp), String::from(member))
                            )?)
                        },
                        Rule::memberCallTail => {
                            let mut pairs = tail_pair.into_inner().peekable();
                            let member = pairs.next().unwrap().as_str();
                            let next_pair = pairs.peek().unwrap();
                            let type_args;
                            if let Rule::typeList = next_pair.as_rule() {
                                type_args = Some(Self::parse_type_list(pairs.next().unwrap())?);
                            } else {
                                type_args = None;
                            }
                            let args_pair = pairs.next().unwrap();
                            let args = Self::parse_expr_list(args_pair)?;
                            Ok(Self::handle_expr_tail_pair(
                                pairs.next().unwrap(),
                                Expression::MemberCall { callee: Box::new(exp), member: String::from(member), args, type_args }
                            )?)
                        },
                        Rule::indexTail => {
                            let mut pairs = tail_pair.into_inner();
                            let args_pair = pairs.next().unwrap();
                            let args = Self::parse_expr_list(args_pair)?;
                            Ok(Self::handle_expr_tail_pair(
                                pairs.next().unwrap(),
                                Expression::MemberCall { callee: Box::new(exp), member: String::from(INDEX_GET_FN_NAME), args, type_args: None, }
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

    fn parse_condition_body(pair: Pair<Rule>) -> Result<ConditionBody, ParseError> {
        let mut pairs = pair.into_inner();
        let cond = Self::parse_expr_pair(pairs.next().unwrap())?;
        let body = Self::parse_body(pairs.next().unwrap())?;
        Ok(ConditionBody {
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

    fn parse_type_pair(pair: Pair<Rule>) -> Result<CortexType, ParseError> {
        let pair_str = pair.as_str();
        let optional = pair_str.ends_with("?");
        let main = pair.into_inner().next().unwrap();
        match main.as_rule() {
            Rule::basicType => {
                let mut pairs = main.into_inner();
                let ident = Self::parse_path_ident(pairs.next().unwrap())?;
                let mut type_args = Vec::new();
                if let Some(type_args_top_pair) = pairs.next() {
                    let type_arg_pairs = type_args_top_pair.into_inner();
                    for typ_pair in type_arg_pairs {
                        type_args.push(Self::parse_type_pair(typ_pair)?);
                    }
                }
                Ok(CortexType::basic(ident, optional, type_args))
            },
            Rule::refType => {
                let typ = Self::parse_type_pair(main.into_inner().next().unwrap())?;
                let mutable = pair_str.starts_with("&mut");
                Ok(CortexType::reference(typ, mutable))
            },
            Rule::tupleType => {
                let pairs = main.into_inner();
                let mut types = Vec::new();
                for p in pairs {
                    types.push(Self::parse_type_pair(p)?);
                }
                Ok(CortexType::tuple(types, optional))
            },
            _ => Err(ParseError::FailType(String::from(pair_str)))
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
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;
        let mut type_args = Vec::new();
        let next = pairs.next().unwrap();
        let field_params;
        if matches!(next.as_rule(), Rule::typeArgList) {
            let type_arg_pairs = next.into_inner();
            for ident in type_arg_pairs {
                type_args.push(ident.as_str());
            }
            field_params = Self::parse_param_list(pairs.next().unwrap())?;
        } else {
            field_params = Self::parse_param_list(next)?;
        }
        let mut fields = HashMap::new();
        for p in field_params {
            fields.insert(p.name, p.typ);
        }
        
        Ok(
            Struct { 
                name: name,
                fields: fields,
                type_param_names: type_args.into_iter().map(|s| String::from(s)).collect(),
            }
        )
    }
    fn parse_bundle_pair(pair: Pair<Rule>) -> Result<Bundle, ParseError> {
        let mut pairs = pair.into_inner();
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;
        let mut type_args = Vec::new();
        let next = pairs.next().unwrap();
        let field_params;
        if matches!(next.as_rule(), Rule::typeArgList) {
            let type_arg_pairs = next.into_inner();
            for ident in type_arg_pairs {
                type_args.push(ident.as_str());
            }
            field_params = Self::parse_param_list(pairs.next().unwrap())?;
        } else {
            field_params = Self::parse_param_list(next)?;
        }
        let functions = Self::parse_bundle_func_list(pairs.next().unwrap())?;

        let mut fields = HashMap::new();
        for p in field_params {
            fields.insert(p.name, p.typ);
        }
        
        Ok(
            Bundle { 
                name: name,
                fields: fields,
                functions: functions,
                type_param_names: type_args.into_iter().map(|s| String::from(s)).collect(),
            }
        )
    }

    fn parse_func_pair(pair: Pair<Rule>) -> Result<Function, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;

        let mut type_args = Vec::new();
        let next = pairs.next().unwrap();
        let params;
        if matches!(next.as_rule(), Rule::typeArgList) {
            let type_arg_pairs = next.into_inner();
            for ident in type_arg_pairs {
                type_args.push(ident.as_str());
            }
            params = Self::parse_param_list(pairs.next().unwrap())?;
        } else {
            params = Self::parse_param_list(next)?;
        }

        let return_type = if matches!(pairs.peek().unwrap().as_rule(), Rule::typ) {
            Self::parse_type_pair(pairs.next().unwrap())?
        } else {
            CortexType::void(false)
        };
        let body = Self::parse_body(pairs.next().unwrap())?;
        Ok(Function {
            name: name,
            params: params,
            return_type: return_type,
            body: Body::Basic(body),
            this_arg: ThisArg::None,
            type_param_names: type_args.into_iter().map(|s| String::from(s)).collect(),
        })
    }
    fn parse_bundle_function(pair: Pair<Rule>) -> Result<Function, ParseError> {
        let mut pairs = pair.into_inner().peekable();
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;

        let mut type_args = Vec::new();
        let next = pairs.next().unwrap();
        let this_arg;
        if matches!(next.as_rule(), Rule::typeArgList) {
            let type_arg_pairs = next.into_inner();
            for ident in type_arg_pairs {
                type_args.push(ident.as_str());
            }
            this_arg = Self::parse_this_arg(pairs.next().unwrap())?;
        } else {
            this_arg = Self::parse_this_arg(next)?;
        }
        
        let params = Self::parse_param_list(pairs.next().unwrap())?;
        
        let return_type = if matches!(pairs.peek().unwrap().as_rule(), Rule::typ) {
            Self::parse_type_pair(pairs.next().unwrap())?
        } else {
            CortexType::void(false)
        };
        let body = Self::parse_body(pairs.next().unwrap())?;
        Ok(Function {
            name: name,
            params: params,
            return_type: return_type,
            body: Body::Basic(body),
            this_arg: this_arg,
            type_param_names: type_args.into_iter().map(|s| String::from(s)).collect(),
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

    fn parse_bundle_func_list(pair: Pair<Rule>) -> Result<Vec<Function>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_bundle_function(p)?);
        }
        Ok(result)
    }

    fn parse_expr_list(pair: Pair<Rule>) -> Result<Vec<Expression>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_expr_pair(p)?);
        }
        Ok(result)
    }

    fn parse_type_list(pair: Pair<Rule>) -> Result<Vec<CortexType>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_type_pair(p)?);
        }
        Ok(result)
    }

    fn parse_param_list(pair: Pair<Rule>) -> Result<Vec<Parameter>, ParseError> {
        let pairs = pair.into_inner();
        let mut result = Vec::new();
        for p in pairs {
            result.push(Self::parse_param(p)?);
        }
        Ok(result)
    }
    fn parse_param(pair: Pair<Rule>) -> Result<Parameter, ParseError> {
        let mut pairs = pair.into_inner();
        let ident = pairs.next().unwrap().as_str();
        let typ = Self::parse_type_pair(pairs.next().unwrap())?;
        Ok(
            Parameter {
                name: String::from(ident),
                typ: typ,
            }
        )
    }

    fn parse_body(pair: Pair<Rule>) -> Result<BasicBody, ParseError> {
        let mut result: Option<Expression> = None;
        let mut statements = Vec::<Statement>::new();

        let mut iter = pair.into_inner().peekable();
        while let Some(p) = iter.next() {
            let is_last = iter.peek().is_none();
            if is_last {
                if p.as_rule() == Rule::expr {
                    result = Some(Self::parse_expr_pair(p)?);
                } else {
                    statements.push(Self::parse_stmt_pair(p)?);
                }
            } else {
                statements.push(Self::parse_stmt_pair(p)?);
            }
        }
        Ok(
            BasicBody {
                statements: statements,
                result: result,
            }
        )
    }
}
