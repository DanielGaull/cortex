use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

use super::ast::{expression::{Atom, Expression, ExpressionTail, PathIdent}, statement::Statement, typ::CType};

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
    #[error("Failed to parse type '{0}'")]
    FailType(String),
}

impl CortexParser {
    pub fn parse_statement(input: &String) -> Result<Statement, ParseError> {
        let pair = PestCortexParser::parse(Rule::statement, input.as_str());
        match pair {
            Ok(mut v) => Self::parse_stmt_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailStatement(input.clone())),
        }
    }
    pub fn parse_expression(input: &String) -> Result<Expression, ParseError> {
        let pair = PestCortexParser::parse(Rule::expr, input.as_str());
        match pair {
            Ok(mut v) => Self::parse_expr_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailExpression(input.clone())),
        }
    }
    pub fn parse_type(input: &String) -> Result<CType, ParseError> {
        let pair = PestCortexParser::parse(Rule::typ, input.as_str());
        match pair {
            Ok(mut v) => Self::parse_type_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailType(input.clone())),
        }
    }

    fn parse_stmt_pair(mut pair: Pair<Rule>) -> Result<Statement, ParseError> {
        pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::expr => {
                let expression = Self::parse_expr_pair(pair)?;
                Ok(Statement::Expression(expression))
            },
            Rule::stop => {
                Ok(Statement::Stop)
            },
            Rule::varDec => {
                let is_const = pair.as_str().starts_with("const");
                let mut pairs = pair.into_inner();
                let name = pairs.next().unwrap().as_str();
                let third_pair = pairs.next().unwrap();
                let mut typ: Option<CType> = None;
                let init_value = 
                    if third_pair.as_rule() == Rule::typ {
                        typ = Some(Self::parse_type_pair(third_pair)?);
                        Self::parse_expr_pair(pairs.next().unwrap())?
                    } else {
                        Self::parse_expr_pair(third_pair)?
                    };
                Ok(Statement::VariableDeclaration { 
                    name: String::from(name),
                    is_const: is_const,
                    typ: typ,
                    initial_value: init_value,
                })
            },
            Rule::varAssign => {
                let mut pairs = pair.into_inner();
                let path = Self::parse_path_ident(pairs.next().unwrap())?;
                let value = Self::parse_expr_pair(pairs.next().unwrap())?;
                Ok(Statement::VariableAssignment { 
                    name: path, 
                    value: value,
                })
            },
            _ => Err(ParseError::FailStatement(String::from(pair.as_str()))),
        }
    }

    fn parse_expr_pair(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner();
        let atom_pair = pairs.next().unwrap();
        let atom = Self::parse_atom_pair(atom_pair.into_inner().next().unwrap())?;
        let mut expr_tail = ExpressionTail::None;
        let next = pairs.next();
        if next.is_some() {
            let expr_tail_pair = next.unwrap();
            expr_tail = Self::parse_expr_tail_pair(expr_tail_pair)?;
        }
        Ok(Expression {
            atom: atom,
            tail: expr_tail,
        })
    }

    fn parse_atom_pair(pair: Pair<Rule>) -> Result<Atom, ParseError> {
        match pair.as_rule() {
            Rule::number => {
                let value: f64 = pair.as_str().parse().unwrap();
                Ok(Atom::Number(value))
            },
            Rule::boolean => {
                let value: bool = pair.as_str().parse().unwrap();
                Ok(Atom::Boolean(value))
            },
            Rule::string => {
                Ok(Atom::String(String::from(pair.into_inner().next().unwrap().as_str())))
            },
            Rule::null => {
                Ok(Atom::Null)
            },
            Rule::void => {
                Ok(Atom::Void)
            },
            Rule::pathIdent => {
                Ok(Atom::PathIdent(Self::parse_path_ident(pair)?))
            },
            Rule::expr => {
                Ok(Atom::Expression(Box::new(Self::parse_expr_pair(pair.into_inner().next().unwrap())?)))
            },
            _ => Err(ParseError::FailAtom(String::from(pair.as_str()))),
        }
    }

    fn parse_expr_tail_pair(pair: Pair<Rule>) -> Result<ExpressionTail, ParseError> {
        match pair.as_rule() {
            Rule::exprTail => {
                if let Some(tail_pair) = pair.into_inner().next() {
                    match tail_pair.as_rule() {
                        Rule::callTail => {
                            let pairs = tail_pair.into_inner();
                            // Silent rule for the args, so last item will be the next tail
                            // Convert to Vec for easier handling
                            let mut items: Vec<Pair<'_, Rule>> = pairs.collect();
                            let next_tail = Self::parse_expr_tail_pair(items.pop().unwrap())?;
                            let mut args: Vec<Expression> = Vec::new();
                            for arg in items {
                                let parsed_arg = Self::parse_expr_pair(arg)?;
                                args.push(parsed_arg);
                            }
                            Ok(
                                ExpressionTail::Call {
                                    args: args,
                                    next: Box::new(next_tail),
                                }
                            )
                        },
                        _ => Err(ParseError::FailTail(String::from(tail_pair.as_str()))),
                    }
                } else {
                    Ok(ExpressionTail::None)
                }
            }
            _ => Err(ParseError::FailTail(String::from(pair.as_str()))),
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

    fn parse_type_pair(pair: Pair<Rule>) -> Result<CType, ParseError> {
        let nullable = pair.as_str().contains("?");
        let ident = pair.into_inner().next().unwrap().as_str();
        Ok(CType::Basic { name: String::from(ident), is_nullable: nullable })
    }
}
