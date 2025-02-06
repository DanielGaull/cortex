use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

use super::ast::{expression::{Atom, Expression, ExpressionTail, OptionalIdentifier, Parameter, PathIdent}, statement::Statement, top_level::{Body, Function, TopLevel}, r#type::CortexType};

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
}

impl CortexParser {
    pub fn parse_statement(input: &str) -> Result<Statement, ParseError> {
        let pair = PestCortexParser::parse(Rule::statement, input);
        match pair {
            Ok(mut v) => Self::parse_stmt_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailStatement(String::from(input))),
        }
    }
    pub fn parse_expression(input: &str) -> Result<Expression, ParseError> {
        let pair = PestCortexParser::parse(Rule::expr, input);
        match pair {
            Ok(mut v) => Self::parse_expr_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailExpression(String::from(input))),
        }
    }
    pub fn parse_type(input: &str) -> Result<CortexType, ParseError> {
        let pair = PestCortexParser::parse(Rule::typ, input);
        match pair {
            Ok(mut v) => Self::parse_type_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailType(String::from(input))),
        }
    }
    pub fn parse_function(input: &str) -> Result<Function, ParseError> {
        let pair = PestCortexParser::parse(Rule::function, input);
        match pair {
            Ok(mut v) => Self::parse_func_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailType(String::from(input))),
        }
    }
    pub fn parse_top_level(input: &str) -> Result<TopLevel, ParseError> {
        let pair = PestCortexParser::parse(Rule::topLevel, input);
        match pair {
            Ok(mut v) => Self::parse_toplevel_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailType(String::from(input))),
        }
    }
    pub fn parse_path(input: &str) -> Result<PathIdent, ParseError> {
        let pair = PestCortexParser::parse(Rule::pathIdent, input);
        match pair {
            Ok(mut v) => Self::parse_path_ident(v.next().unwrap()),
            Err(_) => Err(ParseError::FailType(String::from(input))),
        }
    }

    fn parse_toplevel_pair(mut pair: Pair<Rule>) -> Result<TopLevel, ParseError> {
        pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::function => {
                Ok(TopLevel::Function(Self::parse_func_pair(pair)?))
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
                Ok(Atom::Expression(Box::new(Self::parse_expr_pair(pair)?)))
            },
            Rule::call => {
                let mut pairs = pair.into_inner();
                let name = Self::parse_path_ident(pairs.next().unwrap())?;
                let mut args: Vec<Expression> = Vec::new();
                for arg in pairs {
                    let parsed_arg = Self::parse_expr_pair(arg)?;
                    args.push(parsed_arg);
                }
                Ok(Atom::Call(name, args))
            },
            _ => Err(ParseError::FailAtom(String::from(pair.as_str()))),
        }
    }

    fn parse_expr_tail_pair(pair: Pair<Rule>) -> Result<ExpressionTail, ParseError> {
        match pair.as_rule() {
            Rule::exprTail => {
                if let Some(tail_pair) = pair.into_inner().next() {
                    match tail_pair.as_rule() {
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

    fn parse_type_pair(pair: Pair<Rule>) -> Result<CortexType, ParseError> {
        let nullable = pair.as_str().contains("?");
        let ident = pair.into_inner().next().unwrap().as_str();
        if ident == "any" {
            Ok(CortexType::any(nullable))
        } else {
            Ok(CortexType::new(ident, nullable))
        }
    }

    fn parse_opt_ident(pair: Pair<Rule>) -> Result<OptionalIdentifier, ParseError> {
        if pair.as_str() == "~" {
            Ok(OptionalIdentifier::Ignore)
        } else {
            Ok(OptionalIdentifier::Ident(String::from(pair.as_str())))
        }
    }

    fn parse_func_pair(pair: Pair<Rule>) -> Result<Function, ParseError> {
        let mut pairs = pair.into_inner();
        let name = Self::parse_opt_ident(pairs.next().unwrap())?;
        let params = Self::parse_param_list(pairs.next().unwrap())?;
        let return_type = Self::parse_type_pair(pairs.next().unwrap())?;
        let body = Self::parse_body(pairs.next().unwrap())?;
        Ok(Function {
            name: name,
            params: params,
            return_type: return_type,
            body: body,
        })
    }

    fn parse_param_list(pair: Pair<Rule>) -> Result<Vec<Parameter>, ParseError> {
        let pairs = pair.into_inner();
        let mut params = Vec::<Parameter>::new();
        for p in pairs {
            params.push(Self::parse_param(p)?);
        }
        Ok(params)
    }
    fn parse_param(pair: Pair<Rule>) -> Result<Parameter, ParseError> {
        let mut pairs = pair.into_inner();
        let ident = Self::parse_opt_ident(pairs.next().unwrap())?;
        let typ = Self::parse_type_pair(pairs.next().unwrap())?;
        Ok(
            Parameter {
                name: ident,
                typ: typ,
            }
        )
    }

    fn parse_body(pair: Pair<Rule>) -> Result<Body, ParseError> {
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
            Body::Basic {
                statements: statements,
                result: result,
            }
        )
    }
}
