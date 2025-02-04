use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

use super::ast::expression::{Atom, Expression, ExpressionTail};

#[derive(Parser)]
#[grammar = "grammar.pest"] // relative to src
struct PestCortexParser;

pub struct CortexParser;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Failed to parse expression '{0}'")]
    FailExpression(String),
    #[error("Failed to parse atom '{0}'")]
    FailAtom(String),
    #[error("Failed to parse expression tail '{0}'")]
    FailTail(String),
}

impl CortexParser {
    pub fn parse_expression(input: &String) -> Result<Expression, ParseError> {
        let pair = PestCortexParser::parse(Rule::expr, input.as_str());
        match pair {
            Ok(mut v) => Self::parse_expr_pair(v.next().unwrap()),
            Err(_) => Err(ParseError::FailExpression(input.clone())),
        }
    }

    fn parse_expr_pair(pair: Pair<Rule>) -> Result<Expression, ParseError> {
        let mut pairs = pair.into_inner();
        let atom_pair = pairs.next().unwrap();
        let atom = Self::parse_atom_pair(atom_pair.into_inner().next().unwrap())?;
        let mut expr_tail = ExpressionTail::None;
        let next = pairs.next().unwrap().into_inner().next();
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
            Rule::expr => {
                Ok(Atom::Expression(Box::new(Self::parse_expr_pair(pair.into_inner().next().unwrap())?)))
            },
            _ => Err(ParseError::FailAtom(String::from(pair.as_str()))),
        }
    }

    fn parse_expr_tail_pair(pair: Pair<Rule>) -> Result<ExpressionTail, ParseError> {
        let r = pair.as_rule();
        match pair.as_rule() {
            Rule::callTail => {
                let pairs = pair.into_inner();
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
            _ => Err(ParseError::FailTail(String::from(pair.as_str()))),
        }
    }
}
