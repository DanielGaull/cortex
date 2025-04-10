use thiserror::Error;
use std::error::Error;

use crate::{parsing::codegen::r#trait::SimpleCodeGen, preprocessing::ast::function_address::FunctionAddress};

use super::{top_level::BasicBody, r#type::CortexType};

#[derive(Clone)]
pub struct ConditionBody {
    pub(crate) condition: Expression,
    pub(crate) body: BasicBody,
}
impl SimpleCodeGen for ConditionBody {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.condition.codegen(indent));
        s.push_str(" {\n");
        s.push_str(&self.body.codegen(indent + 1));
        s.push_str(&"    ".repeat(indent));
        s.push_str("}");
        s
    }
}

#[derive(Clone)]
pub enum Expression {
    Number(f64),
    Boolean(bool),
    Void,
    None,
    String(String),
    PathIdent(PathIdent),
    Call {
        name: FunctionAddress, 
        args: Vec<Expression>,
        type_args: Option<Vec<CortexType>>,
    },
    Construction {
        name: PathIdent,
        type_args: Vec<CortexType>,
        assignments: Vec<(String, Expression)>,
    },
    IfStatement {
        first: Box<ConditionBody>,
        conds: Vec<ConditionBody>,
        last: Option<Box<BasicBody>>,
    },
    UnaryOperation {
        op: UnaryOperator,
        exp: Box<Expression>,
    },
    ListLiteral(Vec<Expression>),
    Bang(Box<Expression>),
    MemberAccess(Box<Expression>, String),
    MemberCall {
        callee: Box<Expression>,
        member: String,
        args: Vec<Expression>,
        type_args: Option<Vec<CortexType>>,
    },
    BinaryOperation {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    Tuple(Vec<Expression>),
}
impl SimpleCodeGen for Expression {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Expression::Number(v) => format!("{}", v),
            Expression::Boolean(v) => format!("{}", v),
            Expression::String(v) => format!("\"{}\"", v),
            Expression::Void => String::from("void"),
            Expression::None => String::from("none"),
            Expression::PathIdent(path) => path.codegen(indent),
            Expression::Call{ name, args, type_args } => {
                let mut s = String::new();
                s.push_str(&name.codegen(indent));
                if let Some(type_args) = type_args {
                    s.push_str("<");
                    s.push_str(&type_args.iter().map(|t| t.codegen(indent)).collect::<Vec<_>>().join(", "));
                    s.push_str(">");
                }
                s.push_str("(");
                for (i, arg) in args.iter().enumerate() {
                    s.push_str(&arg.codegen(indent));
                    if i + 1 < args.len() {
                        s.push_str(", ");
                    }
                }
                s.push_str(")");
                s
            },
            Expression::Construction { name, type_args, assignments } => {
                let mut s = String::new();
                s.push_str(&name.codegen(0));
                if type_args.len() > 0 {
                    s.push_str("<");
                    s.push_str(&type_args.iter().map(|s| s.codegen(0)).collect::<Vec<_>>().join(","));
                    s.push_str(">");
                }
                s.push_str(" { ");
                for a in assignments {
                    s.push_str(&a.0);
                    s.push_str(": ");
                    s.push_str(&a.1.codegen(0));
                    s.push_str(", ");
                }
                s.push_str("}");
                s
            },
            Expression::IfStatement { first, conds, last } => {
                let mut s = String::new();
                let indent_prefix = "    ".repeat(indent);
                s.push_str(&indent_prefix);
                s.push_str("if ");
                s.push_str(&first.codegen(indent));
                for c in conds {
                    s.push_str(" elif ");
                    s.push_str(&c.codegen(indent));
                }
                if let Some(body) = last {
                    s.push_str(" else {\n");
                    s.push_str(&body.codegen(indent + 1));
                    s.push_str(&indent_prefix);
                    s.push_str("}");
                }
                s
            },
            Expression::UnaryOperation { op, exp } => {
                format!("{}{}", op.codegen(indent), exp.codegen_as_sub(indent))
            },
            Expression::ListLiteral(items) => {
                let mut s = String::new();
                s.push_str("[");
                s.push_str(
                    &items
                        .iter()
                        .map(|e| e.codegen(0))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                s.push_str("]");
                s
            },
            Expression::Bang(ex) => format!("{}!", ex.codegen(indent)),
            Expression::BinaryOperation { left, op, right } => {
                format!("{} {} {}", left.codegen_as_sub(indent), op.codegen(indent), right.codegen_as_sub(indent))
            },
            Expression::MemberAccess(ex, member) => format!("{}.{}", ex.codegen_as_sub(indent), member),
            Expression::MemberCall { callee, member: member_name, args, type_args } => {
                if let Some(type_args) = type_args {
                    format!("{}.{}<{}>({})", 
                        callee.codegen_as_sub(indent), 
                        member_name, 
                        type_args.iter().map(|t| t.codegen(indent)).collect::<Vec<_>>().join(", "),
                        args.iter().map(|a| a.codegen(indent)).collect::<Vec<_>>().join(", ")
                    )
                } else {
                    format!("{}.{}({})", 
                        callee.codegen_as_sub(indent), 
                        member_name, 
                        args.iter().map(|a| a.codegen(indent)).collect::<Vec<_>>().join(", ")
                    )
                }
            },
            Expression::Tuple(items) => {
                format!("({})", items.iter().map(|i| i.codegen(indent)).collect::<Vec<_>>().join(", "))
            }
        }
    }
}
impl Expression {
    fn is_atomic(&self) -> bool {
        match self {
            Expression::Number(_) | Expression::Boolean(_) | Expression::Void | Expression::None | 
            Expression::String(_) | Expression::PathIdent(_) | Expression::Call { name: _, args: _, type_args: _ } |
            Expression::Construction { name: _, type_args: _, assignments: _ } |
            Expression::IfStatement { first: _, conds: _, last: _ } | Expression::MemberAccess(_, _) |
            Expression::ListLiteral(_) | Expression::MemberCall { callee: _, member: _, args: _, type_args: _ } |
            Expression::Tuple(_)
                => true,
            
            Expression::UnaryOperation { op: _, exp: _ } | Expression::Bang(_) | 
            Expression::BinaryOperation { left: _, op: _, right: _ }
                => false,
        }
    }
    fn codegen_as_sub(&self, indent: usize) -> String {
        let s = self.codegen(indent);
        if !self.is_atomic() {
            format!("({})", s)
        } else {
            s
        }
    }
}

#[derive(Clone)]
pub struct Parameter {
    pub(crate) name: String,
    pub(crate) typ: CortexType,
}
impl SimpleCodeGen for Parameter {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.name);
        s.push_str(": ");
        s.push_str(&self.typ.codegen(indent));
        s
    }
}
impl Parameter {
    pub fn named(name: &str, typ: CortexType) -> Self {
        Parameter {
            name: String::from(name),
            typ: typ,
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }
    pub fn param_type(&self) -> &CortexType {
        &self.typ
    }
}

#[derive(Clone)]
pub struct IdentExpression {
    pub(crate) base: String,
    pub(crate) chain: Vec<String>,
}
impl SimpleCodeGen for IdentExpression {
    fn codegen(&self, _indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.base);
        for c in &self.chain {
            s.push_str(".");
            s.push_str(c);
        }
        s
    }
}
impl IdentExpression {
    pub fn is_simple(&self) -> bool {
        self.chain.is_empty()
    }

    pub fn to_member_access_expr(self) -> Expression {
        let mut expr = Expression::PathIdent(PathIdent::simple(self.base));
        for link in self.chain {
            expr = Expression::MemberAccess(Box::new(expr), link);
        }
        expr
    }

    pub fn without_last(mut self) -> Result<IdentExpression, Box<dyn Error>> {
        // TODO: should return error rather than returning self
        if self.chain.len() > 0 {
            self.chain.remove(self.chain.len() - 1);
            Ok(IdentExpression {
                base: self.base,
                chain: self.chain,
            })
        } else {
            Ok(self)
        }
    }
}

#[derive(Clone)]
pub enum OptionalIdentifier {
    Ident(String), // A true identifier
    Ignore, // The ignore token, "~"
}
impl SimpleCodeGen for OptionalIdentifier {
    fn codegen(&self, _: usize) -> String {
        match self {
            Self::Ident(s) => s.clone(),
            Self::Ignore => String::from("~"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PathIdent {
    pub(crate) path: Vec<String>,
}
impl SimpleCodeGen for PathIdent {
    fn codegen(&self, _: usize) -> String {
        self.to_string("::")
    }
}
#[derive(Error, Debug, PartialEq)]
pub enum PathError {
    #[error("Path is empty")]
    PathEmpty,
    #[error("Subtraction failed: item {0} does not match {1}")]
    SubtractionFailed(String, String),
}
impl PathIdent {
    pub fn simple(name: String) -> Self {
        Self {
            path: vec![name],
        }
    }
    pub fn new(name: Vec<&str>) -> Self {
        Self {
            path: name.iter().map(|s| String::from(*s)).collect(),
        }
    }
    pub fn continued(first: PathIdent, next: String) -> Self {
        if first.is_empty() {
            Self {
                path: vec![next],
            }
        } else {
            let mut path = first.path.clone();
            path.push(next);
            Self {
                path: path,
            }
        }
    }
    pub fn empty() -> Self {
        Self {
            path: Vec::new(),
        }
    }
    pub fn concat(first: &PathIdent, second: &PathIdent) -> Self {
        if first.is_empty() {
            second.clone()
        } else if second.is_empty() {
            first.clone()
        } else {
            let mut path = first.path.clone();
            path.extend(second.path.clone());
            Self {
                path: path,
            }
        }
    }

    pub fn is_prefixed_by(&self, prefix: &PathIdent) -> bool {
        if prefix.path.len() > self.path.len() {
            return false;
        }
        for (i, p) in prefix.path.iter().enumerate() {
            if *self.path.get(i).unwrap() != *p {
                return false;
            }
        }
        true
    }

    pub fn subtract(self, second: &PathIdent) -> Result<Self, PathError> {
        if second.is_empty() {
            Ok(self)
        } else {
            let mut path = self.path.clone();
            for item in &second.path {
                if let Some(first) = path.get(0) {
                    if first != item {
                        return Err(PathError::SubtractionFailed(first.clone(), item.clone()));
                    }
                    path.remove(0);
                } else {
                    return Err(PathError::PathEmpty);
                }
            }
            Ok(
                Self {
                    path: path
                }
            )
        }
    }
    pub fn subtract_if_possible(self, second: &PathIdent) -> Self {
        if self.is_prefixed_by(second) {
            self.subtract(second).unwrap()
        } else {
            self
        }
    }

    pub fn without_last(&self) -> Self {
        let mut new_vec = self.path.clone();
        new_vec.remove(new_vec.len() - 1);
        Self {
            path: new_vec,
        }
    }

    pub fn pop_front(&self) -> Result<PathIdent, PathError> {
        if self.path.len() <= 0 {
            Err(PathError::PathEmpty)
        } else {
            let new_path: Vec<String> = self.path.iter().skip(1).cloned().collect();
            Ok(PathIdent {
                path: new_path,
            })
        }
    }
    pub fn get_front(&self) -> Result<&String, PathError> {
        if let Some(elem) = self.path.get(0) {
            Ok(elem)
        } else {
            Err(PathError::PathEmpty)
        }
    }
    pub fn get_back(&self) -> Result<&String, PathError> {
        if let Some(elem) = self.path.get(self.path.len() - 1) {
            Ok(elem)
        } else {
            Err(PathError::PathEmpty)
        }
    }
    // Returns true if there is only one segment in this path left
    // Error if the path is empty
    pub fn is_final(&self) -> bool {
        if self.path.len() <= 0 {
            false
        } else {
            self.path.len() == 1
        }
    }
    pub fn is_empty(&self) -> bool {
        self.path.len() <= 0
    }

    pub fn to_string(&self, separator: &str) -> String {
        self.path.iter().cloned().collect::<Vec<_>>().join(separator)
    }
}

#[derive(Clone)]
pub enum UnaryOperator {
    Negate,
    Invert,
}
impl SimpleCodeGen for UnaryOperator {
    fn codegen(&self, _indent: usize) -> String {
        String::from(
            match self {
                UnaryOperator::Negate => "-",
                UnaryOperator::Invert => "!",
            }
        )
    }
}

#[derive(Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LogicAnd,
    LogicOr,
    IsEqual,
    IsNotEqual,
    IsLessThan,
    IsGreaterThan,
    IsLessThanOrEqualTo,
    IsGreaterThanOrEqualTo,
}
impl SimpleCodeGen for BinaryOperator {
    fn codegen(&self, _: usize) -> String {
        String::from(
            match self {
                BinaryOperator::Add => "+",
                BinaryOperator::Subtract => "-",
                BinaryOperator::Multiply => "*",
                BinaryOperator::Divide => "/",
                BinaryOperator::Remainder => "%",
                BinaryOperator::LogicAnd => "&&",
                BinaryOperator::LogicOr => "||",
                BinaryOperator::IsEqual => "==",
                BinaryOperator::IsNotEqual => "!=",
                BinaryOperator::IsLessThan => "<",
                BinaryOperator::IsGreaterThan => ">",
                BinaryOperator::IsLessThanOrEqualTo => "<=",
                BinaryOperator::IsGreaterThanOrEqualTo => ">=",
            }
        )
    }
}
