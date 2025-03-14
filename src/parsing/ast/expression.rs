use thiserror::Error;

use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::{top_level::BasicBody, r#type::CortexType};

macro_rules! operator_struct {
    ($name:ident, $item: ty) => {
        #[derive(Clone)]
        pub struct $name {
            pub(crate) first: $item,
            pub(crate) rest: Vec<(BinaryOperator, $item)>,
        }

        impl SimpleCodeGen for $name {
            fn codegen(&self, indent: usize) -> String {
                let mut s = String::new();
                s.push_str(&self.first.codegen(indent));
                for (op, item) in &self.rest {
                    s.push_str(" ");
                    s.push_str(&op.codegen(indent));
                    s.push_str(" ");
                    s.push_str(&item.codegen(indent));
                }
                s
            }
        }
    }
}

operator_struct!(MulResult, Primary);
operator_struct!(SumResult, MulResult);
operator_struct!(EqResult, SumResult);
operator_struct!(Expression, EqResult);

#[derive(Clone)]
pub struct Primary {
    pub(crate) atom: Atom,
    pub(crate) tail: ExpressionTail,
}
impl SimpleCodeGen for Primary {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.atom.codegen(indent));
        s.push_str(&self.tail.codegen(indent));
        s
    }
}

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
pub enum Atom {
    Number(f64),
    Boolean(bool),
    Void,
    Null,
    String(String),
    PathIdent(PathIdent),
    Call(PathIdent, Vec<Expression>),
    Construction {
        name: PathIdent,
        type_args: Vec<CortexType>,
        assignments: Vec<(String, Expression)>
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
    Expression(Box<Expression>),
}
impl SimpleCodeGen for Atom {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Atom::Number(v) => format!("{}", v),
            Atom::Boolean(v) => format!("{}", v),
            Atom::String(v) => format!("\"{}\"", v),
            Atom::Void => String::from("void"),
            Atom::Null => String::from("null"),
            Atom::PathIdent(path) => path.codegen(indent),
            Atom::Expression(expr) => format!("({})", expr.codegen(indent)),
            Atom::Call(path, args) => {
                let mut s = String::new();
                s.push_str(&path.codegen(indent));
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
            Atom::Construction { name, type_args, assignments } => {
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
            Atom::IfStatement { first, conds, last } => {
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
            Atom::UnaryOperation { op, exp } => {
                let mut s = String::new();
                s.push_str(&op.codegen(indent));
                s.push_str(&exp.codegen(indent));
                s
            },
            Atom::ListLiteral(items) => {
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
        }
    }
}

#[derive(Clone)]
pub enum ExpressionTail {
    None,
    PostfixBang {
        next: Box<ExpressionTail>,
    },
    MemberAccess {
        member: String,
        next: Box<ExpressionTail>,
    },
    MemberCall {
        member: String,
        args: Vec<Expression>,
        next: Box<ExpressionTail>,
    },
}
impl SimpleCodeGen for ExpressionTail {
    fn codegen(&self, indent: usize) -> String {
        match self {
            ExpressionTail::None => String::new(),
            ExpressionTail::PostfixBang { next } => {
                let next = next.codegen(indent);
                format!("!{}", next)
            },
            ExpressionTail::MemberAccess { member, next } => {
                let next = next.codegen(indent);
                format!(".{}{}", member, next)
            },
            ExpressionTail::MemberCall { member, args, next } => {
                let next = next.codegen(indent);
                let args = args.iter().map(|x| x.codegen(0)).collect::<Vec<_>>().join(", ");
                format!(".{}({}){}", member, args, next)
            },
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
        let mut tail: ExpressionTail = ExpressionTail::None;
        let mut chain = self.chain;
        chain.reverse();
        for link in chain {
            tail = ExpressionTail::MemberAccess { member: link, next: Box::new(tail) };
        }

        Expression {
            first: EqResult {
                first: SumResult {
                    first: MulResult {
                        first: Primary {
                            atom: Atom::PathIdent(PathIdent::simple(self.base)),
                            tail: tail,
                        },
                        rest: vec![],
                    },
                    rest: vec![],
                },
                rest: vec![],
            },
            rest: vec![],
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

#[derive(Clone, Debug, PartialEq)]
pub struct PathIdent {
    pub(crate) path: Vec<String>,
}
impl SimpleCodeGen for PathIdent {
    fn codegen(&self, _: usize) -> String {
        let mut s = String::new();
        for (i, p) in self.path.iter().enumerate() {
            s.push_str(p);
            if i + 1 < self.path.len() {
                s.push_str("::");
            }
        }
        s
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
        let mut path = first.path.clone();
        path.push(next);
        Self {
            path: path,
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
