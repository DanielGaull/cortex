use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::typ::CType;

pub struct Expression {
    pub(crate) atom: Atom,
    pub(crate) tail: ExpressionTail,
}
impl SimpleCodeGen for Expression {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(self.atom.codegen(indent).as_str());
        s.push_str(self.tail.codegen(indent).as_str());
        s
    }
}

pub enum Atom {
    Number(f64),
    Boolean(bool),
    Void,
    Null,
    String(String),
    PathIdent(PathIdent),
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
            Atom::Expression(expr) => expr.codegen(indent),
        }
    }
}

pub enum ExpressionTail {
    None,
    Call {
        args: Vec<Expression>,
        next: Box<ExpressionTail>,
    },
}
impl SimpleCodeGen for ExpressionTail {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Self::None => String::new(),
            Self::Call { args, next } => {
                let mut s = String::new();
                s.push_str("(");
                for (i, arg) in args.iter().enumerate() {
                    s.push_str(arg.codegen(indent).as_str());
                    if i + 1 < args.len() {
                        s.push_str(", ");
                    }
                }
                s.push_str(")");
                s.push_str(next.codegen(indent).as_str());
                s
            }
        }
    }
}

pub struct Parameter {
    name: String,
    typ: CType,
}
impl SimpleCodeGen for Parameter {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(self.name.as_str());
        s.push_str(": ");
        s.push_str(self.typ.codegen(indent).as_str());
        s
    }
}

pub enum OptionalIdentifier {
    Ident(String), // A true identifier
    Ignore, // The ignore token, "~"
}
impl SimpleCodeGen for OptionalIdentifier {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Self::Ident(s) => s.clone(),
            Self::Ignore => String::from("~"),
        }
    }
}

pub struct PathIdent {
    path: Vec<String>,
}
impl SimpleCodeGen for PathIdent {
    fn codegen(&self, _: usize) -> String {
        let mut s = String::new();
        for (i, p) in self.path.iter().enumerate() {
            s.push_str(p.as_str());
            if i + 1 < self.path.len() {
                s.push_str("::");
            }
        }
        s
    }
}
