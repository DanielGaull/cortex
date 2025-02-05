use thiserror::Error;

use crate::parsing::codegen::r#trait::SimpleCodeGen;

use super::typ::CType;

pub struct Expression {
    pub(crate) atom: Atom,
    pub(crate) tail: ExpressionTail,
}
impl SimpleCodeGen for Expression {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.atom.codegen(indent));
        s.push_str(&self.tail.codegen(indent));
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
    Call(PathIdent, Vec<Expression>),
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
            }
        }
    }
}

pub enum ExpressionTail {
    None,
}
impl SimpleCodeGen for ExpressionTail {
    fn codegen(&self, _: usize) -> String {
        match self {
            Self::None => String::new(),
        }
    }
}

pub struct Parameter {
    pub(crate) name: OptionalIdentifier,
    pub(crate) typ: CType,
}
impl SimpleCodeGen for Parameter {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        s.push_str(&self.name.codegen(indent));
        s.push_str(": ");
        s.push_str(&self.typ.codegen(indent));
        s
    }
}

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
#[derive(Error, Debug)]
pub enum PathError {
    #[error("Path is empty")]
    PathEmpty,
}
impl PathIdent {
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
    pub fn is_final(&self) -> Result<bool, PathError> {
        if self.path.len() <= 0 {
            Err(PathError::PathEmpty)
        } else {
            Ok(self.path.len() == 1)
        }
    }
    pub fn is_empty(&self) -> bool {
        self.path.len() <= 0
    }
}
