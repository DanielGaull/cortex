use crate::{joint::vtable::VTable, parsing::{ast::expression::{BinaryOperator, IdentExpression, UnaryOperator}, codegen::r#trait::SimpleCodeGen}, preprocessing::preprocessor::preprocessor::CortexPreprocessor};

use super::{function::RDefinedBody, statement::RConditionBody};

pub trait RCodeGen {
    fn codegen(&self, indent: usize, preprocessor: &CortexPreprocessor) -> String;
}

#[derive(Clone)]
pub enum RExpression {
    F32(f32),
    F64(f64),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    ISZ(isize),
    USZ(usize),
    Boolean(bool),
    Void,
    None,
    String(String),
    Char(u8),
    Identifier(String),
    Call {
        addr: usize, 
        args: Vec<RExpression>,
    },
    Construction {
        assignments: Vec<(String, RExpression)>,
    },
    IfStatement {
        first: Box<RConditionBody>,
        conds: Vec<RConditionBody>,
        last: Option<Box<RDefinedBody>>,
    },
    UnaryOperation {
        op: UnaryOperator,
        exp: Box<RExpression>,
    },
    CollectionLiteral(Vec<RExpression>),
    Bang(Box<RExpression>),
    MemberAccess(Box<RExpression>, String),
    BinaryOperation {
        left: Box<RExpression>,
        op: BinaryOperator,
        right: Box<RExpression>,
    },
    Tuple(Vec<RExpression>),
    MakeFat(Box<RExpression>, VTable),
    FatCall {
        callee: Box<RExpression>,
        index_in_vtable: usize,
        args: Vec<RExpression>,
    },
    HeapAlloc(Box<RExpression>),
    DerefFat(Box<RExpression>),
    MakeAnon(Box<RExpression>),
    DeAnon(Box<RExpression>),
    FunctionPointerCall{
        ident: String,
        args: Vec<RExpression>,
    },
    MakeFunctionPointer(usize),
}
impl RCodeGen for RExpression {
    fn codegen(&self, indent: usize, preprocessor: &CortexPreprocessor) -> String {
        match self {
            RExpression::F32(v) => format!("{}f32", v),
            RExpression::F64(v) => format!("{}f64", v),
            RExpression::I8(v) => format!("{}i8", v),
            RExpression::U8(v) => format!("{}u8", v),
            RExpression::I16(v) => format!("{}i16", v),
            RExpression::U16(v) => format!("{}u16", v),
            RExpression::I32(v) => format!("{}i32", v),
            RExpression::U32(v) => format!("{}u32", v),
            RExpression::I64(v) => format!("{}i64", v),
            RExpression::U64(v) => format!("{}u64", v),
            RExpression::ISZ(v) => format!("{}isz", v),
            RExpression::USZ(v) => format!("{}usz", v),
            RExpression::Boolean(b) => format!("{}", b),
            RExpression::Void => String::from("void"),
            RExpression::None => String::from("none"),
            RExpression::String(s) => format!("\"{}\"", s),
            RExpression::Char(c) => format!("'{}'", c),
            RExpression::Identifier(i) => i.clone(),
            RExpression::Call { addr, args } => {
                let fn_name = preprocessor.function_dict.get_name(*addr).unwrap().codegen(indent);
                format!("{}({})", fn_name, args.iter().map(|a| a.codegen(indent, preprocessor)).collect::<Vec<_>>().join(", "))
            },
            RExpression::Construction { assignments } => {
                let mut s = String::from("<anonymous> {\n");
                for (n, v) in assignments {
                    s.push_str(&format!("    {}: {},\n", n, v.codegen(indent, preprocessor)));
                }
                s.push_str("}");
                s
            },
            RExpression::IfStatement { first, conds, last } => {
                let mut s = String::new();
                s.push_str("if ");
                s.push_str(&first.condition.codegen(indent, preprocessor));
                s.push_str(" { \n");
                s.push_str(&first.body.codegen(indent + 1, preprocessor));
                s.push_str("}");
                for cond in conds {
                    s.push_str(" elif ");
                    s.push_str(&cond.condition.codegen(indent, preprocessor));
                    s.push_str(" { \n");
                    s.push_str(&cond.body.codegen(indent + 1, preprocessor));
                    s.push_str("}");
                }
                if let Some(last) = last {
                    s.push_str(" else {\n");
                    s.push_str(&last.codegen(indent + 1, preprocessor));
                    s.push_str("}");
                }
                s
            },
            RExpression::UnaryOperation { op, exp } => format!("{}{}", op.codegen(indent), exp.codegen(indent, preprocessor)),
            RExpression::CollectionLiteral(exps) => format!("[{}]", exps.iter().map(|e| e.codegen(indent, preprocessor)).collect::<Vec<_>>().join(", ")),
            RExpression::Bang(inner) => format!("{}!", inner.codegen(indent, preprocessor)),
            RExpression::MemberAccess(base, member) => format!("{}.{}", base.codegen(indent, preprocessor), member),
            RExpression::BinaryOperation { left, op, right } =>
                format!("{} {} {}", left.codegen(indent, preprocessor), op.codegen(indent), right.codegen(indent, preprocessor)),
            RExpression::Tuple(exps) => {
                if exps.len() == 1 {
                    format!("({},)", exps.iter().map(|e| e.codegen(indent, preprocessor)).collect::<Vec<_>>().join(", "))
                } else {
                    format!("({})", exps.iter().map(|e| e.codegen(indent, preprocessor)).collect::<Vec<_>>().join(", "))
                }
            },
            RExpression::MakeFat(exp, _vtable) => exp.codegen(indent, preprocessor),
            RExpression::FatCall { callee, index_in_vtable, args } => {
                format!("{}.<fat{}>({})", callee.codegen(indent, preprocessor), index_in_vtable, args.iter().map(|a| a.codegen(indent, preprocessor)).collect::<Vec<_>>().join(", "))
            },
            RExpression::HeapAlloc(exp) => format!("heap {}", exp.codegen(indent, preprocessor)),
            RExpression::DerefFat(exp) => exp.codegen(indent, preprocessor),
            RExpression::MakeAnon(exp) => format!("anon {}", exp.codegen(indent, preprocessor)),
            RExpression::DeAnon(exp) => format!("deanon<?> {}", exp.codegen(indent, preprocessor)),
            RExpression::FunctionPointerCall { ident, args } => 
                format!("{}({})", ident, args.iter().map(|a| a.codegen(indent, preprocessor)).collect::<Vec<_>>().join(", ")),
            RExpression::MakeFunctionPointer(addr) => format!("func@0x{:x}", addr),
        }
    }
}

#[derive(Clone)]
pub struct RIdentExpression {
    pub(crate) base: String,
    pub(crate) chain: Vec<String>,
}
impl From<IdentExpression> for RIdentExpression {
    fn from(value: IdentExpression) -> Self {
        RIdentExpression {
            base: value.base,
            chain: value.chain,
        }
    }
}
impl RIdentExpression {
    pub fn is_simple(&self) -> bool {
        self.chain.is_empty()
    }
}
impl SimpleCodeGen for RIdentExpression {
    fn codegen(&self, _: usize) -> String {
        if self.is_simple() {
            self.base.clone()
        } else {
            format!("{}.{}", self.base, self.chain.join("."))
        }
    }
}
