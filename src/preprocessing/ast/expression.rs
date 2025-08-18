use crate::{joint::vtable::VTable, parsing::ast::expression::{BinaryOperator, IdentExpression, UnaryOperator}};

use super::{function::RDefinedBody, statement::RConditionBody};

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
