use std::collections::HashMap;

use crate::{interpreting::{env::Environment, error::CortexError, heap::Heap, value::CortexValue}, parsing::codegen::r#trait::SimpleCodeGen};

use super::{expression::{PExpression, OptionalIdentifier, Parameter, PathIdent}, statement::PStatement, r#type::CortexType};

pub enum TopLevel {
    Import {
        name: String, 
        is_string_import: bool,
    },
    Module {
        name: String,
        contents: Vec<TopLevel>,
    },
    Function(PFunction),
    Struct(Struct),
    Bundle(Bundle),
    Extension(Extension),
}
impl SimpleCodeGen for TopLevel {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Self::Import { name, is_string_import } => {
                if *is_string_import {
                    format!("import \"{}\";", name)
                } else {
                    format!("import {};", name.clone())
                }
            },
            Self::Module { name, contents } => {
                let mut s = String::new();
                let indent_prefix = &"    ".repeat(indent);
                s.push_str(indent_prefix);
                s.push_str("module ");
                s.push_str(name);
                s.push_str(" {\n");

                for top in contents {
                    s.push_str(&top.codegen(indent + 1));
                    s.push_str("\n");
                }
                
                s.push_str(indent_prefix);
                s.push_str("}");
                s
            },
            Self::Function(func) => func.codegen(indent),
            Self::Struct(struc) => struc.codegen(indent),
            Self::Bundle(bundle) => bundle.codegen(indent),
            Self::Extension(extension) => extension.codegen(indent),
        }
    }
}

#[derive(PartialEq)]
pub enum ThisArg {
    RefThis,
    RefMutThis,
    DirectThis,
}

#[derive(Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CortexType,
    pub(crate) type_param_names: Vec<String>,
}

pub struct PFunction {
    pub(crate) name: OptionalIdentifier,
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CortexType,
    pub(crate) body: Body,
    pub(crate) type_param_names: Vec<String>,
}
impl SimpleCodeGen for PFunction {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = &"    ".repeat(indent);

        s.push_str(indent_prefix);
        s.push_str("fn ");
        s.push_str(&self.name.codegen(indent));

        if self.type_param_names.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_param_names.join(","));
            s.push_str(">");
        }

        s.push_str("(");

        for (i, param) in self.params.iter().enumerate() {
            s.push_str(&param.codegen(indent));
            if i + 1 < self.params.len() {
                s.push_str(", ");
            }
        }
        s.push_str("): ");
        s.push_str(&self.return_type.codegen(indent));
        s.push_str(" {\n");

        s.push_str(&self.body.codegen(indent + 1));

        s.push_str(indent_prefix);
        s.push_str("}");
        s
    }
}
impl PFunction {
    pub fn new(name: OptionalIdentifier, params: Vec<Parameter>, return_type: CortexType, body: Body, type_param_names: Vec<String>) -> Self {
        PFunction {
            name: name,
            params: params,
            return_type: return_type,
            body: body,
            type_param_names: type_param_names,
        }
    }

    pub fn name(&self) -> &OptionalIdentifier {
        &self.name
    }
    pub fn return_type(&self) -> &CortexType {
        &self.return_type
    }
    pub fn num_params(&self) -> usize {
        self.params.len()
    }
    pub fn get_param(&self, index: usize) -> Option<&Parameter> {
        self.params.get(index)
    }
    pub(crate) fn signature(&self) -> FunctionSignature {
        FunctionSignature {
            return_type: self.return_type.clone(),
            params: self.params.clone(),
            type_param_names: self.type_param_names.clone(),
        }
    }
}

pub struct MemberFunctionSignature {
    pub(crate) name: OptionalIdentifier,
    pub(crate) this_arg: ThisArg,
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CortexType,
    pub(crate) type_param_names: Vec<String>,
}
impl MemberFunctionSignature {
    pub fn new(name: OptionalIdentifier, params: Vec<Parameter>, return_type: CortexType, this_arg: ThisArg, type_param_names: Vec<String>) -> Self {
        MemberFunctionSignature {
            name: name,
            params: params,
            return_type: return_type,
            this_arg: this_arg,
            type_param_names: type_param_names,
        }
    }
}
impl SimpleCodeGen for MemberFunctionSignature {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();

        s.push_str("fn ");
        s.push_str(&self.name.codegen(indent));

        if self.type_param_names.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_param_names.join(","));
            s.push_str(">");
        }

        s.push_str("(");
        
        match self.this_arg {
            ThisArg::RefThis => s.push_str("&this"),
            ThisArg::RefMutThis => s.push_str("&mut this"),
            ThisArg::DirectThis => s.push_str("this"),
        }
        if self.params.len() > 0 {
            s.push_str(", ");
        }

        for (i, param) in self.params.iter().enumerate() {
            s.push_str(&param.codegen(indent));
            if i + 1 < self.params.len() {
                s.push_str(", ");
            }
        }
        s.push_str("): ");
        s.push_str(&self.return_type.codegen(indent));
        s
    }
}

pub struct MemberFunction {
    pub(crate) signature: MemberFunctionSignature,
    pub(crate) body: Body,
}
impl SimpleCodeGen for MemberFunction {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = &"    ".repeat(indent);

        s.push_str(indent_prefix);
        s.push_str(&self.signature.codegen(indent));
        s.push_str(" {\n");
        s.push_str(&self.body.codegen(indent + 1));
        s.push_str(indent_prefix);
        s.push_str("}");
        s
    }
}
impl MemberFunction {
    pub fn new(name: OptionalIdentifier, params: Vec<Parameter>, return_type: CortexType, body: Body, this_arg: ThisArg, type_param_names: Vec<String>) -> Self {
        MemberFunction {
            signature: MemberFunctionSignature {
                name,
                this_arg,
                params,
                return_type,
                type_param_names,
            },
            body,
        }
    }

    pub fn new_with_sig(signature: MemberFunctionSignature, body: Body) -> Self {
        MemberFunction {
            signature,
            body,
        }
    }
}

#[derive(Clone)]
pub struct BasicBody {
    pub(crate) statements: Vec<PStatement>,
    pub(crate) result: Option<PExpression>,
}
impl BasicBody {
    pub fn new(statements: Vec<PStatement>, result: Option<PExpression>) -> Self {
        Self {
            statements: statements,
            result: result,
        }
    }

    pub fn has_result(&self) -> bool {
        self.result.is_some()
    }
}
impl SimpleCodeGen for BasicBody {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        for st in &self.statements {
            s.push_str(&format!("{}\n", st.codegen(indent)));
        }
        if let Some(exp) = &self.result {
            let indent_prefix = "    ".repeat(indent);
            s.push_str(&format!("{}{}\n", indent_prefix, exp.codegen(indent)));
        }
        s
    }
}

pub enum Body {
    Basic(BasicBody),
    Native(Box<dyn Fn(&Environment, &mut Heap) -> Result<CortexValue, CortexError>>),
}
impl SimpleCodeGen for Body {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Body::Basic(b) => b.codegen(indent),
            Body::Native(_) => String::from("[native code]"),
        }
    }
}
impl Body {
    pub fn empty() -> Self {
        Body::Basic(BasicBody {
            statements: Vec::new(),
            result: None,
        })
    }
}

pub struct Struct {
    pub(crate) name: OptionalIdentifier,
    pub(crate) fields: HashMap<String, CortexType>,
    pub(crate) functions: Vec<MemberFunction>,
    pub(crate) type_param_names: Vec<String>,
}
impl SimpleCodeGen for Struct {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);

        s.push_str(&indent_prefix);
        s.push_str("struct ");
        s.push_str(&self.name.codegen(indent));

        if self.type_param_names.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_param_names.join(","));
            s.push_str(">");
        }

        s.push_str(" {\n");

        for (field, typ) in &self.fields {
            s.push_str(&indent_prefix);
            s.push_str("    ");
            s.push_str(field);
            s.push_str(": ");
            s.push_str(&typ.codegen(indent));
            s.push_str(",");
            s.push_str("\n");
        }

        for func in &self.functions {
            s.push_str(func.codegen(indent + 1).as_str());
            s.push_str("\n");
        }

        s.push_str(&indent_prefix);
        s.push_str("}\n");
        s
    }
}
impl Struct {
    pub fn new(name: &str, fields: Vec<(&str, CortexType)>, type_arg_names: Vec<&str>) -> Self {
        Self::new_with_functions(name, fields, vec![], type_arg_names)
    }
    pub fn new_with_functions(name: &str, fields: Vec<(&str, CortexType)>, funcs: Vec<MemberFunction>, type_arg_names: Vec<&str>) -> Self {
        let mut map = HashMap::new();
        for f in fields {
            map.insert(String::from(f.0), f.1);
        }
        Struct {
            name: OptionalIdentifier::Ident(String::from(name)),
            fields: map,
            functions: funcs,
            type_param_names: type_arg_names.into_iter().map(|s| String::from(s)).collect(),
        }
    }
}

pub struct Bundle {
    pub(crate) name: OptionalIdentifier,
    pub(crate) fields: HashMap<String, CortexType>,
    pub(crate) functions: Vec<MemberFunction>,
    pub(crate) type_param_names: Vec<String>,
}
impl SimpleCodeGen for Bundle {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);

        s.push_str(&indent_prefix);
        s.push_str("bundle ");
        s.push_str(&self.name.codegen(indent));

        if self.type_param_names.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_param_names.join(","));
            s.push_str(">");
        }

        s.push_str(" {\n");

        for (field, typ) in &self.fields {
            s.push_str(&indent_prefix);
            s.push_str("    ");
            s.push_str(field);
            s.push_str(": ");
            s.push_str(&typ.codegen(indent));
            s.push_str(",");
            s.push_str("\n");
        }

        for func in &self.functions {
            s.push_str(func.codegen(indent + 1).as_str());
            s.push_str("\n");
        }

        s.push_str(&indent_prefix);
        s.push_str("}\n");
        s
    }
}
impl Bundle {
    pub fn new(name: &str, fields: Vec<(&str, CortexType)>, funcs: Vec<MemberFunction>, type_arg_names: Vec<&str>) -> Self {
        let mut map = HashMap::new();
        for f in fields {
            map.insert(String::from(f.0), f.1);
        }
        Bundle {
            name: OptionalIdentifier::Ident(String::from(name)),
            fields: map,
            functions: funcs,
            type_param_names: type_arg_names.into_iter().map(|s| String::from(s)).collect(),
        }
    }
}

pub struct Extension {
    pub(crate) name: PathIdent,
    pub(crate) type_param_names: Vec<String>,
    pub(crate) functions: Vec<MemberFunction>,
}
impl SimpleCodeGen for Extension {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);

        s.push_str(&indent_prefix);
        s.push_str("extend ");
        s.push_str(&self.name.codegen(indent));

        if self.type_param_names.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_param_names.join(","));
            s.push_str(">");
        }

        s.push_str(" {\n");

        for func in &self.functions {
            s.push_str(func.codegen(indent + 1).as_str());
            s.push_str("\n");
        }

        s.push_str(&indent_prefix);
        s.push_str("}\n");
        s
    }
}
