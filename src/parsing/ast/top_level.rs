use std::collections::HashMap;

use crate::{interpreting::{env::Environment, error::CortexError, heap::Heap, value::CortexValue}, parsing::codegen::r#trait::SimpleCodeGen, r#type::r#type::{CortexType, FollowsClause, TypeArg, TypeParam, TypeParamType}};

use super::{expression::{OptionalIdentifier, PExpression, Parameter, PathIdent}, program::ModuleContent, statement::PStatement};

pub struct ImportEntry {
    pub(crate) path: PathIdent,
    pub(crate) alias: Option<String>,
}
impl SimpleCodeGen for ImportEntry {
    fn codegen(&self, indent: usize) -> String {
        if let Some(alias) = &self.alias {
            format!("{} as {}", self.path.codegen(indent), alias)
        } else {
            format!("{}", self.path.codegen(indent))
        }
    }
}

pub struct Import {
    pub(crate) entries: Vec<ImportEntry>,
}
impl SimpleCodeGen for Import {
    fn codegen(&self, indent: usize) -> String {
        format!(
            "{}import {};", 
            "    ".repeat(indent),
            self.entries.iter()
                .map(|e| e.codegen(0))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

pub enum TopLevel {
    Module {
        name: String,
        contents: ModuleContent,
    },
    Function(PFunction),
    Struct(Struct),
    Extension(Extension),
    Contract(Contract),
}
impl SimpleCodeGen for TopLevel {
    fn codegen(&self, indent: usize) -> String {
        match self {
            Self::Module { name, contents } => {
                let mut s = String::new();
                let indent_prefix = &"    ".repeat(indent);
                s.push_str(indent_prefix);
                s.push_str("module ");
                s.push_str(name);
                s.push_str(" {\n");

                s.push_str(&contents.codegen(indent + 1));
                
                s.push_str(indent_prefix);
                s.push_str("}");
                s
            },
            Self::Function(func) => func.codegen(indent),
            Self::Struct(struc) => struc.codegen(indent),
            Self::Extension(extension) => extension.codegen(indent),
            Self::Contract(contract) => contract.codegen(indent),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum ThisArg {
    RefThis,
    RefMutThis,
    DirectThis,
}

#[derive(Clone)]
pub(crate) struct FunctionSignature {
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CortexType,
    pub(crate) type_params: Vec<TypeParam>,
}

pub struct PFunction {
    pub(crate) name: OptionalIdentifier,
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CortexType,
    pub(crate) body: Body,
    pub(crate) type_params: Vec<TypeParam>,
}
impl SimpleCodeGen for PFunction {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = &"    ".repeat(indent);

        s.push_str(indent_prefix);
        s.push_str("fn ");
        s.push_str(&self.name.codegen(indent));

        if self.type_params.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_params
                .iter()
                .map(|t| t.codegen(0))
                .collect::<Vec<_>>()
                .join(","));
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
    pub fn new(name: OptionalIdentifier, params: Vec<Parameter>, return_type: CortexType, body: Body, type_params: Vec<TypeParam>) -> Self {
        PFunction {
            name: name,
            params: params,
            return_type: return_type,
            body: body,
            type_params,
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
            type_params: self.type_params.clone(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct MemberFunctionSignature {
    pub(crate) name: OptionalIdentifier,
    pub(crate) this_arg: ThisArg,
    pub(crate) params: Vec<Parameter>,
    pub(crate) return_type: CortexType,
    pub(crate) type_params: Vec<TypeParam>,
}
impl MemberFunctionSignature {
    pub fn new(name: OptionalIdentifier, params: Vec<Parameter>, return_type: CortexType, this_arg: ThisArg, type_params: Vec<TypeParam>) -> Self {
        MemberFunctionSignature {
            name: name,
            params: params,
            return_type: return_type,
            this_arg: this_arg,
            type_params,
        }
    }
}
impl SimpleCodeGen for MemberFunctionSignature {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();

        s.push_str("fn ");
        s.push_str(&self.name.codegen(indent));

        if self.type_params.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_params
                .iter()
                .map(|t| t.codegen(0))
                .collect::<Vec<_>>()
                .join(","));
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
    pub fn new(name: OptionalIdentifier, params: Vec<Parameter>, return_type: CortexType, body: Body, this_arg: ThisArg, type_params: Vec<TypeParam>) -> Self {
        MemberFunction {
            signature: MemberFunctionSignature {
                name,
                this_arg,
                params,
                return_type,
                type_params,
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
    pub(crate) name: String,
    pub(crate) fields: HashMap<String, CortexType>,
    pub(crate) functions: Vec<MemberFunction>,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) follows_clause: Option<FollowsClause>,
}
impl SimpleCodeGen for Struct {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);

        s.push_str(&indent_prefix);
        s.push_str("struct ");
        s.push_str(&self.name);

        if self.type_params.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_params
                .iter()
                .map(|t| t.codegen(0))
                .collect::<Vec<_>>()
                .join(","));
            s.push_str(">");
        }

        if let Some(clause) = &self.follows_clause {
            s.push_str(" ");
            s.push_str(&clause.codegen(indent));
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
    pub fn new(name: &str, fields: Vec<(&str, CortexType)>, funcs: Vec<MemberFunction>, type_arg_names: Vec<&str>, follows_clause: Option<FollowsClause>) -> Self {
        let mut map = HashMap::new();
        for f in fields {
            map.insert(String::from(f.0), f.1);
        }
        Struct {
            name: String::from(name),
            fields: map,
            functions: funcs,
            type_params: type_arg_names.into_iter().map(|s| TypeParam::new(s, TypeParamType::Ty)).collect(),
            follows_clause,
        }
    }
}

pub struct Extension {
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) name: PathIdent,
    pub(crate) type_args: Vec<TypeArg>,
    pub(crate) functions: Vec<MemberFunction>,
    pub(crate) follows_clause: Option<FollowsClause>,
}
impl SimpleCodeGen for Extension {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);

        s.push_str(&indent_prefix);
        s.push_str("extend");

        if self.type_args.len() > 0 {
            s.push_str(&format!("<{}> ", self.type_args.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", ")));
        } else {
            s.push_str(" ");
        }

        s.push_str(&self.name.codegen(indent));

        if self.type_params.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_params
                .iter()
                .map(|t| t.codegen(0))
                .collect::<Vec<_>>()
                .join(","));
            s.push_str(">");
        }

        if let Some(clause) = &self.follows_clause {
            s.push_str(" ");
            s.push_str(&clause.codegen(indent));
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

pub struct Contract {
    pub(crate) name: String,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) function_sigs: Vec<MemberFunctionSignature>,
}
impl SimpleCodeGen for Contract {
    fn codegen(&self, indent: usize) -> String {
        let mut s = String::new();
        let indent_prefix = "    ".repeat(indent);

        s.push_str(&indent_prefix);
        s.push_str("contract ");
        s.push_str(&self.name);

        if self.type_params.len() > 0 {
            s.push_str("<");
            s.push_str(&self.type_params
                .iter()
                .map(|t| t.codegen(0))
                .collect::<Vec<_>>()
                .join(","));
            s.push_str(">");
        }

        s.push_str(" {\n");

        for sig in &self.function_sigs {
            s.push_str(&indent_prefix);
            s.push_str("    ");
            s.push_str(sig.codegen(indent + 1).as_str());
            s.push_str(";\n");
        }

        s.push_str(&indent_prefix);
        s.push_str("}\n");
        s
    }
}
impl Contract {
    pub fn new(name: &str, type_params: Vec<TypeParam>, function_sigs: Vec<MemberFunctionSignature>) -> Self {
        Self {
            name: String::from(name),
            type_params,
            function_sigs
        }
    }
}
