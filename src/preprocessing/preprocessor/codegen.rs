use crate::{parsing::{ast::expression::PathIdent, codegen::r#trait::SimpleCodeGen}, preprocessing::{ast::{expression::RExpression, function::RDefinedBody, statement::RStatement, top_level::{RContract, RMemberFunctionSignature}, r#type::RType}, module::TypeDefinition}};

use super::preprocessor::CortexPreprocessor;

impl CortexPreprocessor {
    pub fn codegen(&self) -> String {
        let mut s = String::new();
        for (path, contract) in &self.contract_map {
            s.push_str(&self.codegen_contract(path, contract, 0));
        }

        for (path, typedef) in &self.type_map {
            s.push_str(&self.codegen_typedef(path, typedef, 0));
        }

        s
    }

    fn codegen_exp(&self, exp: &RExpression, indent: usize) -> String {
        match exp {
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
                let fn_name = self.function_dict.get_name(*addr).unwrap().codegen(indent);
                format!("{}({})", fn_name, args.iter().map(|a| self.codegen_exp(a, indent)).collect::<Vec<_>>().join(", "))
            },
            RExpression::Construction { assignments } => {
                let mut s = String::from("<anonymous> {\n");
                for (n, v) in assignments {
                    s.push_str(&format!("    {}: {},\n", n, self.codegen_exp(v, indent)));
                }
                s.push_str("}");
                s
            },
            RExpression::IfStatement { first, conds, last } => {
                let mut s = String::new();
                s.push_str("if ");
                s.push_str(&self.codegen_exp(&first.condition, indent));
                s.push_str(" { \n");
                s.push_str(&self.codegen_defined_body(&first.body, indent + 1));
                s.push_str("}");
                for cond in conds {
                    s.push_str(" elif ");
                    s.push_str(&self.codegen_exp(&cond.condition, indent));
                    s.push_str(" { \n");
                    s.push_str(&self.codegen_defined_body(&cond.body, indent + 1));
                    s.push_str("}");
                }
                if let Some(last) = last {
                    s.push_str(" else {\n");
                    s.push_str(&self.codegen_defined_body(&last, indent + 1));
                    s.push_str("}");
                }
                s
            },
            RExpression::UnaryOperation { op, exp } => format!("{}{}", op.codegen(indent), self.codegen_exp(exp, indent)),
            RExpression::CollectionLiteral(exps) => format!("[{}]", exps.iter().map(|e| self.codegen_exp(e, indent)).collect::<Vec<_>>().join(", ")),
            RExpression::Bang(inner) => format!("{}!", self.codegen_exp(inner, indent)),
            RExpression::MemberAccess(base, member) => format!("{}.{}", self.codegen_exp(base, indent), member),
            RExpression::BinaryOperation { left, op, right } =>
                format!("{} {} {}", self.codegen_exp(left, indent), op.codegen(indent), self.codegen_exp(right, indent)),
            RExpression::Tuple(exps) => {
                if exps.len() == 1 {
                    format!("({},)", exps.iter().map(|e| self.codegen_exp(e, indent)).collect::<Vec<_>>().join(", "))
                } else {
                    format!("({})", exps.iter().map(|e| self.codegen_exp(e, indent)).collect::<Vec<_>>().join(", "))
                }
            },
            RExpression::MakeFat(exp, _vtable) => self.codegen_exp(exp, indent),
            RExpression::FatCall { callee, index_in_vtable, args } => {
                format!("{}.<fat{}>({})", self.codegen_exp(callee, indent), index_in_vtable, args.iter().map(|a| self.codegen_exp(a, indent)).collect::<Vec<_>>().join(", "))
            },
            RExpression::HeapAlloc(exp) => format!("heap {}", self.codegen_exp(exp, indent)),
            RExpression::DerefFat(exp) => self.codegen_exp(exp, indent),
            RExpression::MakeAnon(exp) => format!("anon {}", self.codegen_exp(exp, indent)),
            RExpression::DeAnon(exp) => format!("deanon<?> {}", self.codegen_exp(exp, indent)),
            RExpression::FunctionPointerCall { ident, args } => 
                format!("{}({})", ident, args.iter().map(|a| self.codegen_exp(a, indent)).collect::<Vec<_>>().join(", ")),
            RExpression::MakeFunctionPointer(addr) => format!("func@0x{:x}", addr),
        }
    }

    fn codegen_statement(&self, statement: &RStatement, indent: usize) -> String {
        match statement {
            RStatement::Expression(exp) => format!("{};", self.codegen_exp(exp, indent)),
            RStatement::Throw(exp) => {
                if let Some(exp) = exp {
                    format!("throw {};", self.codegen_exp(exp, indent))
                } else {
                    String::from("throw;")
                }
            },
            RStatement::VariableDeclaration { name, is_const, initial_value } => 
                format!("{} {} = {};", if *is_const { "const" } else { "let" }, name, self.codegen_exp(initial_value, indent)),
            RStatement::Assignment { name, value } =>
                format!("{} = {};", name.codegen(indent), self.codegen_exp(value, indent)),
            RStatement::WhileLoop(body) => {
                let mut s = String::new();
                s.push_str("while ");
                s.push_str(&self.codegen_exp(&body.condition, indent));
                s.push_str(" { \n");
                s.push_str(&self.codegen_defined_body(&body.body, indent + 1));
                s.push_str("}");
                s
            },
            RStatement::Break => String::from("break;"),
            RStatement::Continue => String::from("continue;"),
        }
    }

    fn codegen_defined_body(&self, body: &RDefinedBody, indent: usize) -> String {
        let prefix = "    ".repeat(indent);
        let mut s = String::new();
        for line in &body.statements {
            s.push_str(&prefix);
            s.push_str(&self.codegen_statement(line, indent));
        }
        if let Some(last) = &body.result {
            s.push_str(&prefix);
            s.push_str(&self.codegen_exp(last, indent));
        }
        s
    }

    fn codegen_contract(&self, name: &PathIdent, contract: &RContract, indent: usize) -> String {
        let mut s = String::new();
        let prefix = "    ".repeat(indent);
        s.push_str(&prefix);
        s.push_str("contract ");

        s.push_str(&name.codegen(0));
        s.push_str("<");
        s.push_str(&contract.type_params.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", "));
        s.push_str("> {\n");

        for m in &contract.function_sigs {
            s.push_str(&prefix);
            s.push_str("    ");
            s.push_str(&self.codegen_member_fn_sig(m));
            s.push_str(";\n");
        }

        s.push_str(&prefix);
        s.push_str("}\n");
        s
    }

    fn codegen_member_fn_sig(&self, sig: &RMemberFunctionSignature) -> String {
        let mut s = String::new();
        s.push_str("fn ");
        s.push_str(&sig.name);
        s.push_str(&format!("<{}>", sig.type_params.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", ")));
        s.push_str("(");
        s.push_str(&sig.params.iter().map(|p| format!("{}: {}", p.name, self.codegen_type(&p.typ))).collect::<Vec<_>>().join(", "));
        s.push_str(")");
        s
    }

    fn codegen_type(&self, ty: &RType) -> String {
        ty.codegen(0)
    }

    fn codegen_typedef(&self, path: &PathIdent, typedef: &TypeDefinition, indent: usize) -> String {
        let mut s = String::new();
        let prefix = "    ".repeat(indent);

        s.push_str(&prefix);
        s.push_str("struct ");
        s.push_str(&path.codegen(0));
        s.push_str(&format!("<{}>", typedef.type_params.iter().map(|t| t.codegen(0)).collect::<Vec<_>>().join(", ")));
        if typedef.followed_contracts.len() > 0 {
            s.push_str(&format!(" follows {}", typedef.followed_contracts.iter().map(|c| c.codegen(0)).collect::<Vec<_>>().join(" + ")));
        }
        s.push_str(" {\n");

        for f in &typedef.fields {
            s.push_str(&prefix);
            s.push_str(&format!("{}: {},\n", f.0, self.codegen_type(f.1)));
        }

        s.push_str(&prefix);
        s.push_str("}\n");

        s
    }
}
