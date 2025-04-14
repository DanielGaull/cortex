use std::error::Error;

use crate::{interpreting::{heap::Heap, value::CortexValue}, parsing::ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, PFunction}, r#type::CortexType}, preprocessing::{module::Module, preprocessor::CortexPreprocessor}};

impl CortexPreprocessor {
    pub(crate) fn add_string_funcs(global: &mut Module) -> Result<(), Box<dyn Error>> {
        global.add_function(PFunction::new(
            OptionalIdentifier::Ident(String::from("toString")),
            vec![Parameter::named("item", CortexType::simple("T", false))],
            CortexType::string(false),
            Body::Native(Box::new(move |env, heap| {
                let item = env.get_value("item")?;
                Ok(CortexValue::String(to_string(item, heap)))
            })),
            vec![String::from("T")],
        ))?;
        Ok(())
    }
}

fn to_string(val: CortexValue, heap: &Heap) -> String {
    match val {
        CortexValue::Number(v) => v.to_string(),
        CortexValue::Boolean(v) => v.to_string(),
        CortexValue::String(s) => s,
        CortexValue::Void => String::from("void"),
        CortexValue::None => String::from("none"),
        CortexValue::Composite { field_values } => {
            let mut s = String::new();
            s.push_str("{");
            s.push_str(&field_values
                .iter()
                .map(|(f, v)| format!("{}: {}", f, to_string(v.borrow().clone(), heap)))
                .collect::<Vec<_>>()
                .join(", ")
            );
            s.push_str("}");
            s
        },
        CortexValue::Reference(addr) => {
            format!("&({})", to_string(heap.get(addr).borrow().clone(), heap))
        },
        CortexValue::List(items) => {
            format!("[{}]", items.iter().map(|i| to_string(i.clone(), heap)).collect::<Vec<_>>().join(", "))
        },
        CortexValue::Char(c) => (c as char).to_string(),
    }
}
