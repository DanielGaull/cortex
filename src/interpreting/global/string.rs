use std::{cell::RefCell, error::Error, rc::Rc};

use crate::{interpreting::{heap::Heap, interpreter::CortexInterpreter, module::Module, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Function}, r#type::CortexType}, codegen::r#trait::SimpleCodeGen}};

impl CortexInterpreter {
    pub(crate) fn add_string_funcs(global: &mut Module, heap: Rc<RefCell<Heap>>) -> Result<(), Box<dyn Error>> {
        let rheap = heap.clone();
        global.add_function(Function::new(
            OptionalIdentifier::Ident(String::from("toString")),
            vec![Parameter::named("item", CortexType::simple("T", false))],
            CortexType::string(false),
            Body::Native(Box::new(move |env| {
                let item = env.get_value("item")?;
                Ok(CortexValue::String(to_string(item, &rheap)))
            })),
            vec![String::from("T")],
        ))?;
        Ok(())
    }
}

fn to_string(val: CortexValue, heap: &Rc<RefCell<Heap>>) -> String {
    match val {
        CortexValue::Number(v) => v.to_string(),
        CortexValue::Boolean(v) => v.to_string(),
        CortexValue::String(s) => s,
        CortexValue::Void => String::from("void"),
        CortexValue::None => String::from("none"),
        CortexValue::Composite { struct_name, field_values, type_arg_names: _, type_args } => {
            let mut s = String::new();
            s.push_str(&struct_name.codegen(0));
            if type_args.len() > 0 {
                s.push_str("<");
                s.push_str(&type_args
                    .iter()
                    .map(|a| a.codegen(0))
                    .collect::<Vec<_>>()
                    .join(", ")
                );
                s.push_str(">");
            }
            s.push_str(" {");
            s.push_str(&field_values
                .iter()
                .map(|(f, v)| format!("{}: {}", f, to_string(v.borrow().clone(), heap)))
                .collect::<Vec<_>>()
                .join(", ")
            );
            s.push_str("}");
            s
        },
        CortexValue::Reference(addr, _, mutable) => {
            format!("&{}({})", if mutable {"mut "} else {""}, to_string(heap.borrow().get(addr).borrow().clone(), heap))
        },
        CortexValue::List(items, _) => {
            format!("[{}]", items.iter().map(|i| to_string(i.clone(), heap)).collect::<Vec<_>>().join(", "))
        },
    }
}
