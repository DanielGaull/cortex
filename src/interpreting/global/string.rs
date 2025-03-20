use std::{cell::RefCell, error::Error, rc::Rc};

use crate::{interpreting::{heap::Heap, interpreter::CortexInterpreter, module::Module, value::CortexValue}, parsing::ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Function}, r#type::CortexType}};

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
        CortexValue::Composite { field_values } => {
            let mut s = String::new();
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
        CortexValue::Reference(addr) => {
            format!("&({})", to_string(heap.borrow().get(addr).borrow().clone(), heap))
        },
        CortexValue::List(items) => {
            format!("[{}]", items.iter().map(|i| to_string(i.clone(), heap)).collect::<Vec<_>>().join(", "))
        },
    }
}
