use std::error::Error;

use cortex_lang::{interpreting::{interpreter::CortexInterpreter, module::Module}, parsing::{ast::{expression::{OptionalIdentifier, PathIdent}, top_level::{BasicBody, Body, Bundle, Function}, r#type::CortexType}, parser::CortexParser}};

fn run_test(input: &str, type_str: &str, interpreter: &CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let eval_typ = interpreter.determine_type(&ast)?;
    let typ = CortexParser::parse_type(type_str)?;
    assert_eq!(typ, eval_typ);
    Ok(())
}

#[test]
fn run_simple_type_tests() -> Result<(), Box<dyn Error>> {
    let interpreter = CortexInterpreter::new();
    run_test("5", "number", &interpreter)?;
    run_test("5.3", "number", &interpreter)?;
    run_test("\"hello\"", "string", &interpreter)?;
    run_test("true", "bool", &interpreter)?;
    run_test("false", "bool", &interpreter)?;
    run_test("void", "void", &interpreter)?;
    run_test("(((void)))", "void", &interpreter)?;
    run_test("null", "null?", &interpreter)?;
    Ok(())
}

#[test]
fn run_reference_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let mut module = Module::new();
    module.add_bundle(Bundle::new(
        "Time", 
        vec![
            ("m", CortexType::number(false)),
            ("s", CortexType::number(false)),
        ],
        vec![],
        vec![],
    ))?;
    module.add_bundle(Bundle::new(
        "Box",
        vec![
            ("time", CortexType::reference(CortexType::new(PathIdent::simple(String::from("Time")), false), true))
        ],
        vec![
            Function::member_func(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::reference(CortexType::simple("Time", false), true),
                Body::Basic(BasicBody::new(vec![], Some(CortexParser::parse_expression("this.time")?))),
                cortex_lang::parsing::ast::top_level::ThisArg::MutThis
            )
        ],
        vec![],
    ))?;
    interpreter.register_module(&PathIdent::simple(String::from("Time")), module)?;
    run_test("Time::Time{m:5,s:5}", "&mut Time::Time", &interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("let box = Time::Box{ time: Time::Time{m:4,s:5} };")?)?;
    run_test("box", "&mut Time::Box", &mut interpreter)?;
    run_test("box.get()", "&mut Time::Time", &mut interpreter)?;
    Ok(())
}

// #[test]
// fn run_var_type_tests() -> Result<(), Box<dyn Error>> {
//     let mut interpreter = CortexInterpreter::new();
//     let mut module = Environment::base();
//     mod_env.add_const(String::from("myBoolean"), CortexType::boolean(false), CortexValue::Boolean(true))?;
//     mod_env.add_const(String::from("nullableBoolean"), CortexType::boolean(true), CortexValue::Boolean(true))?;
//     let path = CortexParser::parse_path("simple")?;
//     interpreter.register_module(&path, module)?;

//     run_test("simple::myBoolean", "bool", &interpreter)?;
//     run_test("simple::nullableBoolean", "bool?", &interpreter)?;

//     Ok(())
// }

#[test]
fn subtype_tests() -> Result<(), Box<dyn Error>> {
    assert!(CortexType::null().is_subtype_of(&CortexType::number(true)));
    assert!(CortexType::reference(CortexType::number(false), true).is_subtype_of(&CortexType::reference(CortexType::number(false), false)));
    assert!(!CortexType::reference(CortexType::number(false), false).is_subtype_of(&CortexType::reference(CortexType::number(false), true)));
    Ok(())
}
