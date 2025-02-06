use std::error::Error;

use cortex::{interpreting::{env::Environment, interpreter::CortexInterpreter, module::Module, r#type::CortexType, value::CortexValue}, parsing::parser::CortexParser};

fn run_test(input: &str, type_str: &str, interpreter: &CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let eval_typ = interpreter.determine_type(&ast)?;
    let typ = interpreter.evaluate_type(&CortexParser::parse_type(type_str)?)?;
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
    run_test("null", "any?", &interpreter)?;
    Ok(())
}

#[test]
fn run_var_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let mut mod_env = Environment::base();
    mod_env.add_const(String::from("myBoolean"), CortexType::boolean(false), CortexValue::Boolean(true))?;
    mod_env.add_const(String::from("nullableBoolean"), CortexType::boolean(true), CortexValue::Boolean(true))?;
    let path = CortexParser::parse_path("simple")?;
    let module = Module::new(mod_env);
    interpreter.register_module(&path, module)?;

    run_test("simple::myBoolean", "bool", &interpreter)?;
    run_test("simple::nullableBoolean", "bool?", &interpreter)?;

    Ok(())
}
