use std::error::Error;

use cortex::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

fn run_test(input: &str, type_str: &str, interpreter: &CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(&String::from(input))?;
    let eval_typ = interpreter.determine_type(&ast)?;
    let typ = interpreter.evaluate_type(&CortexParser::parse_type(&String::from(type_str))?)?;
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
