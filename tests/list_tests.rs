use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn test_list_get_set() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList[0]", "1", &mut interpreter)?;
    assert("myList[1]", "2", &mut interpreter)?;
    assert("myList[2]", "3", &mut interpreter)?;
    
    run("myList[1] = 5;", &mut interpreter)?;
    assert("myList[1]", "5", &mut interpreter)?;

    Ok(())
}

fn assert(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.evaluate_expression(&ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
fn run(st: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.run_statement(&CortexParser::parse_statement(st)?)?;
    Ok(())
}
