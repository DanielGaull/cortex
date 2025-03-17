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

#[test]
fn test_list_len() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList.len()", "3", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_find() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList.find(1)", "0", &mut interpreter)?;
    assert("myList.find(2)", "1", &mut interpreter)?;
    assert("myList.find(4)", "null", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_contains() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList.contains(1)", "true", &mut interpreter)?;
    assert("myList.contains(2)", "true", &mut interpreter)?;
    assert("myList.contains(4)", "false", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_add_insert_remove() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;

    run("myList.add(4);", &mut interpreter)?;
    assert("myList[3]", "4", &mut interpreter)?;

    run("myList.insert(1, 1.5);", &mut interpreter)?;
    assert("myList[0]", "1", &mut interpreter)?;
    assert("myList[1]", "1.5", &mut interpreter)?;
    assert("myList[2]", "2", &mut interpreter)?;

    run("myList.remove(0);", &mut interpreter)?;
    assert("myList[0]", "1.5", &mut interpreter)?;

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
