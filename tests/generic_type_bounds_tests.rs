use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser, preprocessing::error::PreprocessingError};

#[test]
fn run_generic_type_bounds_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let path = Path::new("./tests/res/generic_type_bounds.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.run_program(program)?;

    run("let action = heap PassthroughAction { item: 5 };", &mut interpreter)?;
    assert_exp("dispatch(action)", "5", &mut interpreter)?;
    run("let nonAction = heap NotPassthroughAction { item: 5 };", &mut interpreter)?;
    assert_err("dispatch(nonAction);", PreprocessingError::TypeBoundNotSatisfied(String::from("NotPassthroughAction"), String::from("follows Action")), &mut interpreter)?;

    Ok(())
}

#[test]
fn run_member_generic_type_bounds_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let path = Path::new("./tests/res/generic_type_bounds.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.run_program(program)?;

    run("let dispatcher = heap Dispatcher { };", &mut interpreter)?;
    run("let action = heap PassthroughAction { item: 5 };", &mut interpreter)?;
    assert_exp("dispatcher.dispatch(action)", "5", &mut interpreter)?;
    run("let nonAction = heap NotPassthroughAction { item: 5 };", &mut interpreter)?;
    assert_err("dispatcher.dispatch(nonAction);", PreprocessingError::TypeBoundNotSatisfied(String::from("NotPassthroughAction"), String::from("follows Action")), &mut interpreter)?;

    Ok(())
}

fn assert_exp(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
#[allow(dead_code)]
fn assert_exp_or(input: &str, expected1: &str, expected2: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert!(expected1 == value_string || expected2 == value_string);
    Ok(())
}
fn run(input: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.execute_statement(CortexParser::parse_statement(input)?)?;
    Ok(())
}
fn assert_err<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_statement(statement)?;
    let evaled = interpreter.execute_statement(parsed);
    if let Err(e) = evaled {
        let msg = format!("Expected {:?}, but found {:?}", flavor, e);
        let error = *e.downcast::<T>().expect(&msg);
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
}
