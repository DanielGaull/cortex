use std::error::Error;

use cortex_lang::{interpreting::{interpreter::CortexInterpreter, global::list::ListError}, parsing::parser::CortexParser};

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
    assert("myList.find(4)", "none", &mut interpreter)?;

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

    run("myList = [1, 2, 3];", &mut interpreter)?;
    run("myList.insert(3, 10);", &mut interpreter)?;
    assert("myList[0]", "1", &mut interpreter)?;
    assert("myList[3]", "10", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_index_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList: &list<number> = [1, 2, 3];", &mut interpreter)?;
    assert_err("myList[-2];", ListError::InvalidIndex(-2f64, 3usize), &mut interpreter)?;
    assert_err("myList[4];", ListError::InvalidIndex(4f64, 3usize), &mut interpreter)?;
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

fn assert_err<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_statement(statement)?;
    let evaled = interpreter.run_statement(&parsed);
    if let Err(e) = evaled {
        if !e.is::<T>() {
            panic!("Value e {:?} is not T {:?}", e, std::any::type_name::<T>());
        }
        let error = (&*e).downcast_ref::<T>().expect("Error downcast failed");
        assert_eq!(flavor, *error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
}
