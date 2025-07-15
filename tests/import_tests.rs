use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn test_imports() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;

    let path = Path::new("./tests/res/import_tests.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.handle_program(program)?;

    assert("doBox(5)", "5", &mut interpreter)?;
    assert("hello()", "\"hello\"", &mut interpreter)?;
    assert("numWrapper(10)", "15", &mut interpreter)?;
    // assert("doPoint(2, 3)", "(2, 3)", &mut interpreter)?;

    Ok(())
}

fn load_lib(interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let path = Path::new("./tests/res/testlib.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.handle_program(program)?;
    Ok(())
}

fn assert(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
