use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser, preprocessing::error::PreprocessingError};

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
    interpreter.run_program(program)?;

    assert("doBox(5)", "5", &mut interpreter)?;
    assert("doBox2(5)", "5", &mut interpreter)?;
    assert("numWrapper(10)", "15", &mut interpreter)?;
    assert("hello()", "\"hello\"", &mut interpreter)?;

    run("let point = doPoint(2, 3);", &mut interpreter)?;
    assert("point.t0", "2", &mut interpreter)?;
    assert("point.t1", "3", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_imports2() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;

    let path = Path::new("./tests/res/import_tests2.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.run_program(program)?;

    assert("multiplyStringLengths(\"foo\", 3)", "9", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_imports3() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;

    let path = Path::new("./tests/res/import_tests2.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.run_program(program)?;
    interpreter.finish_running_program();

    import("import mylib;", &mut interpreter)?;
    import("import libBoxUtils;", &mut interpreter)?;
    run("let box = heap LibBox<i32>{item: 5};", &mut interpreter)?;
    assert("box.duplicate().get()", "5", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_import_errors1() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;

    import("import mylib::NumWrapper as NumWrapper;", &mut interpreter)?;
    assert_err_import("import mylib::AFunc as NumWrapper;", PreprocessingError::DuplicateSymbolImport(String::from("NumWrapper")), &mut interpreter)?;

    Ok(())
}

#[test]
fn test_import_errors2() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;

    top_level("struct NumWrapper { value: i32 }", &mut interpreter)?;
    assert_err_import("import mylib::AFunc as NumWrapper;", PreprocessingError::DuplicateSymbolImport(String::from("NumWrapper")), &mut interpreter)?;

    Ok(())
}

#[test]
fn test_import_errors3() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;
    top_level("struct NumWrapper { }", &mut interpreter)?;
    assert_err_import("import mylib;", PreprocessingError::DuplicateSymbolImport(String::from("NumWrapper")), &mut interpreter)?;

    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;
    top_level("contract NumWrapper { }", &mut interpreter)?;
    assert_err_import("import mylib;", PreprocessingError::DuplicateSymbolImport(String::from("NumWrapper")), &mut interpreter)?;

    let mut interpreter = CortexInterpreter::new()?;
    load_lib(&mut interpreter)?;
    top_level("fn NumWrapper() { }", &mut interpreter)?;
    assert_err_import("import mylib;", PreprocessingError::DuplicateSymbolImport(String::from("NumWrapper")), &mut interpreter)?;

    Ok(())
}

fn load_lib(interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let path = Path::new("./tests/res/testlib.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.run_program(program)?;
    interpreter.finish_running_program();
    Ok(())
}

fn assert(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
fn run(st: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.execute_statement(CortexParser::parse_statement(st)?)?;
    Ok(())
}
fn import(st: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.handle_import(CortexParser::parse_import(st)?)?;
    Ok(())
}
fn top_level(st: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.run_top_level(CortexParser::parse_top_level(st)?)?;
    Ok(())
}

fn assert_err_import<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_import(statement)?;
    let evaled = interpreter.handle_import(parsed);
    if let Err(e) = evaled {
        let error = *e.downcast::<T>().expect("Expected provided error type");
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
}
