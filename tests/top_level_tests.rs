use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};
use std::{error::Error, fs::File, io::Read, path::Path};

fn assert_expression(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.evaluate_expression(&ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
fn assert_expression_or(input: &str, expected1: &str, expected2: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.evaluate_expression(&ast)?;
    let value_string = format!("{}", value);
    assert!(expected1 == value_string || expected2 == value_string);
    Ok(())
}

#[test]
fn test_top_level() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let path = Path::new("./tests/res/full_file.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    assert_expression("main::main(1, 2)", "3", &mut interpreter)?;
    assert_expression_or("getPoint(3, 5)", "Point(x:3;y:5;)", "Point(y:5;x:3;)", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_module_pathing() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let path = Path::new("./tests/res/module_pathing.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    assert_expression("testFn(0, 0, 10, 10)", "20", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_bundle() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let path = Path::new("./tests/res/bundle_file.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    assert_expression("runTest(5)", "15", &mut interpreter)?;
    Ok(())
}
