use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn test_extensions() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let path = Path::new("./tests/res/extensions_file.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    assert_expression_or("toString(main())", "\"&({x: 4, y: 4})\"", "\"&({y: 4, x: 4})\"", &mut interpreter)?;

    Ok(())
}

#[allow(dead_code)]
fn assert_expression(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
fn assert_expression_or(input: &str, expected1: &str, expected2: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert!(expected1 == value_string || expected2 == value_string);
    Ok(())
}
