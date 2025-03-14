use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn test_identity() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let path = Path::new("./tests/res/generics_file.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }
    
    assert_exp("identity(5)", "5", &mut interpreter)?;
    assert_exp("identity(null)", "null", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_box() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let path = Path::new("./tests/res/generics_file.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    run("let box = Box<number>{item: 5};", &mut interpreter)?;
    assert_exp("box.get()", "5", &mut interpreter)?;
    run("box.set(100);", &mut interpreter)?;
    assert_exp("box.get()", "100", &mut interpreter)?;
    run("let val: number = readBox(box);", &mut interpreter)?;
    assert_exp("val", "100", &mut interpreter)?;

    Ok(())
}

fn assert_exp(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.evaluate_expression(&ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
fn run(input: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.run_statement(&CortexParser::parse_statement(input)?)?;
    Ok(())
}
