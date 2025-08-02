use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn run_test() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    let path = Path::new("./tests/res/linked_list_file.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    interpreter.run_program(program)?;

    interpreter.execute_statement(CortexParser::parse_statement("testAll();")?)?;

    Ok(())
}

fn setup_interpreter() -> Result<CortexInterpreter, Box<dyn Error>> {
    let interpreter = CortexInterpreter::new()?;
    Ok(interpreter)
}
