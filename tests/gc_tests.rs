use std::error::Error;

use cortex_lang::{interpreting::{interpreter::CortexInterpreter, module::Module}, parsing::{ast::{top_level::Bundle, r#type::CortexType}, parser::CortexParser}};

#[test]
fn gc_test_simple() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_eq!(0, interpreter.hpsz());
    for _ in 0..100 {
        interpreter.run_statement(&CortexParser::parse_statement("simple::alloc();")?)?;
    }
    assert_eq!(100, interpreter.hpsz());
    interpreter.gc();
    assert_eq!(0, interpreter.hpsz());
    Ok(())
}

#[test]
fn gc_test_ref() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_eq!(0, interpreter.hpsz());
    interpreter.run_statement(&CortexParser::parse_statement("let time: simple::Time? = null;")?)?;
    for _ in 0..100 {
        interpreter.run_statement(&CortexParser::parse_statement("time = simple::alloc();")?)?;
    }
    assert_eq!(100, interpreter.hpsz());
    interpreter.gc();
    assert_eq!(1, interpreter.hpsz());
    Ok(())
}

fn setup_interpreter() -> Result<CortexInterpreter, Box<dyn Error>> {
    let test_bundle = Bundle::new("Time", vec![
        ("m", CortexType::number(false)),
        ("s", CortexType::number(false)),
    ], vec![]);
    let alloc_func = CortexParser::parse_function("fn alloc(): Time { Time { m: 0, s: 0 } }")?;
    let mut interpreter = CortexInterpreter::new();
    let mut module = Module::new();
    module.add_function(alloc_func)?;
    module.add_bundle(test_bundle)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;
    Ok(interpreter)
}
