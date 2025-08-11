use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::{ast::top_level::Struct, parser::CortexParser}, preprocessing::module::Module, r#type::r#type::PType};

#[test]
fn gc_test_simple() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_eq!(0, interpreter.hpsz());
    for _ in 0..100 {
        interpreter.execute_statement(CortexParser::parse_statement("simple::alloc();")?)?;
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
    interpreter.execute_statement(CortexParser::parse_statement("let time: (&mut simple::Time)? = none;")?)?;
    for _ in 0..100 {
        interpreter.execute_statement(CortexParser::parse_statement("time = simple::alloc();")?)?;
    }
    assert_eq!(100, interpreter.hpsz());
    interpreter.gc();
    assert_eq!(1, interpreter.hpsz());
    Ok(())
}

fn setup_interpreter() -> Result<CortexInterpreter, Box<dyn Error>> {
    let test_struct = Struct::new("Time", vec![
        ("m", PType::i32()),
        ("s", PType::i32()),
    ], vec![], vec![], None);
    let alloc_func = CortexParser::parse_function("fn alloc(): &mut Time { heap Time { m: 0, s: 0 } }")?;
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_function(alloc_func)?;
    module.add_struct(test_struct)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;
    Ok(interpreter)
}
