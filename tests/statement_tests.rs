use cortex::{interpreting::{env::Environment, interpreter::CortexInterpreter, module::Module, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Function}, r#type::CortexType}, parser::CortexParser}};
use thiserror::Error;
use std::error::Error;

#[derive(Error, Debug)]
enum TestError {
    #[error("Test error: {0}")]
    Err(&'static str),
}

fn run_statement(input: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_statement(input)?;
    interpreter.run_statement(&ast)?;
    Ok(())
}
fn assert_expression(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.evaluate_expression(&ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}

fn setup_interpreter() -> Result<CortexInterpreter, Box<dyn Error>> {
    let add_body = Body::Native(Box::new(|env: &Environment| -> Result<CortexValue, Box<dyn Error>> {
        // The two arguments are "a" and "b"
        let a = env.get_value("a")?;
        let b = env.get_value("b")?;
        if let CortexValue::Number(a_val) = a {
            if let CortexValue::Number(b_val) = b {
                Ok(CortexValue::Number(a_val + b_val))
            } else {
                Err(Box::new(TestError::Err("b is not a number")))
            }
        } else {
            Err(Box::new(TestError::Err("a is not a number")))
        }
    }));
    let add_func = Function::new(
        OptionalIdentifier::Ident(String::from("add")),
        vec![
            Parameter::named("a", CortexType::number(false)),
            Parameter::named("b", CortexType::number(false))
        ],
        CortexType::number(false),
        add_body
    );
    let mut interpreter = CortexInterpreter::new();
    let mut mod_env = Environment::base();
    mod_env.add_function(add_func)?;
    let path = CortexParser::parse_path("simple")?;
    let module = Module::new(mod_env);
    interpreter.register_module(&path, module)?;
    Ok(interpreter)
}

#[test]
fn statement_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    run_statement("let x = 5;", &mut interpreter)?;
    run_statement("let y: number = 7;", &mut interpreter)?;
    run_statement("let z = simple::add(x,y);", &mut interpreter)?;
    assert_expression("x", "5", &mut interpreter)?;
    assert_expression("y", "7", &mut interpreter)?;
    assert_expression("z", "12", &mut interpreter)?;

    run_statement("z = 10;", &mut interpreter)?;
    assert_expression("z", "10", &mut interpreter)?;

    Ok(())
}
