use std::error::Error;

use cortex::{interpreting::{env::Environment, interpreter::CortexInterpreter, module::Module, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Function}, r#type::CortexType}, parser::CortexParser}};
use thiserror::Error;

#[derive(Error, Debug)]
enum TestError {
    #[error("Test error: {0}")]
    Err(&'static str),
}

fn run_test(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.evaluate_expression(&ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}

#[test]
fn simple_eval_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    run_test("5", "5", &mut interpreter)?;
    run_test("5.3", "5.3", &mut interpreter)?;
    run_test("true", "true", &mut interpreter)?;
    run_test("false", "false", &mut interpreter)?;
    run_test("\"hello\"", "\"hello\"", &mut interpreter)?;
    run_test("null", "null", &mut interpreter)?;
    run_test("void", "void", &mut interpreter)?;
    Ok(())
}

#[test]
fn mod_var_eval_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new();
    let mut mod_env = Environment::base();
    mod_env.add_const(String::from("myBoolean"), CortexType::boolean(false), CortexValue::Boolean(true))?;
    mod_env.add_const(String::from("nullableBoolean"), CortexType::boolean(true), CortexValue::Null)?;
    let path = CortexParser::parse_path("simple")?;
    let module = Module::new(mod_env);
    interpreter.register_module(&path, module)?;

    run_test("simple::myBoolean", "true", &mut interpreter)?;
    run_test("simple::nullableBoolean", "null", &mut interpreter)?;
    
    Ok(())
}

#[test]
fn native_function_tests() -> Result<(), Box<dyn Error>> {
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

    run_test("simple::add(5,2)", "7", &mut interpreter)?;
    run_test("simple::add(5.5,2.2)", "7.7", &mut interpreter)?;
    run_test("simple::add(-5,2)", "-3", &mut interpreter)?;

    Ok(())
}

#[test]
fn basic_function_tests() -> Result<(), Box<dyn Error>> {
    let test_body = Body::Basic { 
        statements: vec![
            CortexParser::parse_statement("let x = 5;")?
        ],
        result: Some(CortexParser::parse_expression("x")?),
    };
    let test_func = Function::new(
        OptionalIdentifier::Ident(String::from("test")),
        Vec::new(),
        CortexType::number(false),
        test_body
    );
    let mut interpreter = CortexInterpreter::new();
    let mut mod_env = Environment::base();
    mod_env.add_function(test_func)?;
    let path = CortexParser::parse_path("simple")?;
    let module = Module::new(mod_env);
    interpreter.register_module(&path, module)?;

    run_test("simple::test()", "5", &mut interpreter)?;

    Ok(())
}
