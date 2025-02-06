use std::error::Error;

use cortex::{interpreting::{env::{EnvError, Environment}, interpreter::{CortexInterpreter, InterpreterError}, module::Module, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Function}, r#type::CortexType}, parser::CortexParser}};
use thiserror::Error;

#[derive(Error, Debug)]
enum TestError {
    #[error("Test error: {0}")]
    Err(&'static str),
}

#[test]
fn test_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("stop;", InterpreterError::ProgramStopped, &mut interpreter)?;
    assert_err("simple::hi();", EnvError::FunctionDoesNotExist(String::from("hi")), &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("const x = 5;")?)?;
    assert_err("x = 7;", EnvError::ModifyConstant(String::from("x")), &mut interpreter)?;
    assert_err("let y: string = 5;", InterpreterError::MismatchedType(String::from("string"), String::from("number")), &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("let myNum = 7;")?)?;
    assert_err("myNum = true;", InterpreterError::MismatchedType(String::from("number"), String::from("bool")), &mut interpreter)?;
    Ok(())
}

fn assert_err<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_statement(statement)?;
    let evaled = interpreter.run_statement(&parsed);
    if let Err(e) = evaled {
        let error = *e.downcast::<T>().expect("Expected provided error type");
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Statement did not result in an error");
    }
}

fn setup_interpreter() -> Result<CortexInterpreter, Box<dyn Error>> {
    let add_body = Body::Native(|env| {
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
    });
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
