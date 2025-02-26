use std::error::Error;

use cortex_lang::{interpreting::{env::{EnvError, Environment}, interpreter::{CortexInterpreter, InterpreterError}, module::{Module, ModuleError}, value::{CortexValue, ValueError}}, parsing::{ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, Function, Struct}, r#type::CortexType}, parser::CortexParser}};
use thiserror::Error;

#[derive(Error, Debug)]
enum TestError {
    #[error("Test error: {0}")]
    Err(&'static str),
}

#[test]
fn test_misc_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("throw 5;", InterpreterError::ProgramThrow(CortexValue::Number(5f64)), &mut interpreter)?;    
    Ok(())
}

#[test]
fn test_variable_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("dneVar = 7;", EnvError::VariableDoesNotExist(String::from("dneVar")), &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("const x = 5;")?)?;
    assert_err("let x = 7;", EnvError::VariableAlreadyExists(String::from("x")), &mut interpreter)?;
    assert_err("x = 7;", EnvError::ModifyConstant(String::from("x")), &mut interpreter)?;

    assert_err("let y: string = 5;", InterpreterError::MismatchedType(String::from("string"), String::from("number"), String::from("y")), &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("let myNum = 7;")?)?;
    assert_err("myNum = true;", InterpreterError::MismatchedType(String::from("number"), String::from("bool"), String::from("myNum")), &mut interpreter)?;
    // assert_err("dne::value;", ModuleError::ModuleDoesNotExist(String::from("dne")), &mut interpreter)?;
    // assert_err("dne::constantValue = 5;", InterpreterError::CannotModifyModuleEnvironment(String::from("dne::constantValue")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_operator_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("5 - \"foo\";", InterpreterError::InvalidOperator("number", "number"), &mut interpreter)?;
    assert_err("5.2 * \"foo\";", InterpreterError::ExpectedInteger(5.2f64), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_function_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("simple::hi();", ModuleError::FunctionDoesNotExist(String::from("hi")), &mut interpreter)?;
    assert_err("simple::add(1);", InterpreterError::MismatchedArgumentCount(String::from("add"), 2, 1), &mut interpreter)?;

    assert_err("simple::add(1, 2, 3);", InterpreterError::MismatchedArgumentCount(String::from("add"), 2, 3), &mut interpreter)?;
    assert_err("simple::add(1, true);", InterpreterError::MismatchedType(String::from("number"), String::from("bool"), String::from("b")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_struct_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("simple::Time { z: 5 };", InterpreterError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("let myTime = simple::Time { m: 5, s: 2 };")?)?;
    assert_err("myTime.z;", ValueError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    assert_err("myTime.z = 2;", ValueError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    assert_err("myTime.m = true;", InterpreterError::MismatchedType(String::from("number"), String::from("bool"), String::from("m")), &mut interpreter)?;
    assert_err("5.foo;", ValueError::CannotAccessMemberOfNonComposite, &mut interpreter)?;
    assert_err("dneStruct { foo: 5 };", ModuleError::TypeDoesNotExist(String::from("dneStruct")), &mut interpreter)?;
    assert_err("simple::Time { m: 2 };", InterpreterError::NotAllFieldsAssigned(String::from("simple::Time"), String::from("s")), &mut interpreter)?;
    assert_err("simple::Time { m: 2, m: 3 };", InterpreterError::MultipleFieldAssignment(String::from("m")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_null_related_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("let notNullable: number = null;", InterpreterError::MismatchedType(String::from("number"), String::from("null?"), String::from("notNullable")), &mut interpreter)?;
    assert_err("null!;", InterpreterError::BangCalledOnNullValue, &mut interpreter)?;
    Ok(())
}

#[test]
fn test_conditional_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("if true { 5 } else { \"hi\" };", InterpreterError::IfArmsDoNotMatch(String::from("number"), String::from("string")), &mut interpreter)?;
    assert_err("if true { 5 } elif true { 1 };", InterpreterError::IfRequiresElseBlock, &mut interpreter)?;
    assert_err("while true { 5 }", InterpreterError::LoopCannotHaveReturnValue, &mut interpreter)?;
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
        panic!("Statement did not result in an error: {}", statement);
    }
}

#[test]
fn test_other_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    interpreter.run_top_level(CortexParser::parse_top_level("fn f(): void {}")?)?;
    assert_err_toplevel("fn f(): void {}", ModuleError::FunctionAlreadyExists(String::from("f")), &mut interpreter)?;
    interpreter.run_top_level(CortexParser::parse_top_level("struct s{}")?)?;
    assert_err_toplevel("struct s{}", ModuleError::TypeAlreadyExists(String::from("s")), &mut interpreter)?;
    interpreter.run_top_level(CortexParser::parse_top_level("module m{}")?)?;
    assert_err_toplevel("module m{}", ModuleError::ModuleAlreadyExists(String::from("m")), &mut interpreter)?;
    Ok(())
}

fn assert_err_toplevel<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_top_level(statement)?;
    let evaled = interpreter.run_top_level(parsed);
    if let Err(e) = evaled {
        let error = *e.downcast::<T>().expect("Expected provided error type");
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
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
    let test_struct = Struct::new("Time", vec![
        ("m", CortexType::number(false)),
        ("s", CortexType::number(false)),
    ]);
    let mut interpreter = CortexInterpreter::new();
    let mut module = Module::new();
    module.add_function(add_func)?;
    module.add_struct(test_struct)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;
    Ok(interpreter)
}
