use cortex_lang::{interpreting::{interpreter::CortexInterpreter, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, Parameter}, top_level::{Body, PFunction}, r#type::CortexType}, parser::CortexParser}, preprocessing::module::Module};
use thiserror::Error;
use std::error::Error;

#[derive(Error, Debug)]
enum TestError {
    #[error("Test error: {0}")]
    Err(&'static str),
}

fn run_statement(input: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_statement(input)?;
    interpreter.execute_statement(ast)?;
    Ok(())
}
fn assert_expression(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}

fn setup_interpreter() -> Result<CortexInterpreter, Box<dyn Error>> {
    let add_body = Body::Native(Box::new(|env, _| -> Result<CortexValue, Box<dyn Error>> {
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
    let add_func = PFunction::new(
        OptionalIdentifier::Ident(String::from("add")),
        vec![
            Parameter::named("a", CortexType::number(false)),
            Parameter::named("b", CortexType::number(false))
        ],
        CortexType::number(false),
        add_body,
        vec![],
    );
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_function(add_func)?;
    let path = CortexParser::parse_path("simple")?;
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

    run_statement("z += 1;", &mut interpreter)?;
    assert_expression("z", "11", &mut interpreter)?;
    run_statement("z -= 1;", &mut interpreter)?;
    assert_expression("z", "10", &mut interpreter)?;
    run_statement("z *= 2;", &mut interpreter)?;
    assert_expression("z", "20", &mut interpreter)?;
    run_statement("z /= 2;", &mut interpreter)?;
    assert_expression("z", "10", &mut interpreter)?;
    run_statement("z %= 2;", &mut interpreter)?;
    assert_expression("z", "0", &mut interpreter)?;

    run_statement("let a = true;", &mut interpreter)?;
    run_statement("a &&= false;", &mut interpreter)?;
    assert_expression("a", "false", &mut interpreter)?;
    run_statement("a ||= true;", &mut interpreter)?;
    assert_expression("a", "true", &mut interpreter)?;

    run_statement("let myNum = 0;", &mut interpreter)?;
    run_statement("while myNum < 10 { myNum += 1; }", &mut interpreter)?;
    assert_expression("myNum", "10", &mut interpreter)?;

    run_statement("let myOptionalNum: number? = 5;", &mut interpreter)?;
    assert_expression("myOptionalNum", "5", &mut interpreter)?;
    run_statement("myOptionalNum = none;", &mut interpreter)?;
    assert_expression("myOptionalNum", "none", &mut interpreter)?;

    run_statement("let myt = (1, 2, 3);", &mut interpreter)?;
    run_statement("myt.t0 = 6;", &mut interpreter)?;
    assert_expression("myt.t0", "6", &mut interpreter)?;

    Ok(())
}

#[test]
fn tuple_assign_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;

    run_statement("let x = 0;", &mut interpreter)?;
    run_statement("let y = 0;", &mut interpreter)?;
    run_statement("(x, y) = (5, 3);", &mut interpreter)?;
    assert_expression("x", "5", &mut interpreter)?;
    assert_expression("y", "3", &mut interpreter)?;

    run_statement("let z = 0;", &mut interpreter)?;
    run_statement("let w = 0;", &mut interpreter)?;
    run_statement("((x, y), z, ((w,),)) = ((1, 2), 7, ((6,),));", &mut interpreter)?;
    assert_expression("x", "1", &mut interpreter)?;
    assert_expression("y", "2", &mut interpreter)?;
    assert_expression("z", "7", &mut interpreter)?;
    assert_expression("w", "6", &mut interpreter)?;

    run_statement("(x, ~, ~) = (10, 9, 8);", &mut interpreter)?;
    assert_expression("x", "10", &mut interpreter)?;

    Ok(())
}

#[test]
fn tuple_declaration_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;

    run_statement("let (x, y) = (3, 5);", &mut interpreter)?;
    assert_expression("x", "3", &mut interpreter)?;
    assert_expression("y", "5", &mut interpreter)?;

    run_statement("let (tx, ty, tz): (number, number, number) = (1, 2, 3);", &mut interpreter)?;
    assert_expression("tx", "1", &mut interpreter)?;
    assert_expression("ty", "2", &mut interpreter)?;
    assert_expression("tz", "3", &mut interpreter)?;

    run_statement("let (a, ~, ~) = (10, 9, 8);", &mut interpreter)?;
    assert_expression("a", "10", &mut interpreter)?;

    Ok(())
}
