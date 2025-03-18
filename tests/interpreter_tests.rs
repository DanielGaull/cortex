use std::error::Error;

use cortex_lang::{interpreting::{env::Environment, interpreter::CortexInterpreter, module::Module, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, Parameter, PathIdent}, top_level::{BasicBody, Body, Bundle, Function, Struct}, r#type::CortexType}, parser::CortexParser}};
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
    let mut interpreter = CortexInterpreter::new()?;
    run_test("5", "5", &mut interpreter)?;
    run_test("5.3", "5.3", &mut interpreter)?;
    run_test("true", "true", &mut interpreter)?;
    run_test("false", "false", &mut interpreter)?;
    run_test("\"hello\"", "\"hello\"", &mut interpreter)?;
    run_test("none", "none", &mut interpreter)?;
    run_test("void", "void", &mut interpreter)?;
    Ok(())
}

#[test]
fn op_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run_test("5 + 2", "7", &mut interpreter)?;
    run_test("5 - 2", "3", &mut interpreter)?;
    run_test("5 * 2", "10", &mut interpreter)?;
    run_test("4 / 2", "2", &mut interpreter)?;
    run_test("5 % 2", "1", &mut interpreter)?;
    run_test("true && false", "false", &mut interpreter)?;
    run_test("true || false", "true", &mut interpreter)?;
    run_test("5 == 2", "false", &mut interpreter)?;
    run_test("5 != 2", "true", &mut interpreter)?;
    run_test("5 > 2", "true", &mut interpreter)?;
    run_test("5 >= 2", "true", &mut interpreter)?;
    run_test("5 < 2", "false", &mut interpreter)?;
    run_test("5 <= 2", "false", &mut interpreter)?;

    run_test("\"a\" + \"b\"", "\"ab\"", &mut interpreter)?;
    run_test("\"a\" * 3", "\"aaa\"", &mut interpreter)?;
    run_test("3 * \"a\"", "\"aaa\"", &mut interpreter)?;

    run_test("5 * 7 + 2", "37", &mut interpreter)?;
    run_test("5 * (7 + 2)", "45", &mut interpreter)?;
    run_test("3 * 2 == 6 && 5 * 3 == 15", "true", &mut interpreter)?;

    run_test("!false", "true", &mut interpreter)?;
    run_test("-(5+2)", "-7", &mut interpreter)?;
    Ok(())
}

#[test]
fn complex_expr_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run_test("if true{1} else {0}", "1", &mut interpreter)?;
    run_test("if false{1}elif true{2} else {0}", "2", &mut interpreter)?;
    run_test("if false{1}elif 0 == 1{2} else {0}", "0", &mut interpreter)?;
    run_test("if false{1}elif 0 == 1{2} else {none}", "none", &mut interpreter)?;
    Ok(())
}

// #[test]
// fn mod_var_eval_tests() -> Result<(), Box<dyn Error>> {
//     let mut interpreter = CortexInterpreter::new();
//     let mut module = Environment::base();
//     mod_env.add_const(String::from("myBoolean"), CortexType::boolean(false), CortexValue::Boolean(true))?;
//     mod_env.add_const(String::from("optionalBoolean"), CortexType::boolean(true), CortexValue::None)?;
//     let path = CortexParser::parse_path("simple")?;
//     let module = Module::new(mod_env);
//     interpreter.register_module(&path, module)?;

//     run_test("simple::myBoolean", "true", &mut interpreter)?;
//     run_test("simple::optionalBoolean", "none", &mut interpreter)?;
    
//     Ok(())
// }

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
        add_body,
        vec![],
    );
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_function(add_func)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;

    run_test("simple::add(5,2)", "7", &mut interpreter)?;
    run_test("simple::add(5.5,2.2)", "7.7", &mut interpreter)?;
    run_test("simple::add(-5,2)", "-3", &mut interpreter)?;

    interpreter.run_statement(&CortexParser::parse_statement("let a = 2;")?)?;
    run_test("simple::add(a, 3)", "5", &mut interpreter)?;

    Ok(())
}

#[test]
fn basic_function_tests() -> Result<(), Box<dyn Error>> {
    let test_body = Body::Basic(BasicBody::new( 
        vec![
            CortexParser::parse_statement("let x = 5;")?
        ],
        Some(CortexParser::parse_expression("x")?),
    ));
    let test_func = Function::new(
        OptionalIdentifier::Ident(String::from("test")),
        Vec::new(),
        CortexType::number(false),
        test_body,
        vec![],
    );
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_function(test_func)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;

    run_test("simple::test()", "5", &mut interpreter)?;

    Ok(())
}

#[test]
fn struct_tests() -> Result<(), Box<dyn Error>> {
    let test_struct = Struct::new("Time", vec![
        ("m", CortexType::number(false)),
        ("s", CortexType::number(false)),
    ], vec![]);
    let date_struct = Struct::new("Date", vec![
        ("t", CortexType::basic(PathIdent::new(vec!["Time"]), false, vec![])),
    ], vec![]);
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_struct(test_struct)?;
    module.add_struct(date_struct)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;

    interpreter.run_statement(&CortexParser::parse_statement("let time = simple::Time{m:5,s:10};")?)?;
    run_test("time.m", "5", &mut interpreter)?;
    run_test("time.s", "10", &mut interpreter)?;

    interpreter.run_statement(&CortexParser::parse_statement("time.m = 7;")?)?;
    run_test("time.m", "7", &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("time.m += 7;")?)?;
    run_test("time.m", "14", &mut interpreter)?;

    interpreter.run_statement(&CortexParser::parse_statement("let date = simple::Date{t:time};")?)?;
    run_test("date.t.m", "14", &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("date.t.s = 100;")?)?;
    run_test("date.t.s", "100", &mut interpreter)?;
    // Structs are pass-by-value; important to keep them small so copying them is cheap
    run_test("time.s", "10", &mut interpreter)?;

    Ok(())
}

#[test]
fn bundle_tests() -> Result<(), Box<dyn Error>> {
    let test_bundle = Bundle::new("Time", vec![
        ("m", CortexType::number(false)),
        ("s", CortexType::number(false)),
    ], vec![], vec![]);
    let date_bundle = Bundle::new("Date", vec![
        ("t", CortexType::reference(CortexType::basic(PathIdent::new(vec!["Time"]), false, vec![]), true)),
    ], vec![], vec![]);
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_bundle(test_bundle)?;
    module.add_bundle(date_bundle)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;

    interpreter.run_statement(&CortexParser::parse_statement("let time = simple::Time{m:5,s:10};")?)?;
    run_test("time.m", "5", &mut interpreter)?;
    run_test("time.s", "10", &mut interpreter)?;

    interpreter.run_statement(&CortexParser::parse_statement("time.m = 7;")?)?;
    run_test("time.m", "7", &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("time.m += 7;")?)?;
    run_test("time.m", "14", &mut interpreter)?;

    interpreter.run_statement(&CortexParser::parse_statement("let date = simple::Date{t:time};")?)?;
    run_test("date.t.m", "14", &mut interpreter)?;
    interpreter.run_statement(&CortexParser::parse_statement("date.t.s = 100;")?)?;
    run_test("date.t.s", "100", &mut interpreter)?;
    // Bundles are pass-by-reference
    run_test("time.s", "100", &mut interpreter)?;

    Ok(())
}
