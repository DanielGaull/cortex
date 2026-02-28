use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn simple_static_function_test() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let s = "struct StaticTest {
            fn myStaticFn(static, x: i32, y: i32): i32 {
                x + y
            }
        }";
    interpreter.run_top_level(CortexParser::parse_top_level(s)?)?;
    interpreter.build_modules()?;
    run_test("StaticTest.myStaticFn(3, 5)", "8", &mut interpreter)?;
    Ok(())
}

#[test]
fn generic_static_function_test() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let s = "struct StaticTest<T> {
            fn identity(static, thing: T): T {
                thing
            }
        }";
    interpreter.run_top_level(CortexParser::parse_top_level(s)?)?;
    interpreter.build_modules()?;
    run_test("StaticTest<i32>.identity(5)", "5", &mut interpreter)?;
    Ok(())
}

fn run_test(
    input: &str,
    expected: &str,
    interpreter: &mut CortexInterpreter,
) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
