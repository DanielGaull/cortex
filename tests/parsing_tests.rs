use std::error::Error;

use cortex::parsing::{codegen::r#trait::SimpleCodeGen, parser::CortexParser};

fn run_expr_test(input: &str) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(&String::from(input))?;
    let code = ast.codegen(0);
    assert_eq!(input, code);
    Ok(())
}
fn run_type_test(input: &str) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_type(&String::from(input))?;
    let code = ast.codegen(0);
    assert_eq!(input, code);
    Ok(())
}

#[test]
fn test_parse_literals() -> Result<(), Box<dyn Error>> {
    run_expr_test("5")?;
    run_expr_test("1.7")?;
    run_expr_test("\"hello\"")?;
    run_expr_test("\"true\"")?;
    run_expr_test("true")?;
    Ok(())
}

#[test]
fn test_parse_paths() -> Result<(), Box<dyn Error>> {
    run_expr_test("foo")?;
    run_expr_test("foo::bar")?;
    run_expr_test("foo::bar::baz")?;
    Ok(())
}

#[test]
fn test_types() -> Result<(), Box<dyn Error>> {
    run_type_test("number")?;
    run_type_test("any")?;
    run_type_test("number?")?;
    run_type_test("any?")?;
    Ok(())
}
