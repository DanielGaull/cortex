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
fn run_stmt_test(input: &str) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_statement(&String::from(input))?;
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
    run_expr_test("null")?;
    run_expr_test("void")?;
    Ok(())
}

#[test]
fn test_parse_complex_expressions() -> Result<(), Box<dyn Error>> {
    run_expr_test("println(hello, \"hi\")")?;
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

#[test]
fn test_statements() -> Result<(), Box<dyn Error>> {
    run_stmt_test("stop;")?;
    run_stmt_test("null;")?;
    run_stmt_test("println(hello, \"hi\");")?;
    run_stmt_test("let x = 5;")?;
    run_stmt_test("let x: number = 5;")?;
    run_stmt_test("const x = 5;")?;
    run_stmt_test("const x: number = 5;")?;
    run_stmt_test("x = 5;")?;
    run_stmt_test("x::y = 5;")?;
    Ok(())
}
