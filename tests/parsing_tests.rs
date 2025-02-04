use std::error::Error;

use cortex::parsing::{codegen::r#trait::SimpleCodeGen, parser::CortexParser};

fn run_expr_test(input: String) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(&input)?;
    let code = ast.codegen(0);
    assert_eq!(input, code);
    Ok(())
}

#[test]
fn test_parse_literals() -> Result<(), Box<dyn Error>> {
    run_expr_test(String::from("5"))?;
    run_expr_test(String::from("1.7"))?;
    run_expr_test(String::from("\"hello\""))?;
    run_expr_test(String::from("\"true\""))?;
    run_expr_test(String::from("true"))?;
    Ok(())
}

#[test]
fn test_parse_paths() -> Result<(), Box<dyn Error>> {
    run_expr_test(String::from("foo"))?;
    run_expr_test(String::from("foo::bar"))?;
    run_expr_test(String::from("foo::bar::baz"))?;
    Ok(())
}
