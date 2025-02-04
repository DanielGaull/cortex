use std::error::Error;

use cortex::parsing::parser::CortexParser;

fn run_expr_test(input: String) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    
    Ok(())
}

#[test]
fn test_parse_literals() -> Result<(), Box<dyn Error>> {
    

    Ok(())
}
