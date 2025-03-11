use std::error::Error;
use paste::paste;

use cortex_lang::parsing::{codegen::r#trait::SimpleCodeGen, parser::CortexParser};

macro_rules! parse_test {
    ($name:ident) => {
        paste! {
            fn [<run_ $name _test>](input: &str) -> Result<(), Box<dyn Error>> {
                [<run_ $name _test_expected>](input, input)
            }
            fn [<run_ $name _test_expected>](input: &str, expected: &str) -> Result<(), Box<dyn Error>> {
                let ast = CortexParser::[<parse_ $name>](input)?;
                let code = ast.codegen(0);
                assert_eq!(expected, code);
                Ok(())
            }
            #[allow(dead_code)]
            fn [<run_ $name _test_or>](input: &str, other: &str) -> Result<(), Box<dyn Error>> {
                let ast = CortexParser::[<parse_ $name>](input)?;
                let code = ast.codegen(0);
                if input == code {
                    assert_eq!(input, code);
                } else {
                    assert_eq!(other, code);
                }
                Ok(())
            }
        }
    }
}

parse_test!(expression);
parse_test!(type);
parse_test!(statement);
parse_test!(function);
parse_test!(top_level);

#[test]
fn test_parse_literals() -> Result<(), Box<dyn Error>> {
    run_expression_test("5")?;
    run_expression_test("1.7")?;
    run_expression_test("\"hello\"")?;
    run_expression_test("\"true\"")?;
    run_expression_test("true")?;
    run_expression_test("null")?;
    run_expression_test("void")?;
    Ok(())
}

#[test]
fn test_parse_complex_expressions() -> Result<(), Box<dyn Error>> {
    run_expression_test("print(hello, \"hi\")")?;
    run_expression_test("(void)")?;
    run_expression_test("(((void)))")?;
    run_expression_test("5 + 2")?;
    run_expression_test("5 * 7 + 2")?;
    run_expression_test("5 >= 2")?;
    run_expression_test("time::Time { hours: 5, minutes: 5, seconds: 5, }")?;
    run_expression_test("test!")?;
    run_expression_test("test != 7")?;
    run_expression_test("foo.bar")?;
    run_expression_test("foo.bar.baz")?;
    run_expression_test("if hi {\n    doThing();\n} elif foo {\n    bar();\n} else {\n    doOtherThing();\n}")?;
    run_expression_test("-foo")?;
    run_expression_test("!foo")?;
    run_expression_test("-foo()")?;
    run_expression_test("!foo()")?;
    run_expression_test("-foo.bar")?;
    run_expression_test("!foo.bar")?;
    run_expression_test("myNum.increment(3)")?;
    run_expression_test("myNum.ref.ref().ref.increment(3)")?;
    Ok(())
}

#[test]
fn test_parse_paths() -> Result<(), Box<dyn Error>> {
    run_expression_test("foo")?;
    run_expression_test("foo::bar")?;
    run_expression_test("foo::bar::baz")?;
    Ok(())
}

#[test]
fn test_types() -> Result<(), Box<dyn Error>> {
    run_type_test("number")?;
    run_type_test("number?")?;
    run_type_test("&Point")?;
    run_type_test("&mut Point")?;
    run_type_test("Box<number>")?;
    run_type_test("&Box<number>")?;
    run_type_test("&mut Box<number>")?;
    Ok(())
}

#[test]
fn test_statements() -> Result<(), Box<dyn Error>> {
    run_statement_test("stop;")?;
    run_statement_test("null;")?;
    run_statement_test("print(hello, \"hi\");")?;
    run_statement_test("let x = 5;")?;
    run_statement_test("let x: number = 5;")?;
    run_statement_test("const x = 5;")?;
    run_statement_test("const x: number = 5;")?;
    run_statement_test("let ~ = 5;")?;
    run_statement_test("x = 5;")?;
    run_statement_test("while true {\n    x += 1;\n}")?;
    run_statement_test("myNum.increment(3);")?;
    Ok(())
}

#[test]
fn test_functions() -> Result<(), Box<dyn Error>> {
    run_function_test("fn test(x: number): void {\n    stop;\n}")?;
    run_function_test("fn test(x: number): void {\n    const x: number = 5;\n    x;\n}")?;
    run_function_test("fn test(x: number): number {\n    const x: number = 5;\n    x\n}")?;
    run_function_test("fn ~(x: number): void {\n    throw;\n}")?;
    run_function_test("fn test(): void {\n    throw;\n}")?;
    run_function_test("fn ~(): void {\n    throw;\n}")?;
    run_function_test_expected("fn test() {\n    throw;\n}", "fn test(): void {\n    throw;\n}")?;
    run_function_test_expected("fn ~() {\n    throw;\n}", "fn ~(): void {\n    throw;\n}")?;
    Ok(())
}

#[test]
fn test_top_level() -> Result<(), Box<dyn Error>> {
    run_top_level_test("fn test(x: number): void {\n    stop;\n}")?;
    run_top_level_test("module myMod {\n    fn test(x: number): void {\n        stop;\n    }\n}")?;
    run_top_level_test("import hello;")?;
    run_top_level_test("import \"hello\";")?;
    run_top_level_test_or(
        "struct Point {\n    x: number,\n    y: number,\n}\n",
        "struct Point {\n    y: number,\n    x: number,\n}\n"
    )?;
    run_top_level_test_or(
        "bundle Point {\n    x: number,\n    y: number,\n}\n",
        "bundle Point {\n    y: number,\n    x: number,\n}\n"
    )?;
    run_top_level_test_or(
        "bundle Point {\n    x: number,\n    y: number,\n    fn incX(&mut this, amt: number): void {\n        this.x += amt;\n    }\n}\n",
        "bundle Point {\n    y: number,\n    x: number,\n    fn incX(&mut this, amt: number): void {\n        this.x += amt;\n    }\n}\n"
    )?;
    run_top_level_test("struct Box<T> {\n    item: T,\n}\n")?;
    run_top_level_test("bundle Box<T> {\n    item: T,\n}\n")?;
    Ok(())
}
