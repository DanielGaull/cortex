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
    run_expression_test("none")?;
    run_expression_test("void")?;
    run_expression_test("[1, 2, 3]")?;
    run_expression_test("'a'")?;

    run_expression_test("1:10")?;
    run_expression_test("1:10:2")?;
    run_expression_test("5:-5:-2")?;
    run_expression_test("1:")?;
    run_expression_test(":10")?;
    run_expression_test(":")?;
    
    Ok(())
}

#[test]
fn test_parse_complex_expressions() -> Result<(), Box<dyn Error>> {
    run_expression_test("print(hello, \"hi\")")?;
    run_expression_test_expected("(void)", "void")?;
    run_expression_test_expected("(((void)))", "void")?;
    run_expression_test("5 + 2")?;
    run_expression_test_expected("5 * 7 + 2", "(5 * 7) + 2")?;
    run_expression_test("5 >= 2")?;
    run_expression_test("time::Time { hours: 5, minutes: 5, seconds: 5, }")?;
    run_expression_test("simple::Box<number> { item: 5, }")?;
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
    run_expression_test_expected("item[5]", "item.__indexGet(5)")?;
    run_expression_test("trueList")?;
    run_expression_test("heap 5")?;
    run_expression_test("heap true")?;
    run_expression_test("heap foo.bar")?;
    run_expression_test("heap simple::Box<number> { item: 5, }")?;
    run_expression_test("@value")?;
    run_expression_test("@(heap 5)")?;
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
    run_type_test("Geometry::Point")?;
    run_type_test("&Point")?;
    run_type_test("&mut Point")?;
    run_type_test("(&mut Point)?")?;
    run_type_test("(&mut Geometry::Point)?")?;
    run_type_test("Box<number>")?;
    run_type_test("&Box<number>")?;
    run_type_test("&mut Box<number>")?;
    run_type_test("(number, number)")?;
    run_type_test("(number,)")?;
    run_type_test("(&mut Box<number>, &list<string>)")?;
    run_type_test("follows X")?;
    run_type_test("follows X + Y")?;
    run_type_test("follows X<number> + Y<string>")?;
    run_type_test("follows X<T> + Y<R> + Z<A, B>")?;
    Ok(())
}

#[test]
fn test_statements() -> Result<(), Box<dyn Error>> {
    run_statement_test("stop;")?;
    run_statement_test("none;")?;
    run_statement_test("print(hello, \"hi\");")?;
    run_statement_test("let x = 5;")?;
    run_statement_test("let x: number = 5;")?;
    run_statement_test("const x = 5;")?;
    run_statement_test("const x: number = 5;")?;
    run_statement_test("let ~ = 5;")?;
    run_statement_test("x = 5;")?;
    run_statement_test("while true {\n    x = x + 1;\n}")?;
    run_statement_test("myNum.increment(3);")?;
    run_statement_test_expected("myList[1] = 10;", "myList.__indexSet(1, 10);")?;
    run_statement_test_expected("myList[1] += 10;", "myList.__indexSet(1, myList.__indexGet(1) + 10);")?;
    run_statement_test("(x, y) = (5, 3);")?;
    run_statement_test("((x, y), z) = ((5, 3), 7);")?;
    run_statement_test("((x, y), z, ((w,),)) = ((5, 3), 7, ((6,),));")?;
    run_statement_test("trueList;")?;
    run_statement_test("trueList.add(item);")?; // If ident starts with true/false/void/none
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
    run_function_test_expected("fn test<T>() {\n    throw;\n}", "fn test<T>(): void {\n    throw;\n}")?;
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
        "struct Point {\n    x: number,\n    y: number,\n    fn incX(&mut this, amt: number): void {\n        this.x = this.x + amt;\n    }\n}\n",
        "struct Point {\n    y: number,\n    x: number,\n    fn incX(&mut this, amt: number): void {\n        this.x = this.x + amt;\n    }\n}\n"
    )?;
    run_top_level_test("struct Box<T> {\n    item: T,\n}\n")?;
    run_top_level_test("struct Box<T> {\n    fn doAThing<U>(&this): void {\n    }\n}\n")?;
    run_top_level_test("extend string {\n}\n")?;
    run_top_level_test("extend string {\n    fn len(&this): number {\n        5\n    }\n}\n")?;

    run_top_level_test("contract Empty {\n}\n")?;
    run_top_level_test("contract Requester {\n    fn request(&mut this): string;\n}\n")?;
    run_top_level_test("contract Iterator<T> {\n    fn next(&mut this): T;\n    fn hasNext(&this): bool;\n}\n")?;
    run_top_level_test("struct Box<T> follows Iterator<T> {\n}\n")?;
    run_top_level_test("struct Box follows C1 + C2 {\n}\n")?;
    run_top_level_test("struct Box follows C1 {\n}\n")?;
    run_top_level_test("struct Box<T> {\n}\n")?;
    run_top_level_test("struct Box {\n}\n")?;
    run_top_level_test("struct Box<T> follows Iterator<T> + Iterable<T> {\n}\n")?;
    run_top_level_test("struct Box<T,R> follows Iterator<T> + Iterable<R> {\n}\n")?;
    run_top_level_test("struct Box<T,R> follows Iterator<R, T> + Iterable<R, R> {\n}\n")?;
    run_top_level_test("struct NumList follows Iterator<number> {\n}\n")?;
    run_top_level_test("struct DTupleList<T> follows Iterator<(T, T)> {\n}\n")?;
    
    Ok(())
}
