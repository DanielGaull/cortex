use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser, preprocessing::global::runtime_error::RuntimeError};

#[test]
fn test_list_get_set() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList[0]", "1", &mut interpreter)?;
    assert("myList[1]", "2", &mut interpreter)?;
    assert("myList[2]", "3", &mut interpreter)?;
    
    run("myList[1] = 5;", &mut interpreter)?;
    assert("myList[1]", "5", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_len() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList.len()", "3", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_find() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList.find(1)", "0", &mut interpreter)?;
    assert("myList.find(2)", "1", &mut interpreter)?;
    assert("myList.find(4)", "none", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_contains() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;
    assert("myList.contains(1)", "true", &mut interpreter)?;
    assert("myList.contains(2)", "true", &mut interpreter)?;
    assert("myList.contains(4)", "false", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_add_insert_remove() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList = [1, 2, 3];", &mut interpreter)?;

    run("myList.add(4);", &mut interpreter)?;
    assert("myList[3]", "4", &mut interpreter)?;

    run("myList.insert(1, 1.5);", &mut interpreter)?;
    assert("myList[0]", "1", &mut interpreter)?;
    assert("myList[1]", "1.5", &mut interpreter)?;
    assert("myList[2]", "2", &mut interpreter)?;

    run("myList.remove(0);", &mut interpreter)?;
    assert("myList[0]", "1.5", &mut interpreter)?;

    run("myList = [1, 2, 3];", &mut interpreter)?;
    run("myList.insert(3, 10);", &mut interpreter)?;
    assert("myList[0]", "1", &mut interpreter)?;
    assert("myList[3]", "10", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_list_index_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myList: &list<i32> = [1, 2, 3];", &mut interpreter)?;
    // assert_err("myList[-2];", RuntimeError::InvalidIndex(-2usize, 3usize), &mut interpreter)?; // Should now throw a type error
    assert_err("myList[4];", RuntimeError::InvalidIndex(4usize, 3usize), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_index() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    assert("str[0]", "'h'", &mut interpreter)?;
    assert("str[1]", "'e'", &mut interpreter)?;
    assert("str[2]", "'l'", &mut interpreter)?;
    assert("str[3]", "'l'", &mut interpreter)?;
    assert("str[4]", "'o'", &mut interpreter)?;
    assert("str[5]", "'!'", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_len() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    assert("str.len()", "6", &mut interpreter)?;
    Ok(())
}

#[allow(non_snake_case)]
#[test]
fn test_string_isEmpty() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    assert("str.isEmpty()", "false", &mut interpreter)?;
    assert("\"\".isEmpty()", "true", &mut interpreter)?;
    Ok(())
}

#[allow(non_snake_case)]
#[test]
fn test_string_startsWith_endsWith_contains() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    assert("str.startsWith(\"he\")", "true", &mut interpreter)?;
    assert("str.startsWith(\"lo!\")", "false", &mut interpreter)?;
    assert("str.startsWith(\"nothing\")", "false", &mut interpreter)?;

    assert("str.endsWith(\"he\")", "false", &mut interpreter)?;
    assert("str.endsWith(\"lo!\")", "true", &mut interpreter)?;
    assert("str.startsWith(\"nothing\")", "false", &mut interpreter)?;
    
    assert("str.contains(\"he\")", "true", &mut interpreter)?;
    assert("str.contains(\"lo!\")", "true", &mut interpreter)?;
    assert("str.contains(\"nothing\")", "false", &mut interpreter)?;
    Ok(())
}

#[allow(non_snake_case)]
#[test]
fn test_string_indexOf() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    assert("str.indexOf(\"llo\")", "2", &mut interpreter)?;
    assert("str.indexOf(\"invalid\")", "none", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_trim_replace() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"  hello  !   \";", &mut interpreter)?;
    assert("str.trim()", "\"hello  !\"", &mut interpreter)?;
    assert("str.replace(\"hello\", \"goodbye\")", "\"  goodbye  !   \"", &mut interpreter)?;
    assert("str.replace(\"l\", \"\")", "\"  heo  !   \"", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_reverse() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    assert("str.reverse()", "\"!olleh\"", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_padding() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"foo\";", &mut interpreter)?;
    assert("str.padStart(5, ' ')", "\"  foo\"", &mut interpreter)?;
    assert("str.padStart(3, ' ')", "\"foo\"", &mut interpreter)?;
    assert("str.padStart(1, ' ')", "\"foo\"", &mut interpreter)?;

    assert("str.padEnd(5, ' ')", "\"foo  \"", &mut interpreter)?;
    assert("str.padEnd(3, ' ')", "\"foo\"", &mut interpreter)?;
    assert("str.padEnd(1, ' ')", "\"foo\"", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_repeat() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"foo\";", &mut interpreter)?;
    assert("str.repeat(5)", "\"foofoofoofoofoo\"", &mut interpreter)?;
    assert("str.repeat(1)", "\"foo\"", &mut interpreter)?;
    assert("str.repeat(0)", "\"\"", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_split() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"foo,bar,baz\";", &mut interpreter)?;
    assert("toString(str.split(\",\"))", "\"&([foo, bar, baz])\"", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_string_index_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let str: string = \"hello!\";", &mut interpreter)?;
    // assert_err("str[-2];", RuntimeError::InvalidIndex(-2f64, 6usize), &mut interpreter)?; // Should now throw a type error
    assert_err("str[10];", RuntimeError::InvalidIndex(10usize, 6usize), &mut interpreter)?;

    assert_err("str.repeat(-1);", RuntimeError::ExpectedInteger(-1f64), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_char_conditions() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let (ch1, ch2, ch3) = (\'h\', \'1\', \' \');", &mut interpreter)?;
    assert("ch1.isAlpha()", "true", &mut interpreter)?;
    assert("ch1.isDigit()", "false", &mut interpreter)?;
    assert("ch1.isWhitespace()", "false", &mut interpreter)?;
    assert("ch1.isAlphanumeric()", "true", &mut interpreter)?;

    assert("ch2.isAlpha()", "false", &mut interpreter)?;
    assert("ch2.isDigit()", "true", &mut interpreter)?;
    assert("ch2.isWhitespace()", "false", &mut interpreter)?;
    assert("ch2.isAlphanumeric()", "true", &mut interpreter)?;

    assert("ch3.isAlpha()", "false", &mut interpreter)?;
    assert("ch3.isDigit()", "false", &mut interpreter)?;
    assert("ch3.isWhitespace()", "true", &mut interpreter)?;
    assert("ch3.isAlphanumeric()", "false", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_char_upper_lower() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let (ch1, ch2, ch3) = (\'h\', \'A\', \' \');", &mut interpreter)?;
    assert("ch1.toUpper()", "'H'", &mut interpreter)?;
    assert("ch1.toLower()", "'h'", &mut interpreter)?;

    assert("ch2.toUpper()", "'A'", &mut interpreter)?;
    assert("ch2.toLower()", "'a'", &mut interpreter)?;

    assert("ch3.toUpper()", "' '", &mut interpreter)?;
    assert("ch3.toLower()", "' '", &mut interpreter)?;
    Ok(())
}

#[test]
fn test_range_basic() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run("let myRange: range = 1:10:5;", &mut interpreter)?;
    assert("myRange.start", "1isz", &mut interpreter)?;
    assert("myRange.end", "10isz", &mut interpreter)?;
    assert("myRange.step", "5isz", &mut interpreter)?;

    run("myRange = :;", &mut interpreter)?;
    assert("myRange.start", "none", &mut interpreter)?;
    assert("myRange.end", "none", &mut interpreter)?;
    assert("myRange.step", "none", &mut interpreter)?;

    run("myRange = 1:;", &mut interpreter)?;
    assert("myRange.start", "1isz", &mut interpreter)?;
    assert("myRange.end", "none", &mut interpreter)?;
    assert("myRange.step", "none", &mut interpreter)?;

    run("myRange = :5;", &mut interpreter)?;
    assert("myRange.start", "none", &mut interpreter)?;
    assert("myRange.end", "5isz", &mut interpreter)?;
    assert("myRange.step", "none", &mut interpreter)?;

    run("myRange = 1:10;", &mut interpreter)?;
    assert("myRange.start", "1isz", &mut interpreter)?;
    assert("myRange.end", "10isz", &mut interpreter)?;
    assert("myRange.step", "none", &mut interpreter)?;

    Ok(())
}

#[test]
fn test_substring() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    assert("\"hello world\".substring(0:5)", "\"hello\"", &mut interpreter)?;
    assert("\"hello world\".substring(6:11)", "\"world\"", &mut interpreter)?;
    assert("\"test\".substring(:)", "\"test\"", &mut interpreter)?;
    assert("\"abcdef\".substring(2:)", "\"cdef\"", &mut interpreter)?;
    assert("\"abc\".substring(1:1)", "\"\"", &mut interpreter)?;
    assert("\"abc\".substring(3:)", "\"\"", &mut interpreter)?;
    assert("\"abcdef\".substring(:3)", "\"abc\"", &mut interpreter)?;
    Ok(())
}

fn assert(input: &str, expected: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let value = interpreter.execute_expression(ast)?;
    let value_string = format!("{}", value);
    assert_eq!(expected, value_string);
    Ok(())
}
fn run(st: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    interpreter.execute_statement(CortexParser::parse_statement(st)?)?;
    Ok(())
}

fn assert_err<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_statement(statement)?;
    let evaled = interpreter.execute_statement(parsed);
    if let Err(e) = evaled {
        if !e.is::<T>() {
            panic!("Value e {:?} is not T {:?}", e, std::any::type_name::<T>());
        }
        let error = (&*e).downcast_ref::<T>().expect("Error downcast failed");
        assert_eq!(flavor, *error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
}
