use std::error::Error;

use cortex_lang::{interpreting::{env::EnvError, error::InterpreterError, interpreter::CortexInterpreter, value::CortexValue}, parsing::{ast::{expression::{Expression, OptionalIdentifier, Parameter}, top_level::{BasicBody, Body, Bundle, Function, Struct}, r#type::CortexType}, parser::CortexParser}, preprocessing::{error::PreprocessingError, module::{Module, ModuleError}}};
use thiserror::Error;

#[derive(Error, Debug)]
enum TestError {
    #[error("Test error: {0}")]
    Err(&'static str),
}

#[test]
fn test_variable_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("dneVar = 7;", EnvError::VariableDoesNotExist(String::from("dneVar")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("const x = 5;")?)?;
    assert_err("let x = 7;", EnvError::VariableAlreadyExists(String::from("x")), &mut interpreter)?;
    assert_err("x = 7;", PreprocessingError::CannotModifyConst(String::from("x")), &mut interpreter)?;

    assert_err("let y: string = 5;", PreprocessingError::MismatchedType(String::from("string"), String::from("number"), String::from("y"), String::from("let y: string = 5;")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let myNum = 7;")?)?;
    assert_err("myNum = true;", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("myNum"), String::from("myNum = true;")), &mut interpreter)?;
    // assert_err("dne::value;", ModuleError::ModuleDoesNotExist(String::from("dne")), &mut interpreter)?;
    // assert_err("dne::constantValue = 5;", InterpreterError::CannotModifyModuleEnvironment(String::from("dne::constantValue")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_operator_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("5 - \"foo\";", PreprocessingError::InvalidOperator("number", "number"), &mut interpreter)?;
    assert_err("5.2 * \"foo\";", InterpreterError::ExpectedInteger(5.2f64), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_function_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("simple::hi();", ModuleError::FunctionDoesNotExist(String::from("simple::hi")), &mut interpreter)?;
    assert_err("simple::add(1);", PreprocessingError::MismatchedArgumentCount(String::from("simple::add"), 2, 1), &mut interpreter)?;
    assert_err("simple::add(1, 2, 3);", PreprocessingError::MismatchedArgumentCount(String::from("simple::add"), 2, 3), &mut interpreter)?;
    assert_err("simple::add(1, true);", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("b"), String::from("simple::add(1, true);")), &mut interpreter)?;
    assert_err("simple::generic<number>(true);", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("t"), String::from("simple::generic<number>(true);")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_composite_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("simple::Time { z: 5 };", PreprocessingError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let myTime = simple::Time { m: 5, s: 2 };")?)?;
    assert_err("myTime.z;", PreprocessingError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    assert_err("myTime.z = 2;", PreprocessingError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    assert_err("myTime.m = true;", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("m"), String::from("myTime.m = true;")), &mut interpreter)?;
    assert_err("5.foo;", PreprocessingError::CannotAccessMemberOfNonComposite, &mut interpreter)?;
    assert_err("dneStruct { foo: 5 };", ModuleError::TypeDoesNotExist(String::from("dneStruct")), &mut interpreter)?;
    assert_err("simple::Time { m: 2 };", PreprocessingError::NotAllFieldsAssigned(String::from("simple::Time"), String::from("s")), &mut interpreter)?;
    assert_err("simple::Time { m: 2, m: 3 };", PreprocessingError::MultipleFieldAssignment(String::from("m")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box: &simple::IntBox = simple::IntBox { v: 100 };")?)?;
    assert_err("box.v = 7;", PreprocessingError::CannotModifyFieldOnImmutableReference(String::from("simple::IntBox")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_none_related_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("let notOptional: number = none;", PreprocessingError::MismatchedType(String::from("number"), String::from("none?"), String::from("notOptional"), String::from("let notOptional: number = none;")), &mut interpreter)?;
    assert_err("none!;", InterpreterError::BangCalledOnNoneValue, &mut interpreter)?;
    Ok(())
}

#[test]
fn test_conditional_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("if true { 5 } else { \"hi\" };", PreprocessingError::IfArmsDoNotMatch(String::from("number"), String::from("string")), &mut interpreter)?;
    assert_err("if true { 5 } elif true { 1 };", PreprocessingError::IfRequiresElseBlock, &mut interpreter)?;
    assert_err("while true { 5 }", PreprocessingError::LoopCannotHaveReturnValue, &mut interpreter)?;
    Ok(())
}

fn assert_err<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_statement(statement)?;
    let evaled = interpreter.execute_statement(parsed);
    if let Err(e) = evaled {
        let msg = format!("Expected {:?}, but found {:?}", flavor, e);
        let error = *e.downcast::<T>().expect(&msg);
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
}

#[test]
fn test_other_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("throw 5;", InterpreterError::ProgramThrow(CortexValue::Number(5f64)), &mut interpreter)?;    

    interpreter.run_top_level(CortexParser::parse_top_level("fn f(): void {}")?)?;
    assert_err_toplevel("fn f(): void {}", ModuleError::FunctionAlreadyExists(String::from("f")), &mut interpreter)?;
    interpreter.run_top_level(CortexParser::parse_top_level("struct s{}")?)?;
    assert_err_toplevel("struct s{}", ModuleError::TypeAlreadyExists(String::from("s")), &mut interpreter)?;
    assert_err_toplevel("module myMod{ module m{} module m{} }", ModuleError::ModuleAlreadyExists(String::from("m")), &mut interpreter)?;

    assert_err_toplevel("struct A{a:A}", ModuleError::StructContainsCircularFields(String::from("A")), &mut interpreter)?;
    // interpreter.run_top_level(CortexParser::parse_top_level("struct B{c:C}")?)?;
    // assert_err_toplevel("struct C{b:B}", ModuleError::StructContainsCircularFields(String::from("C")), &mut interpreter)?;

    assert_err_toplevel("bundle s{}", ModuleError::TypeAlreadyExists(String::from("s")), &mut interpreter)?;
    interpreter.run_top_level(CortexParser::parse_top_level("bundle b{}")?)?;
    assert_err_toplevel("struct b{}", ModuleError::TypeAlreadyExists(String::from("b")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_tuple_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    interpreter.execute_statement(CortexParser::parse_statement("let myTuple = (5, 2);")?)?;
    assert_err("myTuple.x;", PreprocessingError::TupleMemberSyntaxInvalid(String::from("x")), &mut interpreter)?;
    assert_err("myTuple.t12;", PreprocessingError::TupleIndexValueInvalid(2, 12), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_type_argument_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;

    interpreter.run_top_level(CortexParser::parse_top_level("fn a<T>(){}")?)?;
    assert_err("a();", PreprocessingError::CouldNotInferTypeBinding(String::from("a")), &mut interpreter)?;
    assert_err_toplevel("fn b<T, T>(){}", ModuleError::DuplicateTypeArgumentName(String::from("T")), &mut interpreter)?;
    assert_err_toplevel("bundle www<T, T>{}", ModuleError::DuplicateTypeArgumentName(String::from("T")), &mut interpreter)?;
    assert_err_toplevel("bundle abc<T> {\nfn x<T>(&this) {\n}\n}", ModuleError::DuplicateTypeArgumentName(String::from("T")), &mut interpreter)?;

    interpreter.run_top_level(CortexParser::parse_top_level("bundle GenericBundle<T, R>{}")?)?;
    assert_err("GenericBundle<number>{};", PreprocessingError::MismatchedTypeArgCount(String::from("GenericBundle"), 2, 1), &mut interpreter)?;

    Ok(())
}

fn assert_err_toplevel<T: Error + PartialEq + 'static>(statement: &str, flavor: T, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let parsed = CortexParser::parse_top_level(statement)?;
    let evaled = interpreter.run_top_level(parsed);
    if let Err(e) = evaled {
        let error = *e.downcast::<T>().expect("Expected provided error type");
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Statement did not result in an error: {}", statement);
    }
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
    let generic_func = Function::new(
        OptionalIdentifier::Ident(String::from("generic")),
        vec![
            Parameter::named("t", CortexType::simple("T", false))
        ],
        CortexType::simple("T", true),
        Body::Basic(BasicBody::new(vec![], Some(Expression::None))),
        vec![String::from("T")],
    );
    let test_struct = Struct::new("Time", vec![
        ("m", CortexType::number(false)),
        ("s", CortexType::number(false)),
    ], vec![]);
    let test_bundle = Bundle::new("IntBox", vec![
        ("v", CortexType::number(false)),
    ], vec![], vec![]);
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_function(add_func)?;
    module.add_function(generic_func)?;
    module.add_struct(test_struct)?;
    module.add_bundle(test_bundle)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;
    Ok(interpreter)
}
