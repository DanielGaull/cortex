use std::error::Error;

use cortex_lang::{interpreting::{env::EnvError, error::InterpreterError, interpreter::CortexInterpreter, value::CortexValue}, parsing::{ast::{expression::{OptionalIdentifier, PExpression, Parameter}, top_level::{BasicBody, Body, PFunction, Struct}}, parser::{CortexParser, ParseError}}, preprocessing::{error::PreprocessingError, module::{Module, ModuleError}}, r#type::r#type::{CortexType, TypeParam}};
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
    assert_err("5 - \"foo\";", PreprocessingError::InvalidOperator("number", "number", "-", String::from("number"), String::from("string")), &mut interpreter)?;
    assert_err("5.2 * \"foo\";", InterpreterError::ExpectedInteger(5.2f64), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_function_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("simple::hi();", PreprocessingError::FunctionDoesNotExist(String::from("simple::hi")), &mut interpreter)?;
    assert_err("simple::add(1);", PreprocessingError::MismatchedArgumentCount(String::from("simple::add"), 2, 1), &mut interpreter)?;
    assert_err("simple::add(1, 2, 3);", PreprocessingError::MismatchedArgumentCount(String::from("simple::add"), 2, 3), &mut interpreter)?;
    assert_err("simple::add(1, true);", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("b"), String::from("simple::add(1, true)")), &mut interpreter)?;
    assert_err("simple::generic<number>(true);", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("t"), String::from("simple::generic<number>(true)")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_contract_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;

    interpreter.run_top_level(CortexParser::parse_top_level("contract Iterator<T> {
        fn next(&mut this): T;
    }")?)?;
    interpreter.run_top_level(CortexParser::parse_top_level("contract NetworkRequester {
        fn next(&this): string;
    }")?)?;

    interpreter.run_top_level(CortexParser::parse_top_level("struct NumList follows Iterator<number> {
        i: number,
        values: &list<number>,

        fn next(&mut this): number {
            let value = this.values[this.i];
            this.i += 1;
            value
        }
    }")?)?;

    assert_err_toplevel("struct NumList1 follows Iterator<number> {}", PreprocessingError::ContractFunctionsMissing(String::from("next")), &mut interpreter)?;
    assert_err_toplevel("extend number follows Iterator<number> {}", PreprocessingError::ContractFunctionsMissing(String::from("next")), &mut interpreter)?;
    assert_err_toplevel("struct NumList2 follows NumIterator {}", PreprocessingError::ContractDoesNotExist(String::from("NumIterator")), &mut interpreter)?;
    assert_err_toplevel("struct NumList3 follows Iterator<number> + NetworkRequester {
        fn next(&mut this): number {
            5
        }
    }", PreprocessingError::AmbiguousFunctionFromMultipleContracts(String::from("next")), &mut interpreter)?;
    assert_err_toplevel("struct NumList4 follows NetworkRequester + NetworkRequester {
        fn next(&this): number {
            5
        }
    }", PreprocessingError::DuplicateInFollowsClause(String::from("NetworkRequester")), &mut interpreter)?;

    interpreter.run_top_level(CortexParser::parse_top_level("struct NumList5 follows Iterator<number> { i: number, values: &list<number>, fn next(&mut this): number { let result = this.values[this.i]; this.i += 1; result } }")?)?;
    interpreter.execute_statement(CortexParser::parse_statement("let x: follows Iterator<number> = heap NumList { i: 0, values: [1, 2] };")?)?;
    assert_err("x.dne();", PreprocessingError::FunctionDoesNotExist(String::from("dne")), &mut interpreter)?;
    assert_err("x.i;", PreprocessingError::CannotAccessMemberOfFollowsType, &mut interpreter)?;

    Ok(())
}

#[test]
fn test_composite_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("simple::Time { z: 5 };", PreprocessingError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let myTime = simple::Time { m: 5, s: 2 };")?)?;
    assert_err("myTime.z;", PreprocessingError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    assert_err("myTime.z = 2;", PreprocessingError::FieldDoesNotExist(String::from("z"), String::from("simple::Time")), &mut interpreter)?;
    assert_err("myTime.m = true;", PreprocessingError::MismatchedType(String::from("number"), String::from("bool"), String::from("myTime.m"), String::from("myTime.m = true;")), &mut interpreter)?;
    assert_err("5.foo;", PreprocessingError::CannotAccessMemberOfNonComposite, &mut interpreter)?;
    assert_err("dneStruct { foo: 5 };", PreprocessingError::TypeDoesNotExist(String::from("dneStruct")), &mut interpreter)?;
    assert_err("simple::Time { m: 2 };", PreprocessingError::NotAllFieldsAssigned(String::from("simple::Time"), String::from("s")), &mut interpreter)?;
    assert_err("simple::Time { m: 2, m: 3 };", PreprocessingError::MultipleFieldAssignment(String::from("m")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box: &simple::IntBox = heap simple::IntBox { v: 100 };")?)?;
    assert_err("box.v = 7;", PreprocessingError::CannotModifyFieldOnImmutableReference(String::from("simple::IntBox")), &mut interpreter)?;
    assert_err("5.hello();", PreprocessingError::FunctionDoesNotExist(String::from("hello (on type number)")), &mut interpreter)?;
    Ok(())
}

#[test]
fn nullable_member_access_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    interpreter.execute_statement(CortexParser::parse_statement("let x: (bool,)? = (true,);")?)?;
    assert_err("x.t0;", PreprocessingError::CannotAccessMemberOfOptional(String::from("x")), &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let myTime: simple::Time? = simple::Time { m: 5, s: 2 };")?)?;
    assert_err("myTime.m;", PreprocessingError::CannotAccessMemberOfOptional(String::from("myTime")), &mut interpreter)?;
    Ok(())
}

#[test]
fn test_none_related_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("let notOptional: number = none;", PreprocessingError::MismatchedType(String::from("number"), String::from("none"), String::from("notOptional"), String::from("let notOptional: number = none;")), &mut interpreter)?;
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

#[test]
fn test_tuple_var_errors() -> Result<(), Box<dyn Error>> {
    let mut interpreter = setup_interpreter()?;
    assert_err("let (x, y): number = (1, 2);", PreprocessingError::MismatchedType(String::from("number"), String::from("(number, number)"), String::from("$temp0"), String::from("const $temp0: number = (1, 2);")), &mut interpreter)?;
    assert_err("let (x, y): (bool, bool) = (1, 2);", PreprocessingError::MismatchedType(String::from("(bool, bool)"), String::from("(number, number)"), String::from("$temp1"), String::from("const $temp1: (bool, bool) = (1, 2);")), &mut interpreter)?;
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
    assert_err_toplevel("module myMod{ module m{} module m{} }", ModuleError::ModuleAlreadyExists(String::from("m")), &mut interpreter)?;

    assert_err_toplevel("struct A{a:A}", PreprocessingError::StructContainsCircularFields(String::from("A")), &mut interpreter)?;
    // interpreter.run_top_level(CortexParser::parse_top_level("struct B{c:C}")?)?;
    // assert_err_toplevel("struct C{b:B}", ModuleError::StructContainsCircularFields(String::from("C")), &mut interpreter)?;

    interpreter.run_top_level(CortexParser::parse_top_level("struct s{}")?)?;
    assert_err_toplevel("struct s{}", ModuleError::StructAlreadyExists(String::from("s")), &mut interpreter)?;

    assert_err_equal(CortexParser::parse_top_level("struct A{a:number, a:number}").map_err(|e| Box::new(e) as Box<dyn Error>), ParseError::CompositeContainsDuplicateFields(String::from("A"), String::from("a")))?;

    interpreter.run_top_level(CortexParser::parse_top_level("contract c{}")?)?;
    assert_err_toplevel("contract c{}", ModuleError::ContractAlreadyExists(String::from("c")), &mut interpreter)?;

    assert_err("let myList = [];", PreprocessingError::CannotDetermineType(String::from("[]")), &mut interpreter)?;

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
    assert_err_toplevel("struct www<T, T>{}", ModuleError::DuplicateTypeArgumentName(String::from("T")), &mut interpreter)?;
    assert_err_toplevel("struct abc<T> {\nfn x<T>(&this) {\n}\n}", ModuleError::DuplicateTypeArgumentName(String::from("T")), &mut interpreter)?;

    interpreter.run_top_level(CortexParser::parse_top_level("struct GenericStruct<T, R>{}")?)?;
    assert_err("GenericStruct<number>{};", PreprocessingError::MismatchedTypeArgCount(String::from("GenericStruct"), 2, 1, "Type"), &mut interpreter)?;

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
fn assert_err_equal<T: Error + PartialEq + 'static, S>(result: Result<S, Box<dyn Error>>, flavor: T) -> Result<(), Box<dyn Error>> {
    if let Err(e) = result {
        let error = *e.downcast::<T>().expect("Expected provided error type");
        assert_eq!(flavor, error);
        Ok(())
    } else {
        panic!("Result did not result in an error");
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
    let add_func = PFunction::new(
        OptionalIdentifier::Ident(String::from("add")),
        vec![
            Parameter::named("a", CortexType::number()),
            Parameter::named("b", CortexType::number())
        ],
        CortexType::number(),
        add_body,
        vec![],
    );
    let generic_func = PFunction::new(
        OptionalIdentifier::Ident(String::from("generic")),
        vec![
            Parameter::named("t", CortexType::generic("T"))
        ],
        CortexType::OptionalType(Box::new(CortexType::generic("T"))),
        Body::Basic(BasicBody::new(vec![], Some(PExpression::None))),
        vec![TypeParam::ty("T")],
    );
    let test_struct = Struct::new("Time", vec![
        ("m", CortexType::number()),
        ("s", CortexType::number()),
    ], vec![], vec![], None);
    let test_struct2 = Struct::new("IntBox", vec![
        ("v", CortexType::number()),
    ], vec![], vec![], None);
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_function(add_func)?;
    module.add_function(generic_func)?;
    module.add_struct(test_struct)?;
    module.add_struct(test_struct2)?;
    let path = CortexParser::parse_path("simple")?;
    interpreter.register_module(&path, module)?;
    Ok(interpreter)
}
