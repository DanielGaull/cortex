use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::{ast::{expression::{OptionalIdentifier, PExpression, Parameter, PathIdent}, top_level::{BasicBody, Body, Contract, MemberFunction, PFunction, Struct}}, codegen::r#trait::SimpleCodeGen, parser::CortexParser}, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::{FollowsClause, FollowsEntry, PType, TypeArg, TypeParam, TypeParamType}};

fn run_test(input: &str, type_str: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let eval_typ = interpreter.determine_type(ast)?;
    let typ = interpreter.validate_type(CortexParser::parse_type(type_str)?)?;
    assert_eq!(typ, eval_typ);
    Ok(())
}

#[test]
fn run_simple_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run_test("5", "i32", &mut interpreter)?;
    run_test("5.3", "f64", &mut interpreter)?;
    run_test("\"hello\"", "string", &mut interpreter)?;
    run_test("true", "bool", &mut interpreter)?;
    run_test("false", "bool", &mut interpreter)?;
    run_test("void", "void", &mut interpreter)?;
    run_test("(((void)))", "void", &mut interpreter)?;
    run_test("none", "none", &mut interpreter)?;
    run_test("[1]", "span<i32>", &mut interpreter)?;
    run_test("[1, none]", "span<i32?>", &mut interpreter)?;
    run_test("(1,)", "(i32,)", &mut interpreter)?;
    run_test("(1, 2)", "(i32, i32)", &mut interpreter)?;
    run_test("(1, true, \"hello\")", "(i32, bool, string)", &mut interpreter)?;
    run_test("(1, true, \"hello\").t1", "bool", &mut interpreter)?;
    run_test("[(none, 5), (true, none)]", "span<(bool?, i32?)>", &mut interpreter)?;
    run_test("heap 5", "&mut i32", &mut interpreter)?;
    Ok(())
}

#[test]
fn subtype_tests() -> Result<(), Box<dyn Error>> {
    let mut preprocessor = CortexPreprocessor::new()?;
    let mut module = Module::new();
    module.add_contract(Contract::new("Iterable", vec![], vec![]))?;
    module.add_contract(Contract::new("X", vec![], vec![]))?;
    module.add_contract(Contract::new("Y", vec![], vec![]))?;
    module.add_contract(Contract::new("Z", vec![], vec![]))?;
    module.add_contract(Contract::new("Container", vec![TypeParam::new("T", TypeParamType::Ty)], vec![]))?;

    module.add_struct(Struct::new(
        "TestType", 
        vec![], 
        vec![], 
        vec![], 
        Some(FollowsClause::new(vec![
            FollowsEntry::new(PathIdent::new(vec!["Iterable"]), vec![]),
        ]))
    ))?;
    module.add_struct(Struct::new(
        "OtherTestType", 
        vec![], 
        vec![], 
        vec![], 
        Some(FollowsClause::new(vec![
            FollowsEntry::new(PathIdent::new(vec!["Iterable"]), vec![]),
            FollowsEntry::new(PathIdent::new(vec!["X"]), vec![]),
        ]))
    ))?;
    module.add_struct(Struct::new(
        "Box",
        vec![],
        vec![],
        vec!["T"],
        Some(FollowsClause::new(vec![
            FollowsEntry::new(PathIdent::new(vec!["Container"]), vec![TypeArg::Ty(PType::generic("T"))]),
        ]))
    ))?;

    preprocessor.register_module(&PathIdent::empty(), module)?;

    assert_subtype("none", "i32?", &preprocessor, Some("i32?"))?;
    assert_subtype("&mut i32", "&i32", &preprocessor, Some("&i32"))?;
    assert_not_subtype("&i32", "&mut i32", &preprocessor, Some("&i32"))?;
    assert_subtype("(&mut i32, none)", "(&i32, bool?)", &preprocessor, Some("(&i32, bool?)"))?;
    assert_not_subtype("(i32, i32)", "(i32, i32, i32)", &preprocessor, None)?;

    assert_subtype("follows X + Y + Z", "follows X", &preprocessor, Some("follows X"))?;
    assert_subtype("&TestType", "follows Iterable", &preprocessor, Some("follows Iterable"))?;
    assert_not_subtype("&TestType", "follows Iterable + X", &preprocessor, Some("follows Iterable"))?;
    assert_subtype("&OtherTestType", "follows Iterable", &preprocessor, Some("follows Iterable"))?;
    assert_subtype("TestType", "follows Iterable", &preprocessor, Some("follows Iterable"))?;
    assert_not_subtype("TestType", "follows Iterable + X", &preprocessor, Some("follows Iterable"))?;
    assert_subtype("OtherTestType", "follows Iterable", &preprocessor, Some("follows Iterable"))?;

    // TODO: this is currently bugged
    // assert_subtype("&mut Box<i32>", "follows Container<i32>", &preprocessor, Some("follows Container<i32>"))?;

    assert_not_subtype("Box<TestType>", "Box<follows Iterable>", &preprocessor, Some("Box<follows Iterable>"))?;

    assert_subtype("() => void", "() => void", &preprocessor, Some("() => void"))?;
    assert_subtype("() => &TestType", "() => follows Iterable", &preprocessor, Some("() => follows Iterable"))?;
    assert_not_subtype("() => follows Iterable", "() => &TestType", &preprocessor, Some("() => follows Iterable"))?;
    assert_subtype("(follows Iterable) => void", "(&TestType) => void", &preprocessor, Some("(&TestType) => void"))?;
    assert_not_subtype("(&TestType) => void", "(follows Iterable) => void", &preprocessor, Some("(&TestType) => void"))?;
    assert_subtype("<T: ty>(T) => void", "<R: ty>(R) => void", &preprocessor, Some("<R: ty>(R) => void"))?;

    Ok(())
}

#[test]
fn run_reference_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_struct(Struct::new(
        "Time", 
        vec![
            ("m", PType::i32()),
            ("s", PType::i32()),
        ],
        vec![],
        vec![],
        None,
    ))?;
    module.add_struct(Struct::new(
        "Box",
        vec![
            ("time", PType::reference(PType::basic(PathIdent::simple(String::from("Time")), vec![]), true))
        ],
        vec![
            MemberFunction::new(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                PType::reference(PType::simple("Time"), true),
                Body::Basic(BasicBody::new(vec![], Some(CortexParser::parse_expression("this.time")?))),
                cortex_lang::parsing::ast::top_level::ThisArg::RefMutThis,
                vec![],
            )
        ],
        vec![],
        None,
    ))?;
    interpreter.register_module(&PathIdent::simple(String::from("Time")), module)?;
    run_test("heap Time::Time{m:5,s:5}", "&mut Time::Time", &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box = heap Time::Box{ time: heap Time::Time{m:4,s:5} };")?)?;
    run_test("box", "&mut Time::Box", &mut interpreter)?;
    run_test("box.get()", "&mut Time::Time", &mut interpreter)?;
    Ok(())
}

#[test]
fn run_generic_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_struct(Struct::new(
        "Box",
        vec![
            ("item", PType::generic("T"))
        ],
        vec![
            MemberFunction::new(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                PType::generic("T"),
                Body::Basic(BasicBody::new(vec![], Some(CortexParser::parse_expression("this.item")?))),
                cortex_lang::parsing::ast::top_level::ThisArg::RefThis,
                vec![],
            )
        ],
        vec!["T"],
        None,
    ))?;
    module.add_function(PFunction::new(
        OptionalIdentifier::Ident(String::from("generic")),
        vec![
            Parameter::named("t", PType::OptionalType(Box::new(PType::generic("T"))))
        ],
        PType::OptionalType(Box::new(PType::generic("T"))),
        Body::Basic(BasicBody::new(vec![], Some(PExpression::None))),
        vec![TypeParam::ty("T")],
    ))?;
    interpreter.register_module(&PathIdent::simple(String::from("box")), module)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box = heap box::Box<i32>{ item: 5 };")?)?;
    run_test("box.item", "i32", &mut interpreter)?;
    run_test("box.get()", "i32", &mut interpreter)?;

    interpreter.execute_statement(CortexParser::parse_statement("let box2: &box::Box<&mut box::Box<i32>> = heap box::Box<&mut box::Box<i32>>{ item: heap box::Box<i32>{ item: 5 } };")?)?;
    run_test("box2.item", "&box::Box<i32>", &mut interpreter)?;

    run_test("box::generic(5)", "i32?", &mut interpreter)?;
    run_test("box::generic<i32>(none)", "i32?", &mut interpreter)?;

    Ok(())
}

#[test]
fn run_inference_type_tests() -> Result<(), Box<dyn Error>> {
    // Tests where absence of an error is all that is expected
    let mut interpreter = CortexInterpreter::new()?;
    interpreter.execute_statement(CortexParser::parse_statement("let l1: span<i32> = [];")?)?;
    interpreter.execute_statement(CortexParser::parse_statement("let l2: (span<i32>, span<string>) = ([], []);")?)?;

    Ok(())
}

// #[test]
// fn run_var_type_tests() -> Result<(), Box<dyn Error>> {
//     let mut interpreter = CortexInterpreter::new();
//     let mut module = Environment::base();
//     mod_env.add_const(String::from("myBoolean"), CortexType::boolean(false), CortexValue::Boolean(true))?;
//     mod_env.add_const(String::from("optionalBoolean"), CortexType::boolean(true), CortexValue::Boolean(true))?;
//     let path = CortexParser::parse_path("simple")?;
//     interpreter.register_module(&path, module)?;

//     run_test("simple::myBoolean", "bool", &interpreter)?;
//     run_test("simple::optionalBoolean", "bool?", &interpreter)?;

//     Ok(())
// }

fn assert_subtype(first: &str, second: &str, preprocessor: &CortexPreprocessor, combined_str: Option<&str>) -> Result<(), Box<dyn Error>> {
    let t1 = preprocessor.validate_type(CortexParser::parse_type(first)?)?;
    let t2 = preprocessor.validate_type(CortexParser::parse_type(second)?)?;
    assert!(preprocessor.is_subtype(&t1, &t2)?, "'{}' is not a subtype of '{}'", first, second);

    if let Some(combined_str) = combined_str {
        let combined = preprocessor.validate_type(CortexParser::parse_type(combined_str)?)?;
        let combined_type = preprocessor.combine_types(t1, t2)?.expect(&format!("'{}' cannot be combined with '{}'", first, second));
        assert_eq!(
            combined,
            combined_type,
            "'{}' combined with '{}' => '{}', which isn't equal to '{}'", first, second, combined_type.codegen(0), combined_str);
    } else {
        assert!(preprocessor.combine_types(t1, t2)?.is_none(), "'{}' can be combined with '{}'", first, second);
    }

    Ok(())
}
fn assert_not_subtype(first: &str, second: &str, preprocessor: &CortexPreprocessor, combined_str: Option<&str>) -> Result<(), Box<dyn Error>> {
    let t1 = preprocessor.validate_type(CortexParser::parse_type(first)?)?;
    let t2 = preprocessor.validate_type(CortexParser::parse_type(second)?)?;
    assert!(!preprocessor.is_subtype(&t1, &t2)?, "'{}' is mistakenly a subtype of '{}'", first, second);
    
    if let Some(combined_str) = combined_str {
        let combined = preprocessor.validate_type(CortexParser::parse_type(combined_str)?)?;
        let combined_type = preprocessor.combine_types(t1, t2)?.expect(&format!("'{}' cannot be combined with '{}'", first, second));
        assert_eq!(
            combined,
            combined_type,
            "'{}' combined with '{}' => '{}', which isn't equal to '{}'", first, second, combined_type.codegen(0), combined_str);
    } else {
        assert!(preprocessor.combine_types(t1, t2)?.is_none(), "'{}' can be combined with '{}'", first, second);
    }
    
    Ok(())
}
