use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::{ast::{expression::{OptionalIdentifier, PExpression, Parameter, PathIdent}, top_level::{BasicBody, Body, Contract, MemberFunction, PFunction, Struct}}, parser::CortexParser}, preprocessing::{module::Module, preprocessor::preprocessor::CortexPreprocessor}, r#type::r#type::{CortexType, FollowsClause, FollowsEntry, TypeArg, TypeParam, TypeParamType}};

fn run_test(input: &str, type_str: &str, interpreter: &mut CortexInterpreter) -> Result<(), Box<dyn Error>> {
    let ast = CortexParser::parse_expression(input)?;
    let eval_typ = interpreter.determine_type(ast)?;
    let typ = CortexParser::parse_type(type_str)?;
    assert_eq!(typ, eval_typ);
    Ok(())
}

#[test]
fn run_simple_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    run_test("5", "number", &mut interpreter)?;
    run_test("5.3", "number", &mut interpreter)?;
    run_test("\"hello\"", "string", &mut interpreter)?;
    run_test("true", "bool", &mut interpreter)?;
    run_test("false", "bool", &mut interpreter)?;
    run_test("void", "void", &mut interpreter)?;
    run_test("(((void)))", "void", &mut interpreter)?;
    run_test("none", "none", &mut interpreter)?;
    run_test("[1]", "&mut list<number>", &mut interpreter)?;
    run_test("[1, none]", "&mut list<number?>", &mut interpreter)?;
    run_test("(1,)", "(number,)", &mut interpreter)?;
    run_test("(1, 2)", "(number, number)", &mut interpreter)?;
    run_test("(1, true, \"hello\")", "(number, bool, string)", &mut interpreter)?;
    run_test("(1, true, \"hello\").t1", "bool", &mut interpreter)?;
    run_test("[(none, 5), (true, none)]", "&mut list<(bool?, number?)>", &mut interpreter)?;
    run_test("heap 5", "&mut number", &mut interpreter)?;
    Ok(())
}

#[test]
fn subtype_tests() -> Result<(), Box<dyn Error>> {
    let mut preprocessor = CortexPreprocessor::new()?;
    let mut module = Module::new();
    module.add_contract(Contract::new("Iterable", vec![], vec![]))?;
    module.add_contract(Contract::new("X", vec![], vec![]))?;
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
            FollowsEntry::new(PathIdent::new(vec!["Container"]), vec![TypeArg::Ty(CortexType::generic("T"))]),
        ]))
    ))?;

    preprocessor.register_module(&PathIdent::empty(), module)?;

    assert_subtype("none", "number?", &preprocessor)?;
    assert_subtype("&mut number", "&number", &preprocessor)?;
    assert_not_subtype("&number", "&mut number", &preprocessor)?;
    assert_subtype("(&mut number, none)", "(&number, bool?)", &preprocessor)?;
    assert_not_subtype("(number, number)", "(number, number, number)", &preprocessor)?;

    assert_subtype("follows X + Y + Z", "follows X", &preprocessor)?;
    assert_subtype("&TestType", "follows Iterable", &preprocessor)?;
    assert_not_subtype("&TestType", "follows Iterable + X", &preprocessor)?;
    assert_subtype("&OtherTestType", "follows Iterable", &preprocessor)?;
    assert_subtype("TestType", "follows Iterable", &preprocessor)?;
    assert_not_subtype("TestType", "follows Iterable + X", &preprocessor)?;
    assert_subtype("OtherTestType", "follows Iterable", &preprocessor)?;
    assert_subtype("&mut Box<number>", "follows Container<number>", &preprocessor)?;

    assert_not_subtype("Box<TestType>", "Box<follows Iterable>", &preprocessor)?;
    Ok(())
}

#[test]
fn run_reference_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_struct(Struct::new(
        "Time", 
        vec![
            ("m", CortexType::number()),
            ("s", CortexType::number()),
        ],
        vec![],
        vec![],
        None,
    ))?;
    module.add_struct(Struct::new(
        "Box",
        vec![
            ("time", CortexType::reference(CortexType::basic(PathIdent::simple(String::from("Time")), vec![]), true))
        ],
        vec![
            MemberFunction::new(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::reference(CortexType::simple("Time"), true),
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
            ("item", CortexType::generic("T"))
        ],
        vec![
            MemberFunction::new(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::generic("T"),
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
            Parameter::named("t", CortexType::OptionalType(Box::new(CortexType::generic("T"))))
        ],
        CortexType::OptionalType(Box::new(CortexType::generic("T"))),
        Body::Basic(BasicBody::new(vec![], Some(PExpression::None))),
        vec![TypeParam::ty("T")],
    ))?;
    interpreter.register_module(&PathIdent::simple(String::from("box")), module)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box = heap box::Box<number>{ item: 5 };")?)?;
    run_test("box.item", "number", &mut interpreter)?;
    run_test("box.get()", "number", &mut interpreter)?;

    interpreter.execute_statement(CortexParser::parse_statement("let box2: &box::Box<&mut box::Box<number>> = heap box::Box<&mut box::Box<number>>{ item: heap box::Box<number>{ item: 5 } };")?)?;
    run_test("box2.item", "&box::Box<number>", &mut interpreter)?;

    run_test("box::generic(5)", "number?", &mut interpreter)?;
    run_test("box::generic<number>(none)", "number?", &mut interpreter)?;

    Ok(())
}

#[test]
fn run_inference_type_tests() -> Result<(), Box<dyn Error>> {
    // Tests where absence of an error is all that is expected
    let mut interpreter = CortexInterpreter::new()?;
    interpreter.execute_statement(CortexParser::parse_statement("let l1: &list<number> = [];")?)?;
    interpreter.execute_statement(CortexParser::parse_statement("let l2: (&list<number>, &list<string>) = ([], []);")?)?;

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

fn assert_subtype(first: &str, second: &str, preprocessor: &CortexPreprocessor) -> Result<(), Box<dyn Error>> {
    let t1 = CortexParser::parse_type(first)?;
    let t2 = CortexParser::parse_type(second)?;
    assert!(preprocessor.is_subtype(&t1, &t2)?, "'{}' is not a subtype of '{}'", first, second);
    Ok(())
}
fn assert_not_subtype(first: &str, second: &str, preprocessor: &CortexPreprocessor) -> Result<(), Box<dyn Error>> {
    let t1 = CortexParser::parse_type(first)?;
    let t2 = CortexParser::parse_type(second)?;
    assert!(!preprocessor.is_subtype(&t1, &t2)?, "'{}' is mistakenly a subtype of '{}'", first, second);
    Ok(())
}
