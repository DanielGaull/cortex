use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::{ast::{expression::{PExpression, OptionalIdentifier, Parameter, PathIdent}, top_level::{BasicBody, Body, Bundle, PFunction, MemberFunction}, r#type::CortexType}, parser::CortexParser}, preprocessing::module::Module};

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
    run_test("none", "none?", &mut interpreter)?;
    run_test("[1]", "&mut list<number>", &mut interpreter)?;
    run_test("[1, none]", "&mut list<number?>", &mut interpreter)?;
    run_test("(1,)", "(number,)", &mut interpreter)?;
    run_test("(1, 2)", "(number, number)", &mut interpreter)?;
    run_test("(1, true, \"hello\")", "(number, bool, string)", &mut interpreter)?;
    run_test("(1, true, \"hello\").t1", "bool", &mut interpreter)?;
    run_test("[(none, 5), (true, none)]", "&mut list<(bool?, number?)>", &mut interpreter)?;
    Ok(())
}

#[test]
fn subtype_tests() -> Result<(), Box<dyn Error>> {
    assert_subtype("none?", "number?")?;
    assert_subtype("&mut number", "&number")?;
    assert_not_subtype("&number", "&mut number")?;
    assert_subtype("list<number>", "list<number?>")?;
    assert_subtype("(&mut number, none?)", "(&number, bool?)")?;
    assert_not_subtype("(number, number)", "(number, number, number)")?;
    Ok(())
}

#[test]
fn run_reference_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_bundle(Bundle::new(
        "Time", 
        vec![
            ("m", CortexType::number(false)),
            ("s", CortexType::number(false)),
        ],
        vec![],
        vec![],
        None,
    ))?;
    module.add_bundle(Bundle::new(
        "Box",
        vec![
            ("time", CortexType::reference(CortexType::basic(PathIdent::simple(String::from("Time")), false, vec![]), true))
        ],
        vec![
            MemberFunction::new(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::reference(CortexType::simple("Time", false), true),
                Body::Basic(BasicBody::new(vec![], Some(CortexParser::parse_expression("this.time")?))),
                cortex_lang::parsing::ast::top_level::ThisArg::RefMutThis,
                vec![],
            )
        ],
        vec![],
        None,
    ))?;
    interpreter.register_module(&PathIdent::simple(String::from("Time")), module)?;
    run_test("Time::Time{m:5,s:5}", "&mut Time::Time", &mut interpreter)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box = Time::Box{ time: Time::Time{m:4,s:5} };")?)?;
    run_test("box", "&mut Time::Box", &mut interpreter)?;
    run_test("box.get()", "&mut Time::Time", &mut interpreter)?;
    Ok(())
}

#[test]
fn run_generic_type_tests() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let mut module = Module::new();
    module.add_bundle(Bundle::new(
        "Box",
        vec![
            ("item", CortexType::basic(PathIdent::simple(String::from("T")), false, vec![]))
        ],
        vec![
            MemberFunction::new(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::basic(PathIdent::simple(String::from("T")), false, vec![]),
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
            Parameter::named("t", CortexType::simple("T", true))
        ],
        CortexType::simple("T", true),
        Body::Basic(BasicBody::new(vec![], Some(PExpression::None))),
        vec![String::from("T")],
    ))?;
    interpreter.register_module(&PathIdent::simple(String::from("box")), module)?;
    interpreter.execute_statement(CortexParser::parse_statement("let box = box::Box<number>{ item: 5 };")?)?;
    run_test("box.item", "number", &mut interpreter)?;
    run_test("box.get()", "number", &mut interpreter)?;

    interpreter.execute_statement(CortexParser::parse_statement("let box2: &box::Box<&mut box::Box<number>> = box::Box<&mut box::Box<number>>{ item: box::Box<number>{ item: 5 } };")?)?;
    run_test("box2.item", "&box::Box<number>", &mut interpreter)?;

    run_test("box::generic(5)", "number?", &mut interpreter)?;
    run_test("box::generic<number>(none)", "number?", &mut interpreter)?;

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

fn assert_subtype(first: &str, second: &str) -> Result<(), Box<dyn Error>> {
    let t1 = CortexParser::parse_type(first)?;
    let t2 = CortexParser::parse_type(second)?;
    assert!(t1.is_subtype_of(&t2));
    Ok(())
}
fn assert_not_subtype(first: &str, second: &str) -> Result<(), Box<dyn Error>> {
    let t1 = CortexParser::parse_type(first)?;
    let t2 = CortexParser::parse_type(second)?;
    assert!(!t1.is_subtype_of(&t2));
    Ok(())
}
