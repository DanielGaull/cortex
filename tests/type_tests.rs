use std::error::Error;

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::{ast::{expression::{Expression, OptionalIdentifier, Parameter, PathIdent}, top_level::{BasicBody, Body, Bundle, Function}, r#type::CortexType}, parser::CortexParser}, preprocessing::module::Module};

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
    ))?;
    module.add_bundle(Bundle::new(
        "Box",
        vec![
            ("time", CortexType::reference(CortexType::basic(PathIdent::simple(String::from("Time")), false, vec![]), true))
        ],
        vec![
            Function::member_func(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::reference(CortexType::simple("Time", false), true),
                Body::Basic(BasicBody::new(vec![], Some(CortexParser::parse_expression("this.time")?))),
                cortex_lang::parsing::ast::top_level::ThisArg::MutThis,
                vec![],
            )
        ],
        vec![],
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
            Function::member_func(
                OptionalIdentifier::Ident(String::from("get")), 
                vec![],
                CortexType::basic(PathIdent::simple(String::from("T")), false, vec![]),
                Body::Basic(BasicBody::new(vec![], Some(CortexParser::parse_expression("this.item")?))),
                cortex_lang::parsing::ast::top_level::ThisArg::This,
                vec![],
            )
        ],
        vec!["T"],
    ))?;
    module.add_function(Function::new(
        OptionalIdentifier::Ident(String::from("generic")),
        vec![
            Parameter::named("t", CortexType::simple("T", true))
        ],
        CortexType::simple("T", true),
        Body::Basic(BasicBody::new(vec![], Some(Expression::None))),
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

#[test]
fn subtype_tests() -> Result<(), Box<dyn Error>> {
    assert!(CortexType::none().is_subtype_of(&CortexType::number(true)));
    assert!(CortexType::reference(CortexType::number(false), true).is_subtype_of(&CortexType::reference(CortexType::number(false), false)));
    assert!(!CortexType::reference(CortexType::number(false), false).is_subtype_of(&CortexType::reference(CortexType::number(false), true)));
    assert!(CortexType::basic_simple("list", false, vec![CortexType::simple("number", false)])
        .is_subtype_of(&CortexType::basic_simple("list", false, vec![CortexType::simple("number", true)]))
    );
    Ok(())
}
