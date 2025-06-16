use std::{error::Error, fs::File, io::Read, path::Path};

use cortex_lang::{interpreting::interpreter::CortexInterpreter, parsing::parser::CortexParser};

#[test]
fn test_contracts() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let path = Path::new("./tests/res/contracts.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    run("let myList = [1, 2, 3];", &mut interpreter)?;
    run("let wrapper = heap ListWrapper<number> { items: myList, index: 0, };", &mut interpreter)?;
    run("let addFn = heap AddMapFn { value: 1, };", &mut interpreter)?;
    assert("toString(mapper(wrapper, addFn))", "\"&([2, 3, 4])\"", &mut interpreter)?;

    run("let myBoxNumber = heap BoxNumber { value: 5 };", &mut interpreter)?;
    assert("myBoxNumber.add(5)", "10", &mut interpreter)?;
    assert("myBoxNumber.map(5)", "0", &mut interpreter)?;

    run("let addFn2: follows MapFn<number, number> = heap AddMapFn {value: 1};", &mut interpreter)?;
    assert("addFn2.map(1)", "2", &mut interpreter)?;
    run("let box = heap Box<follows MapFn<number, number>> {item: addFn2};", &mut interpreter)?;
    assert("box.get().map(1)", "2", &mut interpreter)?;
    run("let addFn3: follows MapFn<number, number> = heap AddMapFn {value: 3};", &mut interpreter)?;
    run("box.item = addFn3;", &mut interpreter)?;
    assert("box.get().map(1)", "4", &mut interpreter)?;
    run("box.set(addFn2);", &mut interpreter)?;
    assert("box.get().map(1)", "2", &mut interpreter)?;

    run("wrapper = heap ListWrapper<number> { items: [1, 2, 3], index: 0, };", &mut interpreter)?;
    run("let iterTuple: (follows Iterator<number>,) = (wrapper,);", &mut interpreter)?;
    assert("iterTuple.t0.next()", "1", &mut interpreter)?;

    // TODO: uncomment in the future once all issues with this test are fixed
    // run("wrapper = heap ListWrapper<number> { items: [1, 2, 3], index: 0, };", &mut interpreter)?;
    // run("let iterList: &mut list<follows Iterator<number>> = [wrapper];", &mut interpreter)?;
    // assert("iterList[0].next()", "1", &mut interpreter)?;

    run("let myOtherListWrapper = heap OtherListWrapper<number>{ items: [1, 2, 3] };", &mut interpreter)?;
    run("let result = myOtherListWrapper.map(addFn2);", &mut interpreter)?;
    assert("toString(result)", "\"&([2, 3, 4])\"", &mut interpreter)?;

    run("wrapper = heap ListWrapper<number> { items: [1, 2, 3], index: 0, };", &mut interpreter)?;
    run("let myIteratorMapper = heap IteratorMapper<number, number>{ iter: wrapper, mapper: addFn2 };", &mut interpreter)?;
    run("result = myIteratorMapper.map();", &mut interpreter)?;
    assert("toString(result)", "\"&([2, 3, 4])\"", &mut interpreter)?;

    run("let shape = heap Square {len: 2};", &mut interpreter)?;
    run("let sizer = heap ConcreteShapeSizer{};", &mut interpreter)?;
    assert("sizer.size(shape)", "4", &mut interpreter)?;

    assert("5.add(5)", "10", &mut interpreter)?;
    assert("doAdd(5, 5)", "10", &mut interpreter)?;
    
    Ok(())
}

#[test]
fn test_contracts2() -> Result<(), Box<dyn Error>> {
    let mut interpreter = CortexInterpreter::new()?;
    let path = Path::new("./tests/res/contracts2.txt");
    let mut file = File::open(path).unwrap();
    let mut content = String::new();
    let _ = file.read_to_string(&mut content);
    content = content.replace("\r\n", "\n");
    let program = CortexParser::parse_program(&content)?;
    for tl in program.into_iter() {
        interpreter.run_top_level(tl)?;
    }

    run("let t: follows Transformer = heap IdentityTransformer {};", &mut interpreter)?;
    assert("t.transform(5)", "5", &mut interpreter)?;
    assert("t.transform<number?>(none)", "none", &mut interpreter)?;

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
