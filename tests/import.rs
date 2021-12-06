use wrt::{types::value::ValueType, Engine};

#[test]
fn function() {
    let p1 = r#"(module (func (export "getAnswer") (result i32) i32.const 42))"#;
    let p2 = r#"(module
        (import "lib" "getAnswer" (func $answer (result i32)))
        (func (export "getAnswerPlus1") (result i32)
            call $answer
            i32.const 1
            i32.add))"#;
    let engine = Engine::new();
    let m1 = engine.compile(p1).unwrap();
    let m2 = engine.compile(p2).unwrap();
    let mut instance = engine.instantiate(m2);
    instance.link(m1, "lib");
    let res = instance.execute("getAnswerPlus1", &[]).unwrap();
    assert_eq!(res, &[ValueType::I32(43)])
}

// #[test]
// fn table() {}

// #[test]
// fn memory() {}

// #[test]
// fn global() {}
