// use wrt::Engine;
fn main() {
    // let p2 = r#"(module (global $global (export "global") (mut i32) 42))"#;
    // let p1 = r#"(module
    //     (global $g (import "js" "global") (mut i32))
    //     (func (export "getGlobal") (result i32)
    //          (global.get $g))
    //     (func (export "incGlobal")
    //         global.set $g
    //         global.get $g
    //         i32.const 1
    //         i32.add)
    // )"#;
    // let engine = Engine::new();
    // let m1 = engine.compile(p1).unwrap();
    // let m2 = engine.compile(p2).unwrap();
    // let mut instance = engine.instantiate(m2);
    // instance.link(m1, "lib");
    // let res = instance.execute("getAnswerPlus1", &[]).unwrap();
    // assert_eq!(res, &[ValueType::I32(43)])
    // println!("{:?}", res)
}
