use wrt::parse::parse;

// use wrt::{module::value::ValueType, Engine};
fn main() {
    // https://github.com/WebAssembly/testsuite/blob/main/const.wast
    let p1 = r#";; Test t.const instructions

    ;; Syntax error

    (; test ;)
    
    (module (func (i32.const 0_123_456_789) drop))
    (;(module (func (i32.const 0x0_9acf_fBDF) drop))
    (assert_malformed
        (module quote "(func (i32.const) drop)")
        "unexpected token"
    );)"#;
    let tokens = parse(p1);
    println!("{:?}", tokens);
    // let engine = Engine::new();
    // let m1 = engine.compile(p1).unwrap();
    // let m2 = engine.compile(p2).unwrap();
    // let mut instance = engine.instantiate(m2);
    // instance.link(m1, "lib");
    // let res = instance.execute("getAnswerPlus1", &[]).unwrap();
    // // assert_eq!(res, &[ValueType::I32(43)])
    // println!("{:?}", res)
}
