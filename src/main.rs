use wrt::{types::value::ValueType, Engine};
fn main() {
    let program = r#"(module
        (func $getAnswer (result i32) i32.const 42)
        (func (export "getAnswerPlus1") (result i32)
            call $getAnswer
            i32.const 1
            i32.add))"#;
    let res = Engine::compile_and_run(program, "getAnswerPlus1", &[]).unwrap();

    println!("{:?}", res)
}
