use wrt::{types::value::ValueType, Engine};
fn main() {
    let program = r#"(module
        (func $add (param $lhs i32) (param $rhs i32) (result i32)
            local.get $lhs
            local.get $rhs
            i32.add
        )
        (export "add" (func $add))
    )"#;
    let engine = Engine::new();
    let module = engine.compile(program).unwrap();
    let instance = engine.instantiate(module);
    println!(
        "{:?}",
        instance
            .execute("add", &[ValueType::I32(5), ValueType::I32(3)])
            .unwrap()
    )
}
