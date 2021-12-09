use wrt::Engine;

#[test]
fn compile_type() {
    let text = r#"(module
        (type (func (param i32) (result i32))))"#;
    assert!(Engine::compile(text).is_ok());
}

#[test]
fn compile_function_with_type() {
    let text = r#"(module
        (type (func (param i32) (result i32)))
        (func (type 0) i32.const 0)
    )"#;
    Engine::compile(text).unwrap();
    assert!(true);
}
