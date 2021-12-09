use wrt::Engine;

#[test]
fn compile_inline_export_function() {
    let text = r#"(module (func (export "getAnswer") (result i32) i32.const 42))"#;
    assert!(Engine::compile(text).is_ok());
}

#[test]
fn compile_export_function() {
    let text = r#"(module
        (func (result i32) i32.const 42)
        (func $id (result i32) i32.const 42)
        (export "getAnswer" (func 0))
        (export "getAnswer2" (func $id))
    )"#;
    assert!(Engine::compile(text).is_ok());
}
