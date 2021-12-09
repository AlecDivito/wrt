use wrt::Engine;

#[test]
fn compile_inline_import_function() {
    let text = r#"(module
        (func (import "lib" "answer") (result i32)))"#;
    assert!(Engine::compile(text).is_ok());
}

#[test]
fn compile_import_function() {
    let text = r#"(module
        (import "lib" "getAnswer" (func $answer (result i32)))
        (func (result i32)
            call $answer
            i32.const 1
            i32.add))"#;
    Engine::compile(text).unwrap();
    assert!(true);
}

// #[test]
// fn table() {}

// #[test]
// fn memory() {}

// #[test]
// fn global() {}
