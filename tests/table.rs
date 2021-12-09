use wrt::Engine;

#[test]
fn table() {
    let text = r#"(module
        (table $id1 0 1 funcref)
        (table $id2 0 1 externref)
        (table $id3 0 externref)
        (table 0 externref)
        (table (import \"lib\" \"test\") 0 1 funcref)
        (table (export \"test\") 0 1 funcref)
        (table (export \"test\") (export \"test2\") 0 1 funcref)
    )"#;
    assert!(Engine::compile(text).is_ok());
}
