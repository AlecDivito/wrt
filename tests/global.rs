// use wrt::Engine;

// #[test]
// fn global_compile_test() {
//     let program = r#"(module
//         (global $answer1 i32 42)
//         (global $answer2 i64 42)
//         (global $answer3 f32 42.0)
//         (global $answer4 f64 42.0)
//         (global $answer5 (mut i32) 42)
//         (global $answer6 (mut i64) 42)
//         (global $answer7 (mut f32) 42.0)
//         (global $answer8 (mut f64) 42.0)

//         ;; These two import statements are equal
//         (global $example1 (import "lib" "example") i32)
//         (import "lib" "example" (global $example2 i32))

//         ;; These two export statements are equal
//         (global $example3 (export "example1") i32 42)
//         (export "example2" (global $example3))
//     )"#;
//     let res = Engine::compile_and_run(program, "getAnswerPlus1", &[]).is_ok();
//     assert!(res)
// }
