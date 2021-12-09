use wrt::Engine;

#[test]
fn empty_program_is_incorrect() {
    let result = Engine::compile("");
    assert!(result.is_err());
}

#[test]
fn simpliest_wasm_program() {
    let result = Engine::compile("(module)");
    assert!(result.is_ok());
}

// #[test]
// fn wasm_function_add_with_export() {
//     let program = r#"(module
//     (func $add (param $lhs i32) (param $rhs i32) (result i32)
//         local.get $lhs
//         local.get $rhs
//         i32.add
//     )
//     (export "add" (func $add)))"#;
//     let res =
//         Engine::compile_and_run(program, "add", &[ValueType::I32(5), ValueType::I32(3)]).unwrap();
//     assert_eq!(res, &[ValueType::I32(8)])
// }

// #[test]
// fn multi_declared_parameters_and_results_in_func() {
//     let program = r#"(module
//     (func $test (param i32 i32) (result i32 i32)
//         local.get 1
//         local.get 0
//     ) (export "test" (func $test)))"#;
//     let args = &[ValueType::I32(5), ValueType::I32(3)];
//     let res = Engine::compile_and_run(program, "test", args).unwrap();
//     assert_eq!(res, args)
// }

// #[test]
// fn wasm_function_add_with_export_and_weird_spacing() {
//     let program = r#"(           module
//      (   func   $add     (
//          param   $lhs
//            i32    )

//            (  param    $rhs
//              i32) (result i32)
//         local.get $lhs
//         local.get $rhs
//         i32.add
//     )
//     (  export   "add"   (  func   $add  )  ))"#;
//     let res =
//         Engine::compile_and_run(program, "add", &[ValueType::I32(5), ValueType::I32(3)]).unwrap();
//     assert_eq!(res, &[ValueType::I32(8)])
// }

// #[test]
// fn wasm_function_sum_return_and_parameters_test() {
//     let program = r#"(module
//     (func (export "sum") (param $a i32) (param $b i32) (result i32)
//         local.get $a
//         local.get $b
//         i32.add
//         return
//     ))"#;
//     let res =
//         Engine::compile_and_run(program, "sum", &[ValueType::I32(5), ValueType::I32(6)]).unwrap();
//     assert_eq!(res, &[ValueType::I32(11)])
// }

// #[test]
// fn wasm_function_calls_other_function() {
//     let program = r#"(module (func $getAnswer (result i32) i32.const 42)
//     (func (export "getAnswerPlus1") (result i32)
//     call $getAnswer
//     i32.const 1
//     i32.add))"#;
//     let res = Engine::compile_and_run(program, "getAnswerPlus1", &[]).unwrap();
//     assert_eq!(res, &[ValueType::I32(43)])
// }

// #[test]
// fn global_import_export() {
//     let p2 = r#"(module (global $global (export "global") (mut i32)) 42)"#;
//     let p1 = r#"(module
//         (global $g (import "js" "global") (mut i32))
//         (func (export "getGlobal") (result i32)
//              (global.get $g))
//         (func (export "incGlobal")
//             global.set $g
//             global.get $g
//             i32.const 1
//             i32.add)
//     )"#;
//     let engine = Engine::new();
//     let m1 = engine.compile(p1).unwrap();
//     let m2 = engine.compile(p2).unwrap();
//     let mut instance = engine.instantiate(m2);
//     instance.link(m1, "lib");
//     let res = instance.execute("getAnswerPlus1", &[]).unwrap();
//     assert_eq!(res, &[ValueType::I32(43)])
// }

mod export;
mod global;
mod import;
mod table;
mod types;
