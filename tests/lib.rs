use wrt::{types::value::ValueType, Engine};

#[test]
fn empty_program_is_incorrect() {
    let program = "";
    let engine = Engine::new();
    let result = engine.compile(program);
    assert!(result.is_err());
}

#[test]
fn simpliest_wasm_program() {
    let program = "(module)";
    let engine = Engine::new();
    let result = engine.compile(program);
    assert!(result.is_ok());
}

#[test]
fn wasm_function_add_with_export() {
    let program = r#"(module
    (func $add (param $lhs i32) (param $rhs i32) (result i32)
        local.get $lhs
        local.get $rhs
        i32.add
    )
    (export "add" (func $add))
)"#;
    let res =
        Engine::compile_and_run(program, "add", &[ValueType::I32(5), ValueType::I32(3)]).unwrap();
    assert_eq!(res, &[ValueType::I32(8)])
}

#[test]
fn wasm_function_add_with_export_and_weird_spacing() {
    let program = r#"(           module
     (   func   $add     (  
         param   $lhs  
           i32    )   
           
           (  param    $rhs
             i32) (result i32)
        local.get $lhs
        local.get $rhs
        i32.add
    )
    (  export   "add"   (  func   $add  )  ))"#;
    let res =
        Engine::compile_and_run(program, "add", &[ValueType::I32(5), ValueType::I32(3)]).unwrap();
    assert_eq!(res, &[ValueType::I32(8)])
}

// #[test]
// fn wasm_function_return_test() {
//     let p = r#"(module
//     (func (export "main")
//           (result i32)
//         i32.const 42
//         return
//     )
// )"#;
//     let engine = Engine::new();
//     let module = engine.compile(p).unwrap();
//     let results = engine.execute(module, "main");
//     assert_eq!(vec![ValueType::I32(42)], results);
// }

// #[test]
// fn wasm_function_sum_and_parameters_test() {
//     let p = r#"(module
//     (func (export "sum")
//           (param $a i32)
//           (param $b i32)
//           (result i32)
//         local.get $a
//         local.get $b
//         i32.add
//         return
//     )
// )"#;
//     let engine = Engine::new();
//     let result = engine.compile(p).unwrap();
//     // let results = engine.execute(engine, "main", [ValueType::I32(10), ValueType::I32(10)]);
//     // assert_eq!(vec![ValueType::I32(20)], results);
// }
