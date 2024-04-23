use std::{env::args, fs};

use wrt::parse::parse;

fn main() {
    let mut args = args();
    let file_path = args
        .nth(1)
        .unwrap_or("./testsuite/comments.wast".to_string());

    println!("Reading file {}", file_path);

    let file = fs::read_to_string(file_path).unwrap();
    let substring = &file[797..];

    let tokens = parse(&substring);
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
