use std::{
    env::args,
    fs,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
};

use clap::{Args, Command, Parser, Subcommand};
use wrt::parse::parse;

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    Test(Tests),
}

#[derive(Parser)]
pub struct Tests {
    #[arg(name = "dir", default_value = "testsuite")]
    directory: PathBuf,

    #[command(subcommand)]
    tests: TestCommands,
}

#[derive(Subcommand)]
pub enum TestCommands {
    List,
    All,
    File { name: String },
}

fn get_all_tests(path: &PathBuf) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    if !path.is_dir() {
        println!("{:?} is a file and not a directory", path);
        return Ok(vec![]);
    }
    let mut list = vec![];
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext.as_bytes() == b"wast" {
                    list.push(path);
                }
            }
        }
    }
    Ok(list)
}

fn list_tests(path: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let mut lists = get_all_tests(&path)?;
    lists.sort();
    for item in lists {
        println!("{}", item.file_name().unwrap().to_string_lossy());
    }
    Ok(())
}

pub fn test_all(directory: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let lists = get_all_tests(&directory)?;
    for item in lists {
        let contents = std::fs::read_to_string(&item)?;
        match parse(&contents) {
            Ok(_) => {
                println!("PASS {}", item.file_name().unwrap().to_string_lossy())
            }
            Err(_) => println!("FAIL {}", item.file_name().unwrap().to_string_lossy()),
        };
    }
    Ok(())
}

fn main() {
    let cli = Cli::parse();

    if let Err(err) = match cli.command {
        Commands::Test(test) => match test.tests {
            TestCommands::List => list_tests(test.directory),
            TestCommands::All => test_all(test.directory),
            TestCommands::File { name } => todo!("todo"),
        },
    } {
        println!("{:?}", err);
        println!("Program failed to complete successfully")
    }

    // let file = fs::read_to_string(file_path).unwrap();
    // let substring = &file[797..];

    // let tokens = parse(&substring);
    // println!("{:?}", tokens);
    // let engine = Engine::new();
    // let m1 = engine.compile(p1).unwrap();
    // let m2 = engine.compile(p2).unwrap();
    // let mut instance = engine.instantiate(m2);
    // instance.link(m1, "lib");
    // let res = instance.execute("getAnswerPlus1", &[]).unwrap();
    // // assert_eq!(res, &[ValueType::I32(43)])
    // println!("{:?}", res)
}
