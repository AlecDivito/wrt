use std::{fs, iter::Peekable, os::unix::ffi::OsStrExt, path::PathBuf};

use clap::{Parser, Subcommand};
use wrt::parse::{
    ast::{parse_module_test, parse_module_test_2, print_tee, Expect},
    tokenize, Token,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,

    #[arg(name = "debug", short = 'd', default_value = "false")]
    debug: bool,
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

pub struct Options {
    directory: PathBuf,
    debug: bool,
}

impl Options {
    fn get_all_tests(&self) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
        if !self.directory.is_dir() {
            println!("{:?} is a file and not a directory", self.directory);
            return Ok(vec![]);
        }
        let mut list = vec![];
        for entry in fs::read_dir(&self.directory)? {
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

    pub fn list_tests(&self) -> Result<(), Box<dyn std::error::Error>> {
        let mut lists = self.get_all_tests()?;
        lists.sort();
        for item in lists {
            println!("{}", item.file_name().unwrap().to_string_lossy());
        }
        Ok(())
    }

    pub fn test_all(&self) -> Result<(), Box<dyn std::error::Error>> {
        let lists = self.get_all_tests()?;
        let mut pass = 0;
        let mut fail = 0;
        for item in &lists {
            let contents = std::fs::read_to_string(item)?;
            match tokenize(&contents) {
                Ok(_) => {
                    pass += 1;
                    println!("PASS {}", item.file_name().unwrap().to_string_lossy())
                }
                Err(err) => {
                    fail += 1;
                    if self.debug {
                        println!(
                            "FAIL {} - {} at {:?}",
                            item.file_name().unwrap().to_string_lossy(),
                            err.error,
                            err.location
                        )
                    } else {
                        println!("FAIL {}", item.file_name().unwrap().to_string_lossy())
                    }
                }
            };
        }
        println!("Passed {}/{}", pass, lists.len());
        println!("Failed {}/{}", fail, lists.len());
        Ok(())
    }

    pub fn test_single(&self, name: String) -> Result<(), Box<dyn std::error::Error>> {
        let file = if self.directory.join(&name).is_file() {
            self.directory.join(name)
        } else if self.directory.join(format!("{}.wast", &name)).is_file() {
            self.directory.join(format!("{}.wast", name))
        } else if PathBuf::from(&name).is_file() {
            PathBuf::from(name)
        } else {
            println!("Error: file {} does not exist", name);
            return Ok(());
        };

        let contents = fs::read_to_string(file)?;
        let tokens = match tokenize(&contents) {
            Ok(tokens) => {
                println!("Scanning file and converting to tokens");
                if self.debug {
                    println!("{:?}", tokens);
                }
                println!("Tokenization complete");
                tokens
            }
            Err(err) => {
                if self.debug {
                    println!("Scan Error: {:?}", err)
                } else {
                    println!("Scan Error: {:?} at location {:?}", err.error, err.location)
                }
                return Ok(());
            }
        };

        let mut iter = tokens.iter().peekable();
        // while iter.peek().is_some() {
        //     let tee = parse_module_test_2(&mut iter).unwrap();
        //     print_tee(&tee);
        //     println!("---")
        // }
        let mut last_parsed_module = None;
        while iter.peek().is_some() {
            match parse_module_test(&mut iter, last_parsed_module.as_ref()) {
                Ok(Some(module)) => {
                    last_parsed_module = Some(module);
                    println!("Module parsed successfully...");
                }
                Ok(None) => println!("Passed Test..."),
                Err(err) => {
                    let range = if let Some(token) = &err.token() {
                        if let Some(index) = tokens.iter().position(|t| t == *token) {
                            if index == 0 {
                                tokens.get(0..=5).unwrap().to_vec()
                            } else {
                                tokens.get(index - 2..=index + 2).unwrap().to_vec()
                            }
                        } else {
                            vec![]
                        }
                    } else {
                        vec![]
                    };

                    println!("ERROR");
                    println!("========================================");
                    println!("{:?}", range);
                    let tokens = range.iter().map(|t| t.source()).collect::<Vec<_>>();
                    println!("Tokens around error: {:?}", tokens);
                    println!("Parsing encounted error {:?}", err);
                    return Ok(());
                }
            }
        }

        Ok(())
    }
}

fn main() {
    let cli = Cli::parse();

    if let Err(err) = match cli.command {
        Commands::Test(test) => {
            let opts = Options {
                directory: test.directory.clone(),
                debug: cli.debug,
            };
            match test.tests {
                TestCommands::List => opts.list_tests(),
                TestCommands::All => opts.test_all(),
                TestCommands::File { name } => opts.test_single(name),
            }
        }
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
