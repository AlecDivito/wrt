use std::{fs, os::unix::ffi::OsStrExt, path::PathBuf};

use clap::{Parser, Subcommand};
use wrt::{
    parse::{
        ast::{parse_test_block, TestBlock},
        tokenize, Token,
    },
    validation::{Context, Input, ValidateInstruction},
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

    #[arg(name = "parse", short = 'p', default_value = "false")]
    parse: bool,

    #[arg(name = "validate", short = 'v', default_value = "false")]
    validate: bool,

    #[arg(name = "execute", short = 'e', default_value = "false")]
    execute: bool,

    #[arg(name = "all", short = 'a', default_value = "false")]
    all: bool,

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

    test_parse_flag: bool,
    test_validate_flag: bool,
    test_execute_flag: bool,
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
        let len = lists.len();
        let mut pass = 0;
        let mut fail = 0;
        for item in lists {
            let file = item.clone();
            let name = file.file_name().unwrap().to_string_lossy();
            match self.test(item) {
                Ok(_) => {
                    pass += 1;
                    println!("PASS {}", name)
                }
                Err(err) => {
                    fail += 1;
                    if self.debug {
                        println!("FAIL {} with {:?}", name, err)
                    } else {
                        println!("FAIL {}", name)
                    }
                }
            };
        }
        println!("Passed {}/{}", pass, len);
        println!("Failed {}/{}", fail, len);
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
        self.test(file)
    }

    pub fn test(&self, file: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
        let path = file.clone();
        let name = path.file_name().unwrap().to_string_lossy();
        println!("TEST INPUT {} -------------------------------------", name);
        let tokens = self.test_tokenization(file)?;
        println!("PASS(tokenize): {}", name);
        if self.test_parse_flag {
            let blocks = self.test_parser(tokens)?;
            println!("PASS(parser): {}", name);
            if self.test_validate_flag {
                self.test_validate(&blocks)?;
                println!("PASS(validator): {}", name);
                if self.test_execute_flag {
                    todo!("Implement test execute");
                    // println!("PASS(executor): {}", name);
                }
            }
        }
        Ok(())
    }

    pub fn test_tokenization(
        &self,
        file: PathBuf,
    ) -> Result<Vec<Token>, Box<dyn std::error::Error>> {
        let contents = fs::read_to_string(file)?;
        println!("Scanning file and converting to tokens");
        let tokens = tokenize(&contents)?;
        if self.debug {
            println!("{:?}", tokens);
        }
        println!("Tokenization complete");
        Ok(tokens)
    }

    pub fn test_parser(
        &self,
        tokens: Vec<Token>,
    ) -> Result<Vec<TestBlock>, Box<dyn std::error::Error>> {
        let mut blocks = vec![];
        let mut iter = tokens.iter().peekable();
        while iter.peek().is_some() {
            blocks.push(parse_test_block(&mut iter, &tokens)?);
        }
        Ok(blocks)
    }

    pub fn test_validate(&self, blocks: &[TestBlock]) -> Result<(), Box<dyn std::error::Error>> {
        let mut last_module = None;
        for block in blocks.iter() {
            match (block, last_module) {
                (TestBlock::Module(module), _) => {
                    last_module = Some(module);
                    let mut ctx = Context::default();
                    let mut inputs = Input::default();
                    module.validate(&mut ctx, &mut inputs)?;
                }
                (TestBlock::AssertMalformed(module), _) => module.test()?,
                (TestBlock::AssertInvalid(module), _) => module.test()?,
                (TestBlock::AssertReturn(module), Some(last)) => module.test(last)?,
                _ => panic!("THis shouldn't happen"),
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
                test_parse_flag: test.all || test.parse,
                test_validate_flag: test.all || (test.parse && test.validate),
                test_execute_flag: test.all || (test.parse && test.validate && test.execute),
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
