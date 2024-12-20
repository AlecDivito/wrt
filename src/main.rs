use std::{
    convert::TryFrom,
    fs,
    io::{stdin, stdout, BufRead, Read},
    os::unix::ffi::OsStrExt,
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use wrt::{
    ast::tree::{ast, parse, print_tee_with_indentation},
    execution::{
        instance::{Instance, Value},
        Store,
    },
    lex::{
        ast::{parse_test_block, TestBlock},
        tokenize, Token,
    },
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
    Repl,
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
        // let mut last_module = None;
        // for block in blocks.iter() {
        //     match (block/* , last_module */) {
        //         // (TestBlock::Module(module), _) => {
        //         //     last_module = Some(module);
        //         //     let mut ctx = Context::default();
        //         //     let mut inputs = Input::default();
        //         //     module.validate(&mut ctx, &mut inputs)?;
        //         // }
        //         // (TestBlock::AssertMalformed(module), _) => module.test()?,
        //         // (TestBlock::AssertInvalid(module), _) => module.test()?,
        //         // (TestBlock::AssertReturn(module), Some(last)) => module.test(last)?,
        //         // (TestBlock::AssertTrap(module), Some(last)) => module.test(last)?,
        //         _ => panic!("THis shouldn't happen"),
        //     }
        // }
        Ok(())
    }

    pub fn stdin(&self) {}
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    use std::io::Write;

    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .create(true)
        .truncate(false)
        .append(true)
        .open(".history")?;

    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let mut history = buffer
        .split("---\n")
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();

    let stdout = stdout();
    let stdin = stdin();
    let mut full_buffer = String::new();

    let mut indentation = 0;

    loop {
        buffer.clear();

        let mut lock: std::io::StdoutLock<'_> = stdout.lock();
        lock.write_all(format!("{}> ", "..".repeat(indentation)).as_bytes())?;
        lock.flush()?;
        drop(lock);

        let mut lock = stdin.lock();
        lock.read_line(&mut buffer)?;
        drop(lock);

        match buffer.trim().split(" ").next() {
            Some(".exit") => return Ok(()),
            Some(".buffer") => {
                let mut lock: std::io::StdoutLock<'_> = stdout.lock();
                lock.write_all(format!("{}\n", full_buffer).as_bytes())?;
                lock.flush()?;
                drop(lock);
            }
            Some(".tokens") => {
                let default = "".to_string();
                let program = if !full_buffer.is_empty() {
                    &full_buffer
                } else if let Some(token) = buffer.trim().split(" ").skip(1).next() {
                    if let Ok(index) = token.parse::<usize>() {
                        history.get(index).unwrap_or(&default)
                    } else {
                        &default
                    }
                } else if let Some(program) = history.last() {
                    program
                } else {
                    &default
                };
                let tokens = tokenize(program)?;
                let mut lock: std::io::StdoutLock<'_> = stdout.lock();
                for (index, item) in tokens.iter().enumerate() {
                    lock.write_all(format!("[{}]\t{}\n", index, item).as_bytes())?;
                }
                lock.write_all(b"\n".as_slice())?;
                lock.flush()?;
                drop(lock);
            }
            Some(".nodes") => {
                let program = if let Some(token) = buffer.trim().split(" ").skip(1).next() {
                    if let Ok(index) = token.parse::<usize>() {
                        history.get(index)
                    } else {
                        history.last()
                    }
                } else {
                    history.last()
                };

                if let Some(program) = program {
                    let tokens = tokenize(program)?;
                    let mut iter = tokens.iter().peekable();
                    let node = parse(&mut iter)?;
                    let mut lock: std::io::StdoutLock<'_> = stdout.lock();
                    print_tee_with_indentation(&mut lock, &node, indentation + 1, false)?;
                    lock.flush()?;
                    drop(lock);
                } else {
                    println!("No Nodes currently exist!");
                }
            }
            Some(".history") => {
                let mut lock: std::io::StdoutLock<'_> = stdout.lock();
                for (index, item) in history.iter().enumerate() {
                    let programs = format!("[{}] {}\n", index, item.replace("\n", "\n    "));
                    lock.write_all(programs.as_bytes())?;
                }
                lock.write_all(b"\n")?;
                lock.flush()?;
                drop(lock);
            }
            Some(".delete") => {
                let mut iter = buffer.trim().split(" ");
                let _ = iter.next();
                if let Some(func) = iter.next() {
                    if let Ok(index) = func.parse::<usize>() {
                        history.remove(index);

                        drop(file);
                        file = std::fs::OpenOptions::new()
                            .truncate(true)
                            .write(true)
                            .open(".history")?;

                        file.write_all(history.join("\n---\n").as_bytes())?;
                        full_buffer.clear();
                    }
                }
            }
            Some(".call") => {
                let mut iter = buffer.trim().split(" ").skip(1);
                let program = if let Some(token) = iter.next() {
                    if let Ok(index) = token.parse::<usize>() {
                        history.get(index)
                    } else {
                        history.last()
                    }
                } else {
                    history.last()
                };

                if let Some(program) = program {
                    use std::convert::TryFrom;

                    let tokens = tokenize(program)?;
                    let mut iter = tokens.iter().peekable();
                    let nodes = parse(&mut iter)?;
                    let tree = ast(&nodes)?;
                    let module = tree.as_module().unwrap();
                    // let module = module_node.validate();
                    // let store = Store::new();
                    let instance = Instance::new(module);
                    // let instance = module.instance(&mut store)?;
                    let values = iter.map(|s| s.source()).collect::<Vec<_>>();
                    let mut iter = values.iter();
                    let returned = if let Some(func) = iter.next() {
                        instance.call(func, values.as_slice())
                    } else {
                        instance.call("main", values.as_slice())
                    };
                    println!("{:?}", returned);
                }
            }
            Some(_) => {
                full_buffer.push_str(&format!("\n{}{}", "  ".repeat(indentation), buffer.trim()));
                let tks = tokenize(&full_buffer)?;

                let left_parenthes = tks.iter().filter(|t| t.ty().is_left_paren()).count();
                let right_parenthes = tks.iter().filter(|t| t.ty().is_right_paren()).count();

                if right_parenthes > left_parenthes {
                    indentation = 0;
                } else {
                    indentation = left_parenthes - right_parenthes;
                }
                if indentation == 0 {
                    history.push(full_buffer.clone());
                    file.write_all(b"\n---\n")?;
                    file.write_all(full_buffer.as_bytes())?;
                    full_buffer.clear();
                }
            }
            _ => {}
        }
    }
}

fn main() {
    let cli = Cli::parse();

    if let Err(err) = match cli.command {
        Commands::Repl => repl(),
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
