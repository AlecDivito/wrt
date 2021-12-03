use std::{
    fmt::Display,
    str::{FromStr, SplitWhitespace},
};

use crate::error::{Result, WasmError};

pub struct SubString<'a> {
    index: usize,
    source: &'a str,
    breakpoints: Vec<usize>,
}

impl<'a> SubString<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            breakpoints: Vec::new(),
            index: 0,
            source,
        }
    }

    /// return the rest of the source starting at the current location of the index
    pub fn current(&self) -> &'a str {
        &self.source[self.index..]
    }

    pub fn expect(&self, source: &'a str) -> Result<bool> {
        if self.index <= self.source.len() {
            Ok(self.source[self.index..].starts_with(source))
        } else {
            Err(WasmError::new(0, self.index, "reached EOF"))
        }
    }

    pub fn eat(&mut self) {
        if self.index + 1 != self.source.len() {
            self.index = self.index + 1;
        }
    }

    pub fn split_on_whitespace(&mut self) -> SplitWhitespace {
        self.current().trim().split_whitespace()
    }

    /// read the values between quotation marks. From documentation it seems to
    /// allow any type of string so for now we just look for the ending character.
    pub fn eat_name(&mut self) -> Option<&'a str> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token(SubString::is_valid_name)
    }

    /// eat numeric characters
    pub fn eat_numeric(&mut self) -> Option<&'a str> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token(SubString::is_numeric)
    }

    /// eat instruction consumes only lowercase ascii characters
    pub fn eat_instruction(&mut self) -> Option<&'a str> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token(SubString::is_valid_instruction)
    }

    /// read block type until completition
    pub fn eat_block_type(&mut self) -> Result<BlockType> {
        self.eat_token(SubString::is_whitespace);
        if let Some(token) = self.eat_token(SubString::is_valid_block_type) {
            BlockType::from_str(token)
        } else {
            Err(WasmError::err("expected block string, was empty"))
        }
    }

    /// read the stream until the identifer has been completely read
    pub fn eat_identifier(&mut self) -> Option<&'a str> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token(SubString::is_valid_identifier)
    }

    pub fn breakpoint_content(&mut self) -> Option<&'a str> {
        if let Some(start) = self.pop_breakpoint() {
            if start >= self.index {
                return None;
            }
            let content = self.source[start..self.index].trim();
            self.eat();
            Some(content)
        } else {
            None
        }
    }

    pub fn push_breakpoint(&mut self) {
        self.breakpoints.push(self.index)
    }

    pub fn pop_breakpoint(&mut self) -> Option<usize> {
        self.breakpoints.pop()
    }

    pub fn swap_breakpoint(&mut self) {
        self.pop_breakpoint();
        self.push_breakpoint();
    }

    fn eat_token<F: Fn(char) -> bool>(&mut self, f: F) -> Option<&'a str> {
        let mut end = self.index;
        while end < self.source.len() {
            let character_slice = &self.source[end..end + 1];
            let character = character_slice.chars().next().unwrap();
            if f(character) {
                end = end + 1;
            } else {
                break;
            }
        }
        if self.index == end {
            None
        } else {
            let token = &self.source[self.index..end];
            self.index = end;
            Some(token)
        }
    }

    fn is_whitespace(byte: char) -> bool {
        byte.is_whitespace()
    }

    fn is_valid_identifier(byte: char) -> bool {
        byte.is_ascii_alphanumeric()
            || "!#$%&'*+-./:<=>?@\\~_`|~"
                .chars()
                .into_iter()
                .any(|c| c == byte)
    }

    fn is_valid_instruction(byte: char) -> bool {
        byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == '.'
    }

    fn is_valid_block_type(byte: char) -> bool {
        byte.is_ascii_lowercase()
    }

    fn is_numeric(byte: char) -> bool {
        byte.is_numeric()
    }

    fn is_valid_name(byte: char) -> bool {
        byte != '\"' as char
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockType {
    Module,
    Function,
    Export,
    Parameter,
    Result,
    Local,
}

impl BlockType {
    pub fn is_function(&self) -> bool {
        *self == BlockType::Function
    }
}

impl Display for BlockType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            BlockType::Module => "module",
            BlockType::Function => "function",
            BlockType::Export => "export",
            BlockType::Parameter => "param",
            BlockType::Result => "result",
            BlockType::Local => "local",
        };
        write!(f, "{}", content)
    }
}

impl FromStr for BlockType {
    type Err = WasmError;

    fn from_str(input: &str) -> std::result::Result<BlockType, Self::Err> {
        match input {
            "module" => Ok(BlockType::Module),
            "func" => Ok(BlockType::Function),
            "export" => Ok(BlockType::Export),
            "param" => Ok(BlockType::Parameter),
            "result" => Ok(BlockType::Result),
            "local" => Ok(BlockType::Local),
            _ => Err(WasmError::new(
                0,
                0,
                format!("type {} was unexpected", input),
            )),
        }
    }
}

pub struct Block<'a> {
    block_type: BlockType,
    variable_name: Option<&'a str>,
    content: Option<&'a str>,
    children: Vec<Block<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(block_type: BlockType) -> Self {
        Self {
            block_type,
            variable_name: None,
            content: None,
            children: Vec::new(),
        }
    }

    pub fn parse(source: &mut SubString<'a>) -> Result<Self> {
        // Eat the starting block
        if source.expect("(")? {
            source.eat();
        } else {
            return Err(WasmError::new(0, 0, "Reached EOF"));
        }

        // get the type of block
        let block_type = source.eat_block_type()?; // this should possibly return None
        let mut block = Block::new(block_type);
        source.eat();
        source.push_breakpoint();

        // Read the entire block
        while !source.expect(")")? {
            // assign this as a variable for a function
            if block.children.is_empty() && source.expect("$")? {
                if let Some(name) = source.eat_identifier() {
                    block.variable_name = Some(name);
                } else {
                    return Err(WasmError::new(0, 0, "Expected variable name"));
                }
                source.eat();
                source.swap_breakpoint();
            }

            // parse the name of the block
            if block.children.is_empty() && source.expect("\"")? {
                source.eat();
                if let Some(name) = source.eat_name() {
                    block.variable_name = Some(name);
                    source.eat();
                } else {
                    return Err(WasmError::new(0, 0, "Expected variable name for block"));
                }
                source.swap_breakpoint();
            }

            // new block to be parsed
            if source.expect("(")? {
                let child = Block::parse(source)?;
                block.children.push(child);
                source.swap_breakpoint();
            } else {
                // otherwise, keep incrementing by one
                source.eat();
            }
        }
        block.content = source.breakpoint_content();

        Ok(block)
    }

    pub fn walk(&self, spaces: Option<usize>) {
        let mut s = String::new();
        for i in 0..spaces.unwrap_or(0) {
            if i != 0 {
                s = s + " ";
            }
        }
        println!("{}{}", s, self);
        for block in &self.children {
            block.walk(Some(s.len() + 1))
        }
    }

    pub fn type_id(&self) -> &BlockType {
        &self.block_type
    }

    pub fn children(&self) -> &Vec<Block> {
        &self.children
    }

    pub fn content(&self) -> Option<&'a str> {
        self.content
    }

    pub(crate) fn variable_name(&self) -> Option<&'a str> {
        self.variable_name
    }
}

impl<'a> Display for Block<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prefix = self.block_type.to_string();
        if let Some(var) = self.variable_name {
            prefix = format!("{} -> {}", self.block_type, var);
        }
        let mut args = String::new();
        if let Some(content) = self.content {
            args = format!("({})", content);
        }
        write!(f, "{} {}", prefix, args)
    }
}
