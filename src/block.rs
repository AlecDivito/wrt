use std::{fmt::Display, str::FromStr};

use crate::{
    error::{Result, WasmError, WrapContext, WrapError},
    values::value::ValueType,
};

pub struct SubString<'a> {
    index: usize,
    source: &'a str,
    breakpoints: Vec<usize>,
    line: usize,
    column: usize,
}

impl<'a> SubString<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            breakpoints: Vec::new(),
            index: 0,
            source,
            line: 0,
            column: 0,
        }
    }

    /// return the rest of the source starting at the current location of the index
    fn current(&self) -> &'a str {
        &self.source[self.index..]
    }

    /// expect what the starting of the current index looks like. Good for peeking
    /// at the current character.
    fn expect(&self, source: &'a str) -> Result<bool> {
        if self.index <= self.source.len() {
            Ok(self.source[self.index..].starts_with(source))
        } else {
            //TODO(Alec): Change this so that we convert the sub string into the
            // error. We want to utilize the substring class to create a more
            // detail error.
            Err(WasmError::err("reached EOF"))
        }
    }

    /// peek at the character and check if it is a text
    fn expect_text(&self) -> Result<bool> {
        if self.index <= self.source.len() {
            Ok(self.source[self.index..self.index + 1]
                .chars()
                .next()
                .unwrap()
                .is_alphabetic())
        } else {
            //TODO(Alec): Change this so that we convert the sub string into the
            // error. We want to utilize the substring class to create a more
            // detail error.
            Err(WasmError::err("reached EOF"))
        }
    }

    /// peek at the character and check if it is a digit. Good for setting indexs
    /// or identifications
    fn expect_digit(&self) -> Result<bool> {
        if self.index <= self.source.len() {
            Ok(self.source[self.index..self.index + 1]
                .chars()
                .next()
                .unwrap()
                .is_digit(10))
        } else {
            //TODO(Alec): Change this so that we convert the sub string into the
            // error. We want to utilize the substring class to create a more
            // detail error.
            Err(WasmError::err("reached EOF"))
        }
    }

    /// move the index counter up one as long as it is not as long as the string
    fn eat(&mut self) -> Result<()> {
        if self.index + 1 != self.source.len() {
            self.index = self.index + 1;
            if self.expect("\n")? {
                self.line = self.line + 1;
                self.column = 0;
            } else {
                self.column = self.column + 1;
            }
            Ok(())
        } else {
            Err(WasmError::eof())
        }
    }

    /// eats any character that is white space
    fn eat_white_space(&mut self) {
        self.eat_token(SubString::is_whitespace);
    }

    /// eat an entire line if the current index points at a line comment
    fn eat_line_comment(&mut self) -> Result<()> {
        if self.expect(";;")? {
            self.eat_token(|f| f == '\n');
            Ok(())
        } else {
            Err(WasmError::err(
                "parse line comment called but no comment found",
            ))
        }
    }

    /// eat an entire multi line comment if the index points at the correct characters
    fn eat_multi_line_comment(&mut self) -> Result<()> {
        if self.expect("(;")? {
            while self.index + 2 < self.source.len() {
                let character_slice = &self.source[self.index..self.index + 2];
                if character_slice.starts_with(";)") {
                    break;
                } else {
                    self.eat()?;
                }
            }
            // TODO(Alec): Error checking
            Ok(())
        } else {
            Err(WasmError::err(
                "parse multi line comment called but no multi line comment found",
            ))
        }
    }

    /// read the values between quotation marks. From documentation it seems to
    /// allow any type of string so for now we just look for the ending character.
    fn eat_name(&mut self) -> Result<Option<&'a str>> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token_until(SubString::is_valid_name, SubString::is_quotation)
            .wrap_err("failed to eat name token")
    }

    /// eat text values until completely read. TODO(Alec): we may not need this
    fn eat_text(&mut self) -> Result<Option<&'a str>> {
        self.eat_token_until(SubString::is_text, SubString::is_whitespace_or_end_block)
            .wrap_err("failed to eat text token")
    }

    /// eat numeric characters
    fn eat_numeric(&mut self) -> Result<Option<&'a str>> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token_until(SubString::is_numeric, SubString::is_whitespace_or_end_block)
            .wrap_err("failed to eat numeric token")
    }

    /// eat instruction consumes only lowercase ascii characters and periods
    fn eat_instruction(&mut self) -> Result<Option<&'a str>> {
        self.eat_token(SubString::is_whitespace);
        self.eat_token_until(
            SubString::is_valid_instruction,
            SubString::is_whitespace_or_end_block,
        )
        .wrap_err("failed to eat instruction token")
    }

    /// eat a token and try to convert it into a block type
    fn eat_block_type(&mut self) -> Result<BlockType> {
        self.eat_token(SubString::is_whitespace);
        if let Some(token) = self
            .eat_token_until(
                SubString::is_valid_block_type,
                SubString::is_whitespace_or_end_block,
            )
            .wrap_err("failed to eat instruction token")?
        {
            BlockType::from_str(token)
        } else {
            Err(WasmError::err("expected block string, was empty"))
        }
    }

    /// read the stream until the identifer has been completely read. This can
    /// only be used for identiers with a '$' as that is the only time we know
    /// for sure they are an identifer.
    fn eat_string_identifier(&mut self) -> Result<Identifier> {
        self.eat_token(SubString::is_whitespace);
        // we have at least one digit
        let id = self.eat_token(SubString::is_valid_identifier).unwrap();
        if id.starts_with("$") {
            Ok(Identifier::String(id.to_string()))
        } else {
            Err(WasmError::err(
                "expected string or number identifier, found something else",
            ))
        }
    }

    fn block_as_string(&self, block: &Block) -> &str {
        &self.source[block.start_block_index..self.index + 1]
    }

    /// setup a breakpoint to track the content found after reading all of the
    /// child blocks. This is mainly to pick up instructions that are posted
    /// line by line after a func block.
    fn breakpoint_content(&mut self) -> Option<&'a str> {
        if let Some(start) = self.pop_breakpoint() {
            if start >= self.index {
                return None;
            }
            let content = self.source[start..self.index].trim();
            Some(content)
        } else {
            None
        }
    }

    /// push a new breakpoint
    fn push_breakpoint(&mut self) {
        self.breakpoints.push(self.index)
    }

    /// pop a breakpoint from the list
    fn pop_breakpoint(&mut self) -> Option<usize> {
        self.breakpoints.pop()
    }

    /// update the newest breakpoints position
    fn swap_breakpoint(&mut self) {
        self.pop_breakpoint();
        self.push_breakpoint();
    }

    /// eat a token until the function passed in does not equate to true
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
            // self.index = end;
            for _ in self.index..end {
                self.eat().unwrap();
            }
            Some(token)
        }
    }

    /// eat a token until told not to. This validates that the token ends with
    /// the correct character, otherwise an error is returned. Similar with
    /// eat_token with more error checking
    fn eat_token_until<E: Fn(char) -> bool, U: Fn(char) -> bool>(
        &mut self,
        eat: E,
        until: U,
    ) -> Result<Option<&'a str>> {
        let mut end = self.index;
        while end < self.source.len() {
            let character_slice = &self.source[end..end + 1];
            let character = character_slice.chars().next().unwrap();
            if eat(character) {
                end = end + 1;
            } else if until(character) {
                break;
            } else {
                let e = Err(WasmError::character(character)).wrap_context(format!(
                    "found at position: line {} and character {}",
                    self.line,
                    self.column - 1
                ));
                return e;
            }
        }
        if self.index == end {
            Ok(None)
        } else {
            let token = &self.source[self.index..end];
            // self.index = end;
            for _ in self.index..end {
                self.eat().unwrap();
            }
            Ok(Some(token))
        }
    }

    fn is_whitespace(byte: char) -> bool {
        byte.is_whitespace()
    }

    fn is_whitespace_or_end_block(byte: char) -> bool {
        byte.is_whitespace() || byte == ')'
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

    fn is_text(byte: char) -> bool {
        byte.is_alphanumeric()
    }

    fn is_valid_name(byte: char) -> bool {
        byte != '\"' as char
    }

    fn is_quotation(byte: char) -> bool {
        byte == '"'
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockType {
    Module,
    Global,
    Import,
    Export,
    Table,
    Memory,
    Function,
    Parameter,
    Result,
    Local,
    Type,
    Data,
    Offset,
    Mut, // this is a special block
         // Instruction(Instruction),
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
            BlockType::Global => "global",
            BlockType::Import => "import",
            BlockType::Export => "export",
            BlockType::Function => "func",
            BlockType::Parameter => "param",
            BlockType::Result => "result",
            BlockType::Local => "local",
            BlockType::Type => "type",
            BlockType::Mut => "mut",
            BlockType::Table => "table",
            BlockType::Memory => "memory",
            BlockType::Data => "data",
            BlockType::Offset => "offset",
            // BlockType::Instruction(i) => return write!(f, "{}", i.to_string()),
        };
        write!(f, "{}", content)
    }
}

impl FromStr for BlockType {
    type Err = WasmError;

    fn from_str(input: &str) -> std::result::Result<BlockType, Self::Err> {
        match input {
            "module" => Ok(BlockType::Module),
            "global" => Ok(BlockType::Global),
            "import" => Ok(BlockType::Import),
            "export" => Ok(BlockType::Export),
            "func" => Ok(BlockType::Function),
            "param" => Ok(BlockType::Parameter),
            "result" => Ok(BlockType::Result),
            "local" => Ok(BlockType::Local),
            "type" => Ok(BlockType::Type),
            "mut" => Ok(BlockType::Mut),
            "table" => Ok(BlockType::Table),
            "memory" => Ok(BlockType::Memory),
            "data" => Ok(BlockType::Data),
            "offset" => Ok(BlockType::Offset),
            _ => Err(WasmError::err(format!("type {} was unexpected", input))),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Identifier {
    String(String),
    Number(usize),
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::String(s) => write!(f, "{}", s),
            Identifier::Number(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Attribute<'a> {
    Str(&'a str),
    Num(&'a str),
}

impl<'a> Display for Attribute<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Str(v) => write!(f, "{}", v),
            Attribute::Num(v) => write!(f, "{}", v),
        }
    }
}

pub struct Block<'a> {
    /// the type of the block found
    block_type: BlockType,

    /// a string identifier, digit identifers must be parsed by the actual
    /// block.
    id: Option<Identifier>,

    /// all attributes surrounded by quotation marks
    names: Vec<&'a str>,

    /// any digit or string values. Block implementation must decide how they are
    /// used and parsed. These are any values that are not surrounded by quotation
    /// or start with a dollar sign.
    attributes: Vec<Attribute<'a>>,

    /// any block found inside of the current block.
    children: Vec<Block<'a>>,

    /// any text found at the end of the block that isn't another block.
    content: Vec<&'a str>,

    /// used to assign the start of a block from the source text
    start_block_index: usize,
}

impl<'a> Block<'a> {
    /// create a new empty block with a block type set
    fn new(block_type: BlockType, start_block_index: usize) -> Self {
        Self {
            block_type,
            id: None,
            names: Vec::new(),
            attributes: Vec::new(),
            children: Vec::new(),
            content: Vec::new(),
            start_block_index,
        }
    }

    /// parse some type of web assembly text and build up a tree of blocks making
    /// up the program.
    pub fn parse(source: &mut SubString<'a>) -> Result<Self> {
        // Eat the starting block
        let start_block_index = source.index;
        if source.expect("(")? {
            source.eat()?;
        } else {
            return Err(WasmError::err("Reached EOF"));
        };

        // get the type of block
        let block_type = source
            .eat_block_type()
            .wrap_context("Block type was not found")?; // this should possibly return None
        let mut block = Block::new(block_type, start_block_index);
        source.eat_white_space();
        source.push_breakpoint();

        if let Err(mut err) = Block::parse_block(&mut block, source) {
            if err.context_len() <= 1 {
                Err(err).wrap_context(format!(
                    "Parsed block up until: {}",
                    source.block_as_string(&block)
                ))
            } else {
                Err(err)
            }
        } else {
            Ok(block)
        }
    }

    fn parse_block(block: &mut Block<'a>, source: &mut SubString<'a>) -> Result<()> {
        // Read the entire block
        while !source.expect(")")? {
            // 1. if we expect a multiline comment
            if source.expect("(;")? {
                let temporary_content = source.breakpoint_content().unwrap_or("");
                block.content.push(temporary_content);
                source.eat_multi_line_comment()?;
                source.eat()?; // eat ';'
                source.eat()?; // eat ')'
                source.eat_white_space();
                source.push_breakpoint();
            }
            // 2. if we expect a line comment
            else if source.expect(";;")? {
                let temporary_content = source.breakpoint_content().unwrap_or("");
                block.content.push(temporary_content);
                source.eat_line_comment()?;
                source.eat_white_space();
                source.push_breakpoint();
            }
            // 3. if it is a new block, parse it!
            else if source.expect("(")? {
                let child = Block::parse(source).wrap_err(format!(
                    "Error when parsing child block in ({}) block",
                    block.block_type
                ))?;
                block.children.push(child);
                source.eat()?; // eat the ending (')') block parentheses
                source.swap_breakpoint();
            }
            // 4. if the block is empty and the current index points to a '$', it's an id!
            else if block.is_empty() && source.expect("$")? {
                let id = source.eat_string_identifier().wrap_err(format!(
                    "Error when parsing id block in ({}) block",
                    block.block_type
                ))?;
                block.id.insert(id);
                source.swap_breakpoint();
            }
            // 5. if content is empty and the current index is a digit, it's an attribute
            else if block.is_content_empty() && source.expect_digit()? {
                let attribute = source
                    .eat_numeric()?
                    .ok_or(WasmError::err("Expected numeric block"))
                    .wrap_err(format!(
                        "Error when parsing numeric attribute in ({}) block",
                        block.block_type
                    ))?;
                block.attributes.push(Attribute::Num(attribute));
                source.swap_breakpoint();
            }
            // 6. if content is empty and the current index is text, it's an attribute
            else if block.is_content_empty() && source.expect_text()? {
                let attribute = source
                    .eat_text()?
                    .ok_or(WasmError::err("Expected text block"))
                    .wrap_err(format!(
                        "Error when alpha-numeric attribute in ({}) block",
                        block.block_type
                    ))?;
                block.attributes.push(Attribute::Str(attribute));
                source.swap_breakpoint();
            }
            // 7. if the next character is an quotation
            else if source.expect("\"")? {
                source.eat()?;
                let name = source
                    .eat_name()?
                    .ok_or(WasmError::err("Expected variable name for block"))
                    .wrap_err(format!(
                        "Error when parsing quotation name in ({}) block",
                        block.block_type
                    ))?;
                block.names.push(name);
                source.eat()?;
                source.swap_breakpoint();
            }
            // 8. keep incrementing by one
            else {
                source.eat()?;
            }
        }
        block
            .content
            .push(source.breakpoint_content().unwrap_or(""));

        let mut vec = vec![];
        while let Some(content) = block.content.pop() {
            if !content.trim().is_empty() {
                vec.push(content);
            }
        }
        block.content = vec;

        Ok(())
    }

    /// walk the web assembly program and re-print the program back to console
    pub fn walk(&self, spaces: Option<usize>) {
        let mut s = String::new();
        for _ in 0..spaces.unwrap_or(0) {
            s = format!(" {}", s);
        }
        println!("{}{}", s, self);
        for block in &self.children {
            block.walk(Some(s.len() + 1))
        }
    }

    /// expect the type of block this block is
    pub fn expect(&self, type_id: BlockType) -> Result<()> {
        if self.block_type == type_id {
            Ok(())
        } else {
            Err(WasmError::expect(type_id, &self.block_type))
        }
    }

    /// expect that the block is empty. If not, throw an error
    pub fn should_be_empty(&self) -> Result<()> {
        // self.names.is_empty() && self.attributes.is_empty() && self.children.is_empty()
        let is_empty = self.is_empty() && self.id.is_none() && self.content.is_empty();
        if is_empty {
            Ok(())
        } else {
            let mut err = Err(WasmError::block_not_empty(self));
            if self.id.is_some() {
                err = err.wrap_context("Did not expect id but found one still in block");
            }
            if !self.attributes.is_empty() {
                err = err.wrap_context("More attributes found in block then expected");
            }
            if !self.names.is_empty() {
                err = err.wrap_context("More names found in block then expected");
            }
            if !self.children.is_empty() {
                err = err.wrap_context("More child blocks found in block then expected");
            }
            if !self.content.is_empty() {
                err = err.wrap_context("Content was found in block when it was not expected");
            }
            err
        }
    }

    /// take children that are equal to a certian block type.
    pub fn take_children_that_are(&mut self, parameter: BlockType) -> Vec<Block> {
        let mut i = 0;
        let mut items = vec![];
        while i < self.children.len() {
            if self.children[i].block_type == parameter {
                items.push(self.children.remove(i));
            } else {
                i += 1;
            }
        }
        items
    }

    /// get access to all of the children (should probably not be used)
    // pub fn children(&'a mut self) -> &'a mut Vec<Block> {
    //     self.children.as_mut()
    // }

    pub fn type_id(&self) -> &BlockType {
        &self.block_type
    }

    pub fn attribute_length(&self) -> usize {
        self.attributes.len()
    }

    pub fn take_id(&mut self) -> Option<Identifier> {
        self.id.take()
    }

    pub fn pop_child(&mut self) -> Option<Block<'a>> {
        self.children.pop()
    }

    pub fn pop_attribute(&mut self) -> Result<Attribute<'a>> {
        self.attributes
            .pop()
            .ok_or(WasmError::err("expected attribute; found nothing"))
    }

    pub fn pop_attribute_as_identifier(&mut self) -> Result<Identifier> {
        let attr = self.pop_attribute()?;
        match attr {
            Attribute::Str(_) => {
                return Err(WasmError::err(
                    "expected attribute to be number identifier type, found string",
                ))
            }
            Attribute::Num(n) => Ok(Identifier::Number(n.parse::<usize>()?)),
        }
    }

    pub fn pop_attribute_as_value_type(&mut self) -> Result<ValueType> {
        let attr = self.pop_attribute()?;
        match attr {
            Attribute::Str(s) => Ok(ValueType::from_str(s)?),
            Attribute::Num(_) => {
                return Err(WasmError::err(
                    "expected attribute to be value type, found integer",
                ))
            }
        }
    }

    /// convert all attributes to value types
    pub fn all_attributes_to_value_type(&mut self) -> Result<Vec<ValueType>> {
        let mut vec = vec![];
        while !self.attributes.is_empty() {
            vec.push(self.pop_attribute_as_value_type()?);
        }
        Ok(vec)
    }

    // pub(crate) fn export_children(&self) -> Result<Vec<Export>> {
    // let mut exports = vec![];
    // for child in &self.children {
    //     if BlockType::Export == *child.type_id() {
    //         exports.push(Export::try_from(child)?)
    //     }
    // }
    // Ok(exports)
    // }

    // pub(crate) fn import_child(&self) -> Result<Option<Import>> {
    //     let mut import = None;
    //     for child in &self.children {
    //         if BlockType::Import == *child.type_id() {
    //             if import.is_some() {
    //                 return Err(WasmError::err(
    //                     "only one import can be declared on an block",
    //                 ));
    //             } else {
    //                 import.insert(Import::try_from(child)?);
    //             }
    //         }
    //     }
    //     Ok(import)
    // }

    // pub(crate) fn names(&self) -> Result<&[&str]> {
    //     Ok(&self.variable_name)
    // }

    // pub(crate) fn try_identity(&self) -> Result<Option<Identifier>> {
    //     match self.variable_name.len() {
    //         0 => Ok(None),
    //         1 => {
    //             if let Some(id) = self.variable_name.get(0).map(|s| s.to_string()) {
    //                 if id.starts_with("$") {
    //                     Ok(Some(Identifier::String(id)))
    //                 } else if let Ok(num) = id.parse::<usize>() {
    //                     Ok(Some(Identifier::Number(num)))
    //                 } else {
    //                     Err(WasmError::err(
    //                         "expected string or number identifier, found something else",
    //                     ))
    //                 }
    //             } else {
    //                 Ok(None)
    //             }
    //         }
    //         _ => Err(WasmError::err("tried to find idenity, found to many")),
    //     }
    // }

    #[inline]
    fn is_empty(&self) -> bool {
        self.names.is_empty() && self.attributes.is_empty() && self.children.is_empty()
    }

    #[inline]
    fn is_content_empty(&self) -> bool {
        self.names.is_empty() && self.children.is_empty()
    }
}

impl<'a> Display for Block<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let block = self.block_type.to_string();

        let attributes = self
            .attributes
            .iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<String>>()
            .join(" ");

        let children = self
            .children
            .iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<String>>()
            .join("\n\t");

        let content = self
            .content
            .iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<String>>()
            .join("\n");

        if let Some(id) = &self.id {
            write!(
                f,
                "({} {} {} {} {})",
                block, id, attributes, children, content
            )
        } else {
            write!(f, "({} {} {} {})", block, attributes, children, content)
        }
    }
}
