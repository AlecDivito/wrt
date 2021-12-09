use std::{convert::TryFrom, fmt::Display, str::FromStr};

use crate::{
    block::{Block, BlockType, Identifier},
    error::{Result, WasmError},
    values::value::ValueType,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Call, // (Identifier),
    Return,

    LocalGet, // (Identifier),

    I32Add,
    I32Const, // (i32),
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Opcode::*;
        let s = match self {
            Call => "call",
            Return => "return",
            LocalGet => "local.get",
            I32Add => "i32.add",
            I32Const => "i32.const",
        };
        write!(f, "{}", s)
    }
}

impl FromStr for Opcode {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        use Opcode::*;
        Ok(match s {
            "call" => Call,
            "return" => Return,
            "local.get" => LocalGet,
            "i32.add" => I32Add,
            "i32.const" => I32Const,
            _ => {
                return Err(WasmError::err(format!(
                    "block type {} is not known unexpected",
                    s
                )))
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Oprand {
    Id(Identifier),
    Value(ValueType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    opcode: Opcode,
    oprands: Vec<Oprand>,
}

impl FromStr for Instruction {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut parts = s.split(" ");
        let opcode = Opcode::from_str(parts.next().ok_or(WasmError::err("expected opcode"))?)?;
        let mut items = vec![];
        while let Some(item) = parts.next() {
            items.push(item)
        }
        Instruction::parse(opcode, items)
    }
}

impl<'a> TryFrom<Block<'a>> for Instruction {
    type Error = WasmError;

    fn try_from(mut block: Block<'a>) -> std::result::Result<Self, Self::Error> {
        let content = block.take_content().unwrap_or(String::new());
        let items = content.split("\n").collect();
        block.should_be_empty()?;
        if let BlockType::Opcode(opcode) = block.type_id() {
            Instruction::parse(opcode.clone(), items)
        } else {
            Err(WasmError::err("did not find opcode when expected"))
        }
    }
}

impl Instruction {
    fn pop<'a>(opcode: &Opcode, items: &mut Vec<&'a str>) -> Result<&'a str> {
        items.pop().ok_or(WasmError::err(format!(
            "expected item for opcode {}, found none",
            opcode
        )))
    }

    fn parse(opcode: Opcode, mut items: Vec<&str>) -> Result<Instruction> {
        use Opcode::*;
        let oprands = match opcode {
            Return | I32Add => vec![],
            Call | LocalGet => {
                let item = Instruction::pop(&opcode, &mut items)?;
                vec![Oprand::Id(Identifier::from_str(item)?)]
            }
            I32Const => {
                let item = Instruction::pop(&opcode, &mut items)?;
                let arg = i32::from_str(item)?;
                vec![Oprand::Value(ValueType::I32(arg))]
            }
        };

        if !items.is_empty() {
            Err(WasmError::err(format!(
                "opcode {} recieved to many arguments",
                opcode
            )))
        } else {
            Ok(Self { opcode, oprands })
        }
    }
}
