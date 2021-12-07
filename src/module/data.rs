use std::convert::TryFrom;

use crate::{
    block::{Block, BlockType},
    error::WasmError,
};

use super::{instruction::Instruction, mem::Memory};

pub struct Offset {
    instruction: Instruction,
}

impl<'a> TryFrom<&Block<'a>> for Offset {
    type Error = WasmError;

    fn try_from(value: &Block<'a>) -> Result<Self, Self::Error> {
        block.expect(BlockType::Offset)?;
    }
}

pub struct ActiveData {
    mem: Memory,
    offset: Offset,
}

pub enum Modes {
    Passive,
    Active(),
}

pub struct Data {
    id: Option<String>,
    data: String,
    memory: Memory,
}

impl<'a> TryFrom<&Block<'a>> for Data {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Data)?;
        let id = block.try_identity()?;

        let name = block.names()?;
        let data = match name.len() {
            0 => Err(WasmError::err("data not defined but required")),
            1 => Ok(name[0].to_string()),
            _ => Err(WasmError::err(
                "too many data blocks defined, only one required",
            )),
        }?;

        let memory = Some(Memory::default());
    }
}

#[cfg(test)]
mod test {
    use crate::block::SubString;

    use super::*;

    fn parse(program: &str) -> Result<Data> {
        let mut source = SubString::new(program);
        let block = Block::parse(&mut source)?;
        Data::try_from(&block)
    }

    #[test]
    fn simple_data_block() {
        let data = parse("(data \"this is data\")").unwrap();
    }

    #[test]
    fn simple_data_block_with_id() {
        let data = parse("(data $id \"this is data\")").unwrap();
    }

    #[test]
    fn data_with_memory_use() {
        let data = parse("(data (memory 0) (offset (i32.const 0)) \"this is data\")").unwrap();
    }

    #[test]
    fn data_with_memory_use_with_id() {
        let data = parse("(data $id (memory 0) (offset (i32.const 0)) \"this is data\")").unwrap();
    }
}
