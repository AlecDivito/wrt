use std::{convert::TryFrom, str::FromStr};

use crate::{
    block::{Block, BlockType, Identifier},
    error::WasmError,
    values::limit::Limit,
};

use super::instruction::Instruction;

use super::mem::MemoryUse;

pub struct DataString {
    data: Vec<u8>,
}

impl DataString {
    pub fn limit(&self) -> Limit {
        // TODO(Alec): This may break
        let min = self.data.len() as u32 / 65536;
        Limit::min(min)
    }
}

impl FromStr for DataString {
    type Err = WasmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let source = s.trim();
        if source.starts_with("\"") && source.ends_with("\"") {
            Ok(Self {
                data: source.as_bytes().to_vec(),
            })
        } else {
            Err(WasmError::err(
                "data string is not surrounded by quotation marks",
            ))
        }
    }
}

impl<'a> TryFrom<&mut &mut Block<'a>> for DataString {
    type Error = WasmError;

    fn try_from(block: &mut &mut Block<'a>) -> Result<Self, Self::Error> {
        block.expect(BlockType::Data)?;
        let name = block
            .pop_name()
            .ok_or(WasmError::err("expected string of bytes on data block"))?;
        Ok(Self {
            data: name.as_bytes().to_vec(),
        })
    }
}

pub struct Offset {
    instruction: Instruction,
}

impl<'a> TryFrom<&mut Block<'a>> for Offset {
    type Error = WasmError;

    fn try_from(block: &mut Block<'a>) -> Result<Self, Self::Error> {
        block.expect(BlockType::Offset)?;
        let mut str = vec![];
        while let Ok(attr) = block.pop_attribute() {
            str.push(attr.to_string());
        }
        let expr = str.join(" ");
        let instruction = Instruction::from_str(&expr)?;
        block.should_be_empty()?;
        Ok(Self { instruction })
    }
}

pub struct ActiveData {
    mem_use: MemoryUse,
    offset: Offset,
    data: DataString,
}

pub enum Modes {
    Passive(DataString),
    Active(ActiveData),
}

pub struct Data {
    id: Option<Identifier>,
    mode: Modes,
}

impl<'a> TryFrom<&mut Block<'a>> for Data {
    type Error = WasmError;

    fn try_from(mut block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Data)?;
        let id = block.take_id_or_attribute_as_identifier();

        let offset = if let Some(mut blk) = block.take_the_only_child_that_is(BlockType::Offset)? {
            Some(Offset::try_from(&mut blk)?)
        } else {
            None
        };

        let mode = if offset.is_none() {
            Modes::Passive(DataString::try_from(&mut block)?)
        } else {
            // decode using content
            let mem_use =
                if let Some(mut blk) = block.take_the_only_child_that_is(BlockType::Memory)? {
                    MemoryUse::try_from(&mut blk)?
                } else {
                    MemoryUse::default()
                };
            let data = DataString::from_str(
                &block
                    .take_content()
                    .ok_or(WasmError::err("expected content, found nothing"))?,
            )?;
            Modes::Active(ActiveData {
                mem_use,
                offset: offset.unwrap(),
                data,
            })
        };

        block.should_be_empty()?;
        Ok(Self { id, mode })
    }
}

#[cfg(test)]
mod test {
    use crate::block::SubString;
    use crate::error::Result;

    use super::*;

    fn parse(program: &str) -> Result<Data> {
        let mut source = SubString::new(program);
        let mut block = Block::parse(&mut source)?;
        Data::try_from(&mut block)
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
