use std::convert::TryFrom;

use crate::{
    block::{Block, BlockType, Identifier},
    error::{Result, WasmError},
    values::limit::Limit,
};

use super::{data::DataString, export::Export, import::Import};

pub struct MemoryUse {
    id: Option<Identifier>,
}

impl Default for MemoryUse {
    fn default() -> Self {
        Self {
            id: Some(Identifier::Number(0)),
        }
    }
}

impl<'a> TryFrom<&mut Block<'a>> for MemoryUse {
    type Error = WasmError;

    fn try_from(block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Memory)?;
        let id = block.take_id_or_attribute_as_identifier();
        block.should_be_empty()?;
        Ok(Self { id })
    }
}

pub struct Memory {
    id: Option<Identifier>,
    limit: Limit,

    // other stuff
    exports: Vec<Export>,
    import: Option<Import>,
    data: Option<DataString>,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            id: Some(Identifier::Number(0)),
            limit: Limit::min(1),
            exports: vec![],
            import: None,
            data: None,
        }
    }
}

impl<'a> TryFrom<&mut Block<'a>> for Memory {
    type Error = WasmError;

    fn try_from(mut block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Memory)?;

        let exports = block
            .take_children_that_are(BlockType::Export)
            .iter_mut()
            .map(|block| Export::try_from_block_without_description(block, None))
            .collect::<Result<Vec<Export>>>()?;

        let mut import = None;
        if let Some(mut block) = block.take_the_only_child_that_is(BlockType::Import)? {
            import = Some(Import::try_from_block_without_description(
                &mut block, None, None,
            )?);
        }

        let data = if let Some(mut blk) = block.take_the_only_child_that_is(BlockType::Data)? {
            Some(DataString::try_from(&mut &mut blk)?)
        } else {
            None
        };

        let limit = if let Some(d) = &data {
            d.limit()
        } else {
            Limit::try_from(&mut block)?
        };

        let id = block.take_id_or_attribute_as_identifier();
        block.should_be_empty()?;

        Ok(Self {
            id,
            limit,
            import,
            exports,
            data,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::block::SubString;
    use crate::error::Result;

    use super::*;

    fn parse(program: &str) -> Result<Memory> {
        let mut source = SubString::new(program);
        let mut block = Block::parse(&mut source)?;
        Memory::try_from(&mut block)
    }

    #[test]
    fn memory_with_min() {
        let mem = parse("(memory 1)").unwrap();
        assert_eq!(mem.limit, Limit::min(1));
        assert!(mem.exports.is_empty());
        assert!(mem.import.is_none());
        assert!(mem.id.is_none());
    }

    #[test]
    fn memory_with_min_max() {
        let mem = parse("(memory 1 2)").unwrap();
        assert_eq!(mem.limit, Limit::new(1, Some(2)));
        assert!(mem.exports.is_empty());
        assert!(mem.import.is_none());
        assert!(mem.id.is_none());
    }

    #[test]
    fn memory_with_limits_and_id() {
        let mem = parse("(memory $id 1 2)").unwrap();
        assert_eq!(mem.limit, Limit::new(1, Some(2)));
        assert_eq!(&mem.id.unwrap().to_string(), "$id");
        assert!(mem.exports.is_empty());
        assert!(mem.import.is_none());
    }

    #[test]
    fn memory_with_export_and_min() {
        let mem = parse("(memory (export \"m2\") 1)").unwrap();
        assert_eq!(mem.exports[0], Export::new("m2"));
        assert_eq!(mem.limit, Limit::min(1));
        assert!(mem.import.is_none());
        assert!(mem.id.is_none());
    }
}
