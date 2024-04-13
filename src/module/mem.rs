use std::{convert::TryFrom, str::FromStr};

use crate::{
    block::{Block, BlockType},
    error::WasmError,
    values::limit::Limit,
};

use super::{export::Export, import::Import};

pub struct Memory {
    id: Option<String>,
    limit: Limit,

    // other stuff
    exports: Vec<Export>,
    import: Option<Import>,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            id: Some("0".into()),
            limit: Limit::min(1),
            exports: vec![],
            import: None,
        }
    }
}

impl<'a> TryFrom<&Block<'a>> for Memory {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Memory)?;
        let id = block.try_identity()?;

        let limit = if let Some(content) = block.content() {
            Limit::from_str(content)?
        } else {
            //TODO(Alec): this is where we check for a data definition
            todo!("implement the check for data definition");
        };

        let exports = block.export_children()?;
        let import = block.import_child()?;

        Ok(Self {
            id,
            limit,
            exports,
            import,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::error::Result;
    use crate::{block::SubString, module::export::Export};

    use super::*;

    fn parse(program: &str) -> Result<Memory> {
        let mut source = SubString::new(program);
        let block = Block::parse(&mut source)?;
        Memory::try_from(&block)
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
        assert_eq!(mem.id.unwrap(), "$id");
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
