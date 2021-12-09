use std::{convert::TryFrom, str::FromStr};

use crate::{
    block::{Block, BlockType, Identifier},
    error::{Result, WasmError, WrapContext},
    values::{limit::Limit, value::RefType},
};

use super::{export::Export, import::Import};

#[derive(Debug)]
pub struct TableType {
    limit: Limit,
    ref_type: RefType,
}

impl FromStr for TableType {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut blocks = s
            .trim()
            .split_whitespace()
            .filter(|s| !s.trim().is_empty())
            .collect::<Vec<&str>>();
        let ref_type = RefType::from_str(blocks.pop().ok_or(WasmError::err(
            "no limits or ref type decleared for table type",
        ))?)?;

        let min_or_max = blocks
            .pop()
            .ok_or(WasmError::err(
                "no limits or ref type decleared for table type",
            ))?
            .parse::<u32>()?;

        if let Some(block) = blocks.pop() {
            let min = block.parse::<u32>()?;
            Ok(Self {
                ref_type,
                limit: Limit::new(min, Some(min_or_max)),
            })
        } else {
            Ok(Self {
                ref_type,
                limit: Limit::min(min_or_max),
            })
        }
    }
}

impl<'a> TryFrom<&mut &mut Block<'a>> for TableType {
    type Error = WasmError;

    fn try_from(block: &mut &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Table)?;
        let ref_type = RefType::from_str(
            (*block)
                .pop_attribute()
                .wrap_context("no attribute found for ref type")?
                .as_str()
                .wrap_context("ref type is not a string")?,
        )
        .wrap_context("error when building ref_type for table type")?;
        let limit =
            Limit::try_from(block).wrap_context("error when building limits for table type")?;
        Ok(Self { limit, ref_type })
    }
}

#[derive(Debug)]
pub struct Table {
    id: Option<Identifier>,
    table_type: TableType,

    import: Option<Import>,
    exports: Vec<Export>,
}

impl<'a> TryFrom<&mut Block<'a>> for Table {
    type Error = WasmError;

    fn try_from(mut block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Table)?;

        let mut import = None;
        if let Some(mut blk) = block.take_the_only_child_that_is(BlockType::Import)? {
            import = Some(Import::try_from_block_without_description(
                &mut blk, None, None,
            )?);
        }

        let exports = block
            .take_children_that_are(BlockType::Export)
            .iter_mut()
            .map(|block| Export::try_from_block_without_description(block, None))
            .collect::<Result<Vec<Export>>>()?;

        let table_type = if exports.is_empty() && import.is_none() {
            TableType::try_from(&mut block).wrap_context("failed to build table type for table")
        } else {
            if let Some(content) = block.take_content() {
                TableType::from_str(&content)
            } else {
                Err(WasmError::err(
                    "no limit or/and ref type was provided for table definition",
                ))
            }
        }?;

        let id = block.take_id_or_attribute_as_identifier();

        block.should_be_empty()?;
        Ok(Self {
            id,
            table_type,
            import,
            exports,
        })
    }
}

#[cfg(test)]
mod test {

    use crate::block::{Block, Identifier};

    use super::*;

    fn parse(string: &str) -> Result<Table> {
        let mut source = crate::block::SubString::new(string);
        let mut block = Block::parse(&mut source)?;
        let func = Table::try_from(&mut block)?;
        Ok(func)
    }

    #[test]
    fn table_block_fails() {
        assert!(parse("(table)").is_err());
    }

    #[test]
    fn table_block_with_id_fails() {
        assert!(parse("(table $id)").is_err());
    }

    #[test]
    fn table_block_with_no_type_fails() {
        assert!(parse("(table 0 10)").is_err());
    }

    #[test]
    fn table_block_with_no_limit_fails() {
        assert!(parse("(table funcref)").is_err());
        assert!(parse("(table $id funcref)").is_err());
    }

    #[test]
    fn table_with_funcref() {
        let table = parse("(table $id 0 1 funcref)").unwrap();
        assert_eq!(table.id.unwrap(), Identifier::String("$id".into()));
        assert_eq!(table.table_type.limit, Limit::max(0, 1));
        assert_eq!(table.table_type.ref_type, RefType::FuncRef)
    }

    #[test]
    fn table_with_externref() {
        let table = parse("(table $id 0 1 externref)").unwrap();
        assert_eq!(table.id.unwrap(), Identifier::String("$id".into()));
        assert_eq!(table.table_type.limit, Limit::max(0, 1));
        assert_eq!(table.table_type.ref_type, RefType::ExternRef);
    }

    #[test]
    fn table_with_no_max() {
        let table = parse("(table $id 0 externref)").unwrap();
        assert_eq!(table.id.unwrap(), Identifier::String("$id".into()));
        assert_eq!(table.table_type.limit, Limit::min(0));
        assert_eq!(table.table_type.ref_type, RefType::ExternRef);
    }

    #[test]
    fn table_with_no_max_and_id() {
        let table = parse("(table 0 externref)").unwrap();
        assert!(table.id.is_none());
        assert_eq!(table.table_type.limit, Limit::min(0));
        assert_eq!(table.table_type.ref_type, RefType::ExternRef);
    }

    #[test]
    fn table_imported() {
        let table = parse("(table (import \"lib\" \"test\") 0 1 funcref)").unwrap();
        assert!(table.id.is_none());
        assert_eq!(table.table_type.limit, Limit::max(0, 1));
        assert_eq!(table.table_type.ref_type, RefType::FuncRef);
        assert_eq!(table.import.unwrap(), Import::new("lib", "test"));
    }

    #[test]
    fn table_imported_more_then_once() {
        assert!(
            parse("(table (import \"lib\" \"test\") (import \"lib\" \"test\") 0 1 funcref)")
                .is_err()
        );
    }

    #[test]
    fn table_exported() {
        let table = parse("(table (export \"test\") 0 1 funcref)").unwrap();
        assert!(table.id.is_none());
        assert_eq!(table.table_type.limit, Limit::max(0, 1));
        assert_eq!(table.table_type.ref_type, RefType::FuncRef);
        assert_eq!(*table.exports.get(0).unwrap(), Export::new("test"));
    }

    #[test]
    fn table_exported_twice() {
        let table = parse("(table (export \"test\") (export \"test2\") 0 1 funcref)").unwrap();
        assert!(table.id.is_none());
        assert_eq!(table.table_type.limit, Limit::max(0, 1));
        assert_eq!(table.table_type.ref_type, RefType::FuncRef);
        assert_eq!(*table.exports.get(0).unwrap(), Export::new("test"));
        assert_eq!(*table.exports.get(1).unwrap(), Export::new("test2"));
    }
}
