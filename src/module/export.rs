use std::convert::TryFrom;

use crate::{
    block::{BlockType, Identifier},
    error::{Result, WasmError, WrapContext},
    Block,
};

#[derive(Debug, PartialEq)]
pub enum ExportDescription {
    Function(Identifier),
    Table(Identifier),
    Memory(Identifier),
    Global(Identifier),
}

#[derive(Debug, PartialEq)]
pub struct Export {
    name: String,
    description: Option<ExportDescription>,
}

impl Export {
    pub fn new(name: impl Into<String>) -> Export {
        Self {
            name: name.into(),
            description: None,
        }
    }

    pub fn try_from_block_without_description<'a>(
        block: &mut Block<'a>,
        description: Option<ExportDescription>,
    ) -> Result<Export> {
        block.expect(BlockType::Export)?;
        let name = block
            .pop_name()
            .ok_or(WasmError::err("expected name, found none"))?
            .to_string();
        block.should_be_empty()?;
        Ok(Self { name, description })
    }

    /// Get a reference to the export's name.
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

impl<'a> TryFrom<&mut Block<'a>> for Vec<Export> {
    type Error = WasmError;

    fn try_from(block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block
            .take_children_that_are(BlockType::Export)
            .iter_mut()
            .map(|block| Export::try_from_block_without_description(block, None))
            .collect::<Result<Vec<Export>>>()
    }
}

impl<'a> TryFrom<&mut Block<'a>> for Export {
    type Error = WasmError;

    fn try_from(block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Export)?;

        if let Some(mut child) = block.pop_child() {
            let id = child
                .take_id_or_attribute_as_identifier()
                .ok_or(WasmError::err(
                    "export child block requires index identifer",
                ))?;
            let description = match child.type_id() {
                BlockType::Function => ExportDescription::Function(id),
                BlockType::Table => ExportDescription::Table(id),
                BlockType::Memory => ExportDescription::Memory(id),
                BlockType::Global => ExportDescription::Global(id),
                _ => {
                    return Err(WasmError::expected(
                        &[
                            BlockType::Function,
                            BlockType::Table,
                            BlockType::Memory,
                            BlockType::Global,
                        ],
                        child.type_id(),
                    ))
                }
            };
            child.should_be_empty().wrap_context(
                "Expected the child block of export to only contain an id, but found more",
            )?;
            Export::try_from_block_without_description(block, Some(description))
        } else {
            Err(WasmError::err(
                "expected a child description, found nothing",
            ))
        }
    }
}

#[cfg(test)]
mod test {

    use crate::block::SubString;

    use super::*;

    fn parse(program: &str) -> Result<Export> {
        let mut source = SubString::new(program);
        let mut block = Block::parse(&mut source)?;
        Export::try_from(&mut block)
    }

    #[test]
    fn parse_only_export_fails() {
        assert!(parse("(export)").is_err());
    }

    #[test]
    fn parse_export() {
        let export = parse("(export \"test\")").unwrap();
        assert_eq!(export.name, "test");
    }

    #[test]
    fn parse_export_with_too_many_arguments_fails() {
        assert!(parse("(export \"lib\" \"test\")").is_err());
    }

    #[test]
    fn parse_export_with_func_without_identifier() {
        assert!(parse("(export \"test\" (func))").is_err());
    }

    #[test]
    fn parse_export_with_func_usize_definition() {
        let export = parse("(export \"test\" (func 0))").unwrap();
        assert_eq!(export.name, "test");
        assert!(export.description.is_some());
        match export.description.unwrap() {
            ExportDescription::Function(id) => assert_eq!(id, Identifier::Number(0)),
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_export_with_func_string_definition() {
        let export = parse("(export \"test\" (func $id))").unwrap();
        assert_eq!(export.name, "test");
        assert!(export.description.is_some());
        match export.description.unwrap() {
            ExportDescription::Function(id) => {
                assert_eq!(id, Identifier::String("$id".to_string()))
            }
            _ => assert!(false),
        }
    }
}
