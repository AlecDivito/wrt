use std::convert::TryFrom;

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Identifier {
    String(String),
    Number(usize),
}

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
        block: &Block<'a>,
        description: Option<ExportDescription>,
    ) -> Result<Export> {
        block.expect(BlockType::Export)?;

        let names = block.names()?;
        match names.len() {
            0 => Err(WasmError::err("export does not contain a name")),
            1 => Ok(Self {
                name: names[1].to_owned(),
                description,
            }),
            _ => Err(WasmError::err("export contains to many names")),
        }
    }

    /// Get a reference to the export's name.
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

impl<'a> TryFrom<&Block<'a>> for Export {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Export)?;

        let children = block.children();

        match children.len() {
            0 => Err(WasmError::err(
                "expected a child description, found nothing",
            )),
            1 => {
                let child = children[0];

                let id = match child.try_identity()? {
                    Some(id) => Identifier::String(id),
                    None => match child.content() {
                        Some(id_index) => Identifier::Number(id_index.parse::<usize>()?),
                        None => {
                            return Err(WasmError::err("export id or index could not be found"))
                        }
                    },
                };

                let description = Some(match child.type_id() {
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
                });
                Export::try_from_block_without_description(&block, description)
            }
            _ => Err(WasmError::err(
                "expected a child description, found more then 1",
            )),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::block::SubString;

    use super::*;

    fn parse(program: &str) -> Result<Block> {
        let mut source = SubString::new(program);
        Block::parse(&mut source)
    }

    #[test]
    fn parse_only_export_fails() {
        assert!(parse("(export)").is_err());
    }

    #[test]
    fn parse_export() {
        let block = parse("(export \"test\")").unwrap();
        let export = Export::try_from(&block).unwrap();
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
        let block = parse("(export \"test\" (func 0))").unwrap();
        let export = Export::try_from(&block).unwrap();
        assert_eq!(export.name, "test");
        assert!(export.description.is_some());
        match export.description.unwrap() {
            ExportDescription::Function(id) => assert_eq!(id, Identifier::Number(0)),
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_export_with_func_string_definition() {
        let block = parse("(export \"test\" (func $id))").unwrap();
        let export = Export::try_from(&block).unwrap();
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
