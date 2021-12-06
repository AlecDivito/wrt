use std::{convert::TryFrom, fmt::Display};

use crate::{
    block::{Block, BlockType},
    error::{Result, WasmError},
    values::func::FunctionType,
};

#[derive(Debug)]
pub enum ImportDescription {
    Function(FunctionType),
    // Global(Global),
}

impl Display for ImportDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportDescription::Function(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub struct Import {
    module: String,
    name: String,
    description_id: Option<String>,
    description: Option<ImportDescription>,
}

impl Import {
    pub fn try_from_block_without_description<'a>(
        block: &Block<'a>,
        description_id: Option<String>,
        description: Option<ImportDescription>,
    ) -> Result<Import> {
        block.expect(BlockType::Import)?;
        let names = block.names()?;

        match names.len() {
            0 => Err(WasmError::err("import does not contain module and name")),
            1 => Err(WasmError::err("import does not contain name")),
            2 => Ok(Self {
                module: names[0].to_owned(),
                name: names[1].to_owned(),
                description_id,
                description,
            }),
            _ => Err(WasmError::err("import contains to many names")),
        }
    }

    /// Get a reference to the import's module.
    pub fn module(&self) -> &str {
        self.module.as_ref()
    }

    /// Get a reference to the import's name.
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

impl<'a> TryFrom<&Block<'a>> for Import {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Import)?;

        let children = block.children();

        match children.len() {
            0 => Err(WasmError::err(
                "expected a child description, found nothing",
            )),
            1 => {
                let child = children[0];
                let description_id = child.try_identity()?;
                let description = Some(match child.type_id() {
                    BlockType::Function => {
                        ImportDescription::Function(FunctionType::try_from(&child)?)
                    }
                    _ => return Err(WasmError::err("import does not contain description")),
                });
                Import::try_from_block_without_description(&block, description_id, description)
            }
            _ => Err(WasmError::err(
                "expected a child description, found more then 1",
            )),
        }
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(desc) = self.description {
            write!(f, "(module {} {} {})", self.module, self.name, desc)
        } else {
            write!(f, "(module {} {})", self.module, self.name)
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
    fn parse_only_import_fails() {
        assert!(parse("(import)").is_err());
    }

    #[test]
    fn parse_import_with_only_module_fails() {
        assert!(parse("(import \"lib\")").is_err());
    }

    #[test]
    fn parse_import() {
        let block = parse("(import \"lib\" \"test\")").unwrap();
        let import = Import::try_from(&block).unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.module, "test");
    }

    #[test]
    fn parse_import_with_too_many_arguments_fails() {
        assert!(parse("(import \"lib\" \"test\" \"test\")").is_err());
    }

    #[test]
    fn parse_import_with_func_definition() {
        let block = parse("(import \"lib\" \"test\" (func))").unwrap();
        let import = Import::try_from(&block).unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.module, "test");
        assert!(import.description_id.is_none());
        match import.description.unwrap() {
            ImportDescription::Function(_) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_import_with_func_definition_including_id() {
        let block = parse("(import \"lib\" \"test\" (func $id))").unwrap();
        let import = Import::try_from(&block).unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.module, "test");
        assert_eq!(import.description_id.unwrap(), "$id");
        match import.description.unwrap() {
            ImportDescription::Function(_) => assert!(true),
            _ => assert!(false),
        }
    }
}
