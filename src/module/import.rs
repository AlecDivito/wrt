use std::{convert::TryFrom, fmt::Display};

use crate::{
    block::{Block, BlockType, Identifier},
    error::{Result, WasmError},
    values::func::FunctionType,
};

#[derive(Debug, PartialEq)]
pub enum ImportDescription {
    Function(FunctionType),
    // Table(Table),
    // Memory(Memory),
    // Global(Global),
}

impl Display for ImportDescription {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportDescription::Function(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Import {
    module: String,
    name: String,
    description_id: Option<Identifier>,
    description: Option<ImportDescription>,
}

impl Import {
    pub fn new(module: impl Into<String>, name: impl Into<String>) -> Import {
        Self {
            module: module.into(),
            name: name.into(),
            description_id: None,
            description: None,
        }
    }

    pub fn try_from_block_without_description<'a>(
        block: &mut Block<'a>,
        id: Option<Identifier>,
        description: Option<ImportDescription>,
    ) -> Result<Import> {
        block.expect(BlockType::Import)?;
        let name = block
            .pop_name()
            .ok_or(WasmError::err(
                "expected module and name on import block. Found none",
            ))?
            .to_string();
        let module = block
            .pop_name()
            .ok_or(WasmError::err(
                "expected name on import block. Found only module",
            ))?
            .to_string();
        block.should_be_empty()?;
        Ok(Self {
            name,
            module,
            description_id: id,
            description: description,
        })
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

impl<'a> TryFrom<&mut Block<'a>> for Option<Import> {
    type Error = WasmError;

    fn try_from(block: &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        let mut imports = block
            .take_children_that_are(BlockType::Import)
            .iter_mut()
            .map(|block| Import::try_from_block_without_description(block, None, None))
            .collect::<Result<Vec<Import>>>()?;
        match imports.pop() {
            Some(import) => {
                if imports.is_empty() {
                    Ok(Some(import))
                } else {
                    Err(WasmError::err(format!(
                        "expected at most 1 import, found {}",
                        imports.len() + 1
                    )))
                }
            }
            None => Ok(None),
        }
    }
}

impl<'a> TryFrom<&'a mut Block<'a>> for Import {
    type Error = WasmError;

    fn try_from(block: &'a mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Import)?;

        if let Some(mut child) = block.pop_child() {
            let id = child.take_id_or_attribute_as_identifier();
            let description = match child.type_id() {
                BlockType::Function => {
                    ImportDescription::Function(FunctionType::try_from(&mut child)?)
                }
                _ => return Err(WasmError::err("import does not contain description")),
            };
            Import::try_from_block_without_description(block, id, Some(description))
        } else {
            Err(WasmError::err(
                "expected a child description, found nothing",
            ))
        }
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(desc) = &self.description {
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

    fn parse(program: &str) -> Result<Import> {
        let mut source = SubString::new(program);
        let mut block = Block::parse(&mut source)?;
        Import::try_from(&mut block)
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
        let import = parse("(import \"lib\" \"test\")").unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.name, "test");
    }

    #[test]
    fn parse_import_with_too_many_arguments_fails() {
        assert!(parse("(import \"lib\" \"test\" \"test\")").is_err());
    }

    #[test]
    fn parse_import_with_func_definition() {
        let import = parse("(import \"lib\" \"test\" (func))").unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.name, "test");
        assert!(import.description_id.is_none());
        match import.description.unwrap() {
            ImportDescription::Function(_) => {
                assert!(true)
            } // _ => assert!(false),
        }
    }

    #[test]
    fn parse_import_with_func_definition_including_id() {
        let import = parse("(import \"lib\" \"test\" (func $id))").unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.name, "test");
        assert_eq!(
            import.description_id.unwrap(),
            Identifier::String("$id".into())
        );
        match import.description.unwrap() {
            ImportDescription::Function(_) => assert!(true),
            // _ => assert!(false),
        }
    }

    #[test]
    fn parse_import_with_func_definition_including_number_id() {
        let import = parse("(import \"lib\" \"test\" (func 0))").unwrap();
        assert_eq!(import.module, "lib");
        assert_eq!(import.name, "test");
        assert_eq!(import.description_id.unwrap(), Identifier::Number(0));
        match import.description.unwrap() {
            ImportDescription::Function(_) => assert!(true),
            // _ => assert!(false),
        }
    }
}
