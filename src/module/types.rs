use std::{convert::TryFrom, fmt::Display};

use crate::{
    block::{Block, BlockType, Identifier},
    error::WasmError,
    values::func::FunctionType,
};

#[derive(Debug)]
pub struct TypeIdentifier {
    id: Option<Identifier>,
    func_type: FunctionType,
}

impl<'a> TryFrom<&'a mut Block<'a>> for TypeIdentifier {
    type Error = WasmError;

    fn try_from(block: &'a mut Block<'a>) -> Result<Self, Self::Error> {
        block.expect(BlockType::Type)?;

        let id = if let Some(id) = block.take_id() {
            Some(id)
        } else if block.attribute_length() >= 1 {
            Some(block.pop_attribute_as_identifier()?)
        } else {
            None
        };

        let mut child = block.pop_child().ok_or(WasmError::err(
            "expected 1 child for type block; found none",
        ))?;
        let func_type = FunctionType::try_from(&mut child)?;

        block.should_be_empty()?;
        Ok(Self { id, func_type })
    }
}

impl Display for TypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = if let Some(id) = &self.id {
            format!("{} {}", id, self.func_type)
        } else {
            self.func_type.to_string()
        };
        write!(f, "(type {})", content)
    }
}

#[cfg(test)]
mod test {

    use crate::block::SubString;
    use crate::error::Result;

    use super::*;

    fn parse(program: &str) -> Result<Block> {
        let mut source = SubString::new(program);
        Block::parse(&mut source)
    }

    #[test]
    fn parse_only_type_identifier_fails() {
        assert!(parse("(type)").is_err());
    }

    #[test]
    fn parse_type_identifier_with_mulitple_ids_fails() {
        assert!(parse("(type $id1 $id2)").is_err());
    }

    #[test]
    fn parse_valid_type_identifier() {
        let mut block = parse("(type (func))").unwrap();
        let typeid = TypeIdentifier::try_from(&mut block).unwrap();
        assert!(typeid.id.is_none());
        assert!(typeid.func_type.parameters().is_empty());
        assert!(typeid.func_type.results().is_empty());
    }

    #[test]
    fn parse_valid_type_identifier_with_id() {
        let mut block = parse("(type $id (func))").unwrap();
        let typeid = TypeIdentifier::try_from(&mut block).unwrap();
        assert_eq!(typeid.id.unwrap(), Identifier::String("$id".to_string()));
        assert!(typeid.func_type.parameters().is_empty());
        assert!(typeid.func_type.results().is_empty());
    }
}
