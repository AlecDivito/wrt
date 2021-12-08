use std::convert::TryFrom;

use crate::{block::BlockType, error::WasmError, Block};

use super::types::TypeIdentifier;

#[derive(Debug)]
pub struct Module {
    types: Vec<TypeIdentifier>,
    // funcs: Vec<Func>
    // tables: Vec<Table>
    // memories: Vec<Memory>
    // globals: Vec<Global>,
    // elements: Vec<Element>
    // datas: Vec<Data>
    // start: Option<Start>
    // exports: Vec<Export>,
    // functions: Vec<Function>,
}

impl<'a> TryFrom<Block<'a>> for Module {
    type Error = WasmError;

    fn try_from(mut block: Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Module)?;

        let mut types = vec![];

        while let Some(mut child) = block.pop_child() {
            match child.type_id() {
                BlockType::Type => types.push(TypeIdentifier::try_from(&mut child)?),
                _ => return Err(WasmError::expected(&[BlockType::Type], &child.type_id())),
            }
        }

        Ok(Self { types })
    }
}

impl Module {
    pub fn new() -> Self {
        Self { types: Vec::new() }
    }

    // pub fn function(&self, id: Identifier) -> Option<&Function> {
    //     for func in &self.functions {
    //         if func.identifier() == *id {
    //             return Some(func);
    //         }
    //     }
    //     None
    // }

    // pub fn export(&self, name: impl Into<String>) -> Option<&Export> {
    //     self.exports.get(&Identifier::String(name.into()))
    // }
}
