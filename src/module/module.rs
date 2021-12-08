use std::convert::TryFrom;

use crate::{block::BlockType, error::WasmError, Block};

use super::{export::Export, function::Function, import::Import, types::TypeIdentifier};

#[derive(Debug)]
pub struct Module {
    types: Vec<TypeIdentifier>,
    // tables: Vec<Table>
    // memories: Vec<Memory>
    // globals: Vec<Global>,
    // elements: Vec<Element>
    // datas: Vec<Data>
    // start: Option<Start>
    exports: Vec<Export>,
    imports: Vec<Import>,
    funcs: Vec<Function>,
}

impl<'a> TryFrom<Block<'a>> for Module {
    type Error = WasmError;

    fn try_from(mut block: Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Module)?;

        let mut types = vec![];
        let mut funcs = vec![];
        let mut imports = vec![];
        let mut exports = vec![];

        while let Some(mut child) = block.pop_child() {
            match child.type_id() {
                BlockType::Type => types.push(TypeIdentifier::try_from(&mut child)?),
                BlockType::Function => funcs.push(Function::try_from(&mut child)?),
                BlockType::Import => imports.push(Import::try_from(&mut child)?),
                BlockType::Export => exports.push(Export::try_from(&mut child)?),
                _ => return Err(WasmError::expected(&[BlockType::Type], &child.type_id())),
            }
        }

        Ok(Self {
            types,
            funcs,
            imports,
            exports,
        })
    }
}

impl Module {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            funcs: Vec::new(),
            imports: Vec::new(),
            exports: Vec::new(),
        }
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
