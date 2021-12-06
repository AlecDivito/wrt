use std::convert::{TryFrom, TryInto};

use crate::{
    block::{Block, BlockType},
    error::{Result, WasmError},
};

use super::function::Function;
use super::global::Global;

#[derive(Debug)]
pub enum ImportDefinition {
    Function(Function),
    // Global(Global),
}

#[derive(Debug, PartialEq)]
pub struct Import {
    module: String,
    name: String,
}

impl<'a> TryFrom<&Block<'a>> for Import {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        if BlockType::Import != *block.type_id() {
            return Err(WasmError::err(format!(
                "expected import, found {}",
                block.type_id()
            )));
        }

        let names = block.variable_name();
        let module = names
            .get(0)
            .ok_or(WasmError::err("module name not found in import"))?
            .to_string();
        let name = names
            .get(1)
            .ok_or(WasmError::err("module item name not found in import"))?
            .to_string();
        Ok(Self { module, name })
    }
}

impl Import {
    pub fn new(module: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            module: module.into(),
            name: name.into(),
        }
    }

    pub fn build(block: &Block) -> Result<ImportDefinition> {
        let import = block.try_into()?;

        //TODO(Alec): Assuming there is only one import
        let child = block.children().get(0).ok_or(WasmError::err(
            "exported import block to include a definition",
        ))?;

        match child.type_id() {
            BlockType::Function => Ok(ImportDefinition::Function(Function::import_block(
                child, import,
            )?)),
            BlockType::Global => todo!("we aren't implementing the block just yet"),
            _ => Err(WasmError::err(format!(
                "block {} is not supported in imports",
                child.type_id()
            ))),
        }
    }

    pub fn module(&self) -> &str {
        &self.module
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
