use crate::{
    block::{Block, BlockType},
    error::{Result, WasmError},
};

use super::function::Function;

#[derive(Debug)]
pub enum ImportDefinition {
    Function(Function),
}

#[derive(Debug)]
pub struct Import {
    module: String,
    name: String,
}

impl Import {
    pub fn build(block: &Block) -> Result<ImportDefinition> {
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
        let import = Self { module, name };

        //TODO(Alec): Assuming there is only one import
        let child = block.children().get(0).ok_or(WasmError::err(
            "exported import block to include a definition",
        ))?;

        match child.type_id() {
            BlockType::Function => Ok(ImportDefinition::Function(Function::import_block(
                child, import,
            )?)),
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
