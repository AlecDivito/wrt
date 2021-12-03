use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

use super::{module::Module, Identifier};

#[derive(Debug)]
pub enum ExportType {
    Function(Identifier),
}

#[derive(Debug)]
pub struct Export {
    id: Identifier,
    export: ExportType,
}

impl Export {
    pub fn build(module: &Module, block: &Block) -> Result<Self> {
        if let BlockType::Export = block.type_id() {
            // we require that the export block has an identifer
            let id = block.variable_name().ok_or(WasmError::err(format!(
                "exported type required an identifier. None found"
            )))?;
            // we require that the export block exports something
            let child = block.children().get(0).ok_or(WasmError::err(format!(
                "expected module, found {}",
                block.type_id()
            )))?;

            let identifier = child.variable_name().ok_or(WasmError::err(format!(
                "exported type requires an identifier, none found"
            )))?;

            match child.type_id() {
                BlockType::Function => {
                    if let Some(func) = module.function(&Identifier::String(identifier.into())) {
                        Ok(Self {
                            id: Identifier::String(id.into()),
                            export: ExportType::Function(func.identifier()),
                        })
                    } else {
                        Err(WasmError::err(format!(
                            "exported type function could not be found with identifier {}",
                            identifier
                        )))
                    }
                }
                _ => Err(WasmError::err(format!(
                    "support for exporting type {} is not currently supported",
                    child.type_id()
                ))),
            }
        } else {
            Err(WasmError::err(format!(
                "expected module, found {}",
                block.type_id()
            )))
        }
    }

    pub fn export_type(&self) -> &ExportType {
        &self.export
    }
}
