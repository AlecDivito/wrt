use crate::{
    block::{Block, BlockType},
    error::{Result, WasmError},
};

use super::{function::FunctionType, parameter::Parameter, Identifier};

#[derive(Debug)]
pub enum ImportType {
    Function(FunctionType),
}

#[derive(Debug)]
pub struct Import {
    module: String,
    name: String,
    import_type_id: Identifier,
    import_type: ImportType,
}

impl Import {
    pub fn build(block: &Block) -> Result<Import> {
        if BlockType::Import != *block.type_id() {
            return Err(WasmError::err(format!(
                "expected import, found {}",
                block.type_id()
            )));
        }

        let names = block.variable_name();
        let module = names
            .get(0)
            .ok_or(WasmError::err("module name not found in import"))?;

        let name = names
            .get(1)
            .ok_or(WasmError::err("module item name not found in import"))?;

        //TODO(Alec): Assuming there is only one import
        let child = block.children().get(0).ok_or(WasmError::err(
            "exported import block to include a definition",
        ))?;

        let import_type_id =
            Identifier::String(child.variable_name().get(0).unwrap_or(&"").to_string());

        let import_type = match child.type_id() {
            BlockType::Function => {
                let mut func = FunctionType::new();
                for arg in child.children() {
                    let param = Parameter::build(arg)?;
                    match arg.type_id() {
                        BlockType::Parameter => func.add_param_type(param),
                        BlockType::Result => func.add_result_type(param),
                        _ => {
                            return Err(WasmError::err(format!(
                                "block {} is not supported in imported functions types",
                                child.type_id()
                            )))
                        }
                    }
                }
                ImportType::Function(func)
            }
            _ => {
                return Err(WasmError::err(format!(
                    "block {} is not supported in imports",
                    child.type_id()
                )))
            }
        };

        Ok(Self {
            module: module.to_string(),
            name: name.to_string(),
            import_type_id,
            import_type,
        })
    }

    pub(crate) fn module(&self) -> String {
        self.module.clone()
    }

    pub(crate) fn name(&self) -> String {
        self.name.clone()
    }
}
