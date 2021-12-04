use std::str::FromStr;

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

use super::value::ValueType;

#[derive(Debug)]
pub struct Parameter {
    id_type: BlockType,
    value: Option<ValueType>,
    id: Option<String>,
}

impl Parameter {
    pub fn build(block: &Block) -> Result<Parameter> {
        use BlockType::*;

        match block.type_id() {
            Parameter | Local | Result => {
                let content = block.content().ok_or(WasmError::err(
                    "expected argument with value, found nothing",
                ))?;
                let id = block
                    .variable_name()
                    .get(0)
                    .and_then(|s| Some(s.to_string()));
                let value = Some(ValueType::from_str(content)?);
                Ok(Self {
                    id_type: block.type_id().clone(),
                    id,
                    value,
                })
            }
            Export => {
                let id = block
                    .variable_name()
                    .get(0)
                    .ok_or(WasmError::err("export parameter requires a name assigned"))?;
                Ok(Self {
                    id_type: block.type_id().clone(),
                    value: None,
                    id: Some(id.to_string()),
                })
            }
            _ => Err(WasmError::err(format!(
                "expected block types of {:?}, found {}",
                &[Parameter, Local, Result, Export],
                block.type_id()
            ))),
        }
    }

    pub fn type_id(&self) -> BlockType {
        self.id_type.clone()
    }

    pub fn id(&self) -> Option<String> {
        self.id.clone()
    }

    pub fn value(&self) -> Option<ValueType> {
        self.value.clone()
    }
}
