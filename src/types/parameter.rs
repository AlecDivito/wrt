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
    value: ValueType,
    name: Option<String>,
}

impl Parameter {
    pub fn build(block: &Block) -> Result<Parameter> {
        let allowed_blocks = [BlockType::Parameter, BlockType::Result, BlockType::Local];
        if allowed_blocks.contains(block.type_id()) {
            if let Some(content) = block.content() {
                let name = if let Some(s) = block.variable_name() {
                    Some(s.to_string())
                } else {
                    None
                };
                let value = ValueType::from_str(content)?;
                Ok(Self {
                    id_type: block.type_id().clone(),
                    name,
                    value,
                })
            } else {
                Err(WasmError::err(format!(
                    "expected argument describing value, nothing instead"
                )))
            }
        } else {
            Err(WasmError::err(format!(
                "expected block types of {:?}, found {}",
                allowed_blocks,
                block.type_id()
            )))
        }
    }

    pub fn type_id(&self) -> BlockType {
        self.id_type.clone()
    }

    pub fn name(&self) -> Option<String> {
        self.name.clone()
    }

    pub fn value(&self) -> ValueType {
        self.value.clone()
    }
}
