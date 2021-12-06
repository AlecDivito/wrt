use std::{convert::TryFrom, fmt::Display};

use crate::{
    block::{Block, BlockType},
    error::WasmError,
};

use super::value::ValueType;

pub struct Param {
    id: Option<String>,
    value_type: Vec<ValueType>,
}

impl<'a> TryFrom<&Block<'a>> for Param {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Result)?;
        let id = block.try_identity()?;
        let value_type = match id {
            Some(_) => vec![block.value_type()?],
            None => block.value_types()?,
        };
        Ok(Self { id, value_type })
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = if self.value_type.len() == 1 {
            let value_type = self.value_type.pop().unwrap().type_id_string();
            if let Some(id) = self.id {
                format!("{} {}", id, value_type)
            } else {
                value_type
            }
        } else {
            self.value_type
                .iter()
                .map(|v| v.type_id_string())
                .collect::<Vec<String>>()
                .join(" ")
        };
        write!(f, "(result {})", content)
    }
}
pub struct Result {
    value_type: Vec<ValueType>,
}

impl<'a> TryFrom<&Block<'a>> for Result {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Result)?;
        let value_type = block.value_types()?;
        Ok(Self { value_type })
    }
}

impl Display for Result {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let types = self
            .value_type
            .iter()
            .map(|v| v.type_id_string())
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "(result {})", types)
    }
}

pub struct FunctionType {
    parameters: Vec<Param>,
    results: Vec<Result>,
}

impl<'a> TryFrom<&Block<'a>> for FunctionType {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Function)?;

        let mut parameters = Vec::new();
        let mut results = Vec::new();

        for child in block.children() {
            match child.type_id() {
                BlockType::Parameter => parameters.push(Param::try_from(child)?),
                BlockType::Result => results.push(Result::try_from(child)?),
                _ => {
                    return Err(WasmError::expected(
                        &[BlockType::Parameter, BlockType::Result],
                        child.type_id(),
                    ))
                }
            }
        }

        Ok(Self {
            parameters,
            results,
        })
    }
}

fn to_string<F: Display>(m: &Vec<F>) -> String {
    m.iter()
        .map(|b| b.to_string())
        .collect::<Vec<String>>()
        .join(" ")
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(func {} {})",
            to_string(&self.parameters),
            to_string(&self.results)
        )
    }
}
