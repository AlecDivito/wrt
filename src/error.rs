use std::{error::Error, fmt::Display, num::ParseIntError};

use crate::{
    block::{Block, BlockType},
    values::value::ValueType,
};

#[derive(Debug)]
pub struct WasmError {
    reason: String,
}

impl WasmError {
    pub fn err(reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
        }
    }

    pub fn expect(expected: BlockType, found: &BlockType) -> WasmError {
        Self {
            reason: format!("expected: {} found: {}", expected, found),
        }
    }

    pub fn expected(expected: &[BlockType], found: &BlockType) -> WasmError {
        let types = expected
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        Self {
            reason: format!("expected: {}; found: {}", types, found),
        }
    }

    pub fn expected_type(expected: ValueType, found: ValueType) -> WasmError {
        Self {
            reason: format!("expected: {}; found: {}", expected, found),
        }
    }

    pub fn block_not_empty(block: &Block) -> WasmError {
        Self {
            reason: format!("expected block to be empty, found the following: {}", block),
        }
    }
}

impl From<ParseIntError> for WasmError {
    fn from(e: ParseIntError) -> Self {
        WasmError::err(format!("failed to parse int with error: {}", e))
    }
}

impl Error for WasmError {}

impl Display for WasmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.reason)
    }
}

pub type Result<T> = std::result::Result<T, WasmError>;
