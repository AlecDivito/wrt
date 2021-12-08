use std::{error::Error, fmt::Display, num::ParseIntError};

use crate::{
    block::{Block, BlockType},
    values::value::ValueType,
};

pub trait WrapError<T, D: Display + Send + Sync + 'static> {
    fn wrap_err(self, t: D) -> Self;
}

pub trait WrapContext<T, D: Display + Send + Sync + 'static> {
    fn wrap_context(self, t: D) -> Self;
}

pub type Result<T> = std::result::Result<T, WasmError>;

#[derive(Debug)]
pub struct WasmError {
    reason: String,
    context: Vec<String>,
    errors: Vec<String>,
}

impl Error for WasmError {}

impl WasmError {
    fn new(reason: impl Into<String>) -> Self {
        Self {
            reason: reason.into(),
            context: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn err(reason: impl Into<String>) -> Self {
        Self::new(reason)
    }

    pub fn character(character: char) -> Self {
        Self::new(format!("unexpected character found: {}", character))
    }

    pub fn expect(expected: BlockType, found: &BlockType) -> WasmError {
        Self::new(format!("expected: {} found: {}", expected, found))
    }

    pub fn expected(expected: &[BlockType], found: &BlockType) -> WasmError {
        let types = expected
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        Self::new(format!("expected: {}; found: {}", types, found))
    }

    pub fn expected_type(expected: ValueType, found: ValueType) -> WasmError {
        Self::new(format!("expected: {}; found: {}", expected, found))
    }

    pub fn block_not_empty(block: &Block) -> WasmError {
        let mut err = Self::new("expected block to be empty, found that it still contained values");
        err.context
            .push(format!("Remaining Items in Block: {}", block));
        err
    }

    pub fn eof() -> WasmError {
        Self::new("end of file reached")
    }

    pub(crate) fn context_len(&self) -> usize {
        self.context.len()
    }
}

impl Display for WasmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}\n", self.reason)?;
        if !self.errors.is_empty() {
            write!(f, "\nCaused by:\n")?;
            for err in &self.errors {
                write!(f, "\t{}\n", err)?;
            }
        }
        if !self.context.is_empty() {
            write!(f, "\nWith Context:\n")?;
            for err in &self.context {
                write!(f, "\t{}\n", err)?;
            }
        }
        write!(f, "\n")
    }
}

impl<T, D: Display + Send + Sync + 'static> WrapContext<T, D> for Result<T> {
    fn wrap_context(self, t: D) -> Self {
        if let Err(mut err) = self {
            err.context.push(t.to_string());
            Err(err)
        } else {
            self
        }
    }
}

impl<T, D: Display + Send + Sync + 'static> WrapError<T, D> for Result<T> {
    fn wrap_err(self, t: D) -> Self {
        if let Err(mut err) = self {
            err.errors.push(t.to_string());
            Err(err)
        } else {
            self
        }
    }
}

impl From<ParseIntError> for WasmError {
    fn from(e: ParseIntError) -> Self {
        WasmError::err(format!("failed to parse int with error: {}", e))
    }
}
