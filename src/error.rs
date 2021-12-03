use std::{error::Error, fmt::Display, num::ParseIntError, str::FromStr};

#[derive(Debug)]
pub struct WasmError {
    line: usize,
    character: usize,
    reason: String,
}

impl WasmError {
    pub fn new(line: usize, character: usize, reason: impl Into<String>) -> Self {
        Self {
            line,
            character,
            reason: reason.into(),
        }
    }

    pub fn err(reason: impl Into<String>) -> Self {
        Self {
            line: 0,
            character: 0,
            reason: reason.into(),
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
        write!(
            f,
            "Error found on line {} on character {}",
            self.line, self.character
        )
    }
}

pub type Result<T> = std::result::Result<T, WasmError>;
