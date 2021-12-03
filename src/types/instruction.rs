use std::{panic, str::FromStr};

use super::function::Frame;
use crate::{block::SubString, error::WasmError};

#[derive(Debug)]
pub enum Instruction {
    LocalGet(usize),
    I32Add,
}

impl FromStr for Instruction {
    type Err = WasmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimed = s.trim();
        let mut source = SubString::new(trimed);
        if let Some(token) = source.eat_instruction() {
            match token {
                "local.get" => {
                    let index = source
                        .eat_numeric()
                        .ok_or(WasmError::err("expected argument"))?;
                    let arg = index.parse()?;
                    Ok(Instruction::LocalGet(arg))
                }

                "i32.add" => Ok(Instruction::I32Add),

                _ => Err(WasmError::err(format!(
                    "instruction {} doesn't exist",
                    token
                ))),
            }
        } else {
            Err(WasmError::err(format!(
                "Did not find a instruction in {}",
                s
            )))
        }
    }
}
