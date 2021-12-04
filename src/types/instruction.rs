use std::str::FromStr;

use crate::{block::SubString, error::WasmError};

#[derive(Debug)]
pub enum Instruction {
    // ?
    Call(String),
    Return,

    // ?
    LocalGet(usize),

    // i32 operations
    I32Add,
    I32Const(i32),
}

impl FromStr for Instruction {
    type Err = WasmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimed = s.trim();
        let mut source = SubString::new(trimed);
        if let Some(token) = source.eat_instruction() {
            match token {
                "call" => {
                    let id = source
                        .eat_identifier()
                        .ok_or(WasmError::err("expected argument of function id"))?;
                    Ok(Instruction::Call(id.into()))
                }
                "return" => Ok(Instruction::Return),

                "local.get" => {
                    let index = source
                        .eat_numeric()
                        .ok_or(WasmError::err("expected argument"))?;
                    let arg = index.parse()?;
                    Ok(Instruction::LocalGet(arg))
                }

                "i32.add" => Ok(Instruction::I32Add),
                "i32.const" => {
                    let number = source
                        .eat_numeric()
                        .ok_or(WasmError::err("expected argument"))?;
                    let arg = i32::from_str(number)
                        .map_err(|_| WasmError::err("expected i32 but did not find it"))?;
                    Ok(Instruction::I32Const(arg))
                }

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
