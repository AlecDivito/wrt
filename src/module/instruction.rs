use std::{fmt::Display, str::FromStr};

use crate::{
    block::{Identifier, SubString},
    error::{WasmError, WrapError},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // ?
    Call(Identifier),
    Return,

    // ?
    LocalGet(Identifier),

    // i32 operations
    I32Add,
    I32Const(i32),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            Call(v) => write!(f, "(call {})", v),
            Return => write!(f, "(return)"),
            LocalGet(v) => write!(f, "(local.get {})", v),
            I32Add => write!(f, "(i32.add)"),
            I32Const(v) => write!(f, "(i32.const {})", v),
        }
    }
}

impl FromStr for Instruction {
    type Err = WasmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimed = s.trim();
        let mut source = SubString::new(trimed);
        if let Some(token) = source.eat_instruction()? {
            match token {
                "call" => {
                    let id = source
                        .eat_identifier()
                        .wrap_err(WasmError::err("expected argument of function id"))?;
                    Ok(Instruction::Call(id))
                }
                "return" => Ok(Instruction::Return),

                "local.get" => {
                    let id = source
                        .eat_identifier()
                        .wrap_err(WasmError::err("expected argument of function id"))?;
                    Ok(Instruction::LocalGet(id))
                }

                "i32.add" => Ok(Instruction::I32Add),
                "i32.const" => {
                    let number = source
                        .eat_numeric()?
                        .ok_or(WasmError::err("expected numeric argument"))?;
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
