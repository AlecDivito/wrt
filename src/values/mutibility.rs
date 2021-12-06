use std::fmt::Display;
use std::str::FromStr;

use crate::error::Result;
use crate::error::WasmError;

use super::value::ValueType;

#[derive(Debug, Clone, PartialEq)]
pub enum Mutibility {
    Const(ValueType),
    Mut(ValueType),
}

impl Mutibility {
    pub fn set_value(self, value: &str) -> Result<Mutibility> {
        match self {
            Mutibility::Const(_) => Err(WasmError::err(
                "it is not possible to set the value of a const variable",
            )),
            Mutibility::Mut(v) => Ok(Mutibility::Mut(v.set_value(value)?)),
        }
    }
}

impl Display for Mutibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mutibility::Const(i) => write!(f, "{}", i),
            Mutibility::Mut(i) => write!(f, "(mut {})", i),
        }
    }
}

impl FromStr for Mutibility {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut splits = s.trim().split(" ");
        let is_mut = splits.next().ok_or(WasmError::err("expected type"))?;

        Ok(if is_mut == "mut" {
            let value_type = splits
                .next()
                .ok_or(WasmError::err("need the actual value"))?;
            Mutibility::Const(ValueType::from_str(value_type)?)
        } else {
            let value_type = is_mut;
            Mutibility::Const(ValueType::from_str(value_type)?)
        })
    }
}
