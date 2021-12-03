use std::{fmt::Display, str::FromStr};

use crate::error::WasmError;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            ValueType::I32(i) => format!("{}", i),
            ValueType::I64(i) => format!("{}", i),
            ValueType::F32(i) => format!("{}", i),
            ValueType::F64(i) => format!("{}", i),
        };
        write!(f, "{}", content)
    }
}

impl FromStr for ValueType {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "i32" => Ok(ValueType::I32(0)),
            "i64" => Ok(ValueType::I64(0)),
            "f32" => Ok(ValueType::F32(0.0)),
            "f64" => Ok(ValueType::F64(0.0)),
            _ => Err(WasmError::err(format!("did not expect type '{}'", s))),
        }
    }
}
impl ValueType {
    pub fn same_stack_types(p1: &[ValueType], p2: &[ValueType]) -> bool {
        if p1.len() != p2.len() {
            false
        } else {
            for (i1, i2) in p1.iter().zip(p2) {
                //TODO(Alec): This should return an error so we can let the user know
                if !i1.type_equality(i2) {
                    return false;
                }
            }
            true
        }
    }

    pub fn type_equality(&self, other: &ValueType) -> bool {
        let mut result = false;
        match self {
            ValueType::I32(_) => {
                if let ValueType::I32(_) = other {
                    result = true
                }
            }
            ValueType::I64(_) => {
                if let ValueType::I64(_) = other {
                    result = true
                }
            }
            ValueType::F32(_) => {
                if let ValueType::F32(_) = other {
                    result = true
                }
            }
            ValueType::F64(_) => {
                if let ValueType::F64(_) = other {
                    result = true
                }
            }
        };
        result
    }

    pub fn value_equality(&self, other: &ValueType) -> bool {
        let mut result = false;
        match self {
            ValueType::I32(i1) => {
                if let ValueType::I32(i2) = other {
                    result = *i1 == *i2
                }
            }
            ValueType::I64(i1) => {
                if let ValueType::I64(i2) = other {
                    result = *i1 == *i2
                }
            }
            ValueType::F32(i1) => {
                if let ValueType::F32(i2) = other {
                    result = *i1 == *i2
                }
            }
            ValueType::F64(i1) => {
                if let ValueType::F64(i2) = other {
                    result = *i1 == *i2
                }
            }
        };
        result
    }

    pub(crate) fn add(&self, other: ValueType) -> crate::error::Result<ValueType> {
        match self {
            ValueType::I32(i1) => {
                if let ValueType::I32(i2) = other {
                    Ok(ValueType::I32(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
            ValueType::I64(i1) => {
                if let ValueType::I64(i2) = other {
                    Ok(ValueType::I64(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
            ValueType::F32(i1) => {
                if let ValueType::F32(i2) = other {
                    Ok(ValueType::F32(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
            ValueType::F64(i1) => {
                if let ValueType::F64(i2) = other {
                    Ok(ValueType::F64(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
        }
    }
}
