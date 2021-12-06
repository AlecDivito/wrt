use std::{fmt::Display, str::FromStr};

use crate::error::WasmError;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Number(NumberType),
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            ValueType::I32(i) => format!("i32 {}", i),
            ValueType::I64(i) => format!("i64 {}", i),
            ValueType::F32(i) => format!("f32 {}", i),
            ValueType::F64(i) => format!("f64 {}", i),
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
    pub fn set_value(self, value: &str) -> crate::error::Result<ValueType> {
        let err = WasmError::err(format!(
            "value defined '{}' does not match defined type of {}",
            value, self
        ));
        match self {
            ValueType::I32(_) => Ok(ValueType::I32(i32::from_str(value).map_err(|_| err)?)),
            ValueType::I64(_) => Ok(ValueType::I64(i64::from_str(value).map_err(|_| err)?)),
            ValueType::F32(_) => Ok(ValueType::F32(f32::from_str(value).map_err(|_| err)?)),
            ValueType::F64(_) => Ok(ValueType::F64(f64::from_str(value).map_err(|_| err)?)),
        }
    }

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

#[derive(Debug, Clone, PartialEq)]
pub enum Mutibility {
    Const(ValueType),
    Mut(ValueType),
}

impl Mutibility {
    pub fn set_value(self, value: &str) -> crate::error::Result<Mutibility> {
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

#[derive(Debug, Clone, PartialEq)]
pub enum NumberType {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            NumberType::I32(i) => format!("i32 {}", i),
            NumberType::I64(i) => format!("i64 {}", i),
            NumberType::F32(i) => format!("f32 {}", i),
            NumberType::F64(i) => format!("f64 {}", i),
        };
        write!(f, "{}", content)
    }
}

impl FromStr for NumberType {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "i32" => Ok(NumberType::I32(0)),
            "i64" => Ok(NumberType::I64(0)),
            "f32" => Ok(NumberType::F32(0.0)),
            "f64" => Ok(NumberType::F64(0.0)),
            _ => Err(WasmError::err(format!("did not expect type '{}'", s))),
        }
    }
}

impl NumberType {
    pub fn set_value(self, value: &str) -> crate::error::Result<NumberType> {
        let err = WasmError::err(format!(
            "value defined '{}' does not match defined type of {}",
            value, self
        ));
        match self {
            NumberType::I32(_) => Ok(NumberType::I32(i32::from_str(value).map_err(|_| err)?)),
            NumberType::I64(_) => Ok(NumberType::I64(i64::from_str(value).map_err(|_| err)?)),
            NumberType::F32(_) => Ok(NumberType::F32(f32::from_str(value).map_err(|_| err)?)),
            NumberType::F64(_) => Ok(NumberType::F64(f64::from_str(value).map_err(|_| err)?)),
        }
    }

    pub fn same_stack_types(p1: &[NumberType], p2: &[NumberType]) -> bool {
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

    pub fn type_equality(&self, other: &NumberType) -> bool {
        let mut result = false;
        match self {
            NumberType::I32(_) => {
                if let NumberType::I32(_) = other {
                    result = true
                }
            }
            NumberType::I64(_) => {
                if let NumberType::I64(_) = other {
                    result = true
                }
            }
            NumberType::F32(_) => {
                if let NumberType::F32(_) = other {
                    result = true
                }
            }
            NumberType::F64(_) => {
                if let NumberType::F64(_) = other {
                    result = true
                }
            }
        };
        result
    }

    pub fn value_equality(&self, other: &NumberType) -> bool {
        let mut result = false;
        match self {
            NumberType::I32(i1) => {
                if let NumberType::I32(i2) = other {
                    result = *i1 == *i2
                }
            }
            NumberType::I64(i1) => {
                if let NumberType::I64(i2) = other {
                    result = *i1 == *i2
                }
            }
            NumberType::F32(i1) => {
                if let NumberType::F32(i2) = other {
                    result = *i1 == *i2
                }
            }
            NumberType::F64(i1) => {
                if let NumberType::F64(i2) = other {
                    result = *i1 == *i2
                }
            }
        };
        result
    }

    pub(crate) fn add(&self, other: NumberType) -> crate::error::Result<NumberType> {
        match self {
            NumberType::I32(i1) => {
                if let NumberType::I32(i2) = other {
                    Ok(NumberType::I32(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
            NumberType::I64(i1) => {
                if let NumberType::I64(i2) = other {
                    Ok(NumberType::I64(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
            NumberType::F32(i1) => {
                if let NumberType::F32(i2) = other {
                    Ok(NumberType::F32(*i1 + i2))
                } else {
                    Err(WasmError::err(format!(
                        "failed to 'add' {:?} with {:?}. Incorrect types",
                        self, other
                    )))
                }
            }
            NumberType::F64(i1) => {
                if let NumberType::F64(i2) = other {
                    Ok(NumberType::F64(*i1 + i2))
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
