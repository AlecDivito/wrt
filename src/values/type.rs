use std::{fmt::Display, str::FromStr};

use crate::error::WasmError;

/// Classify Imports and external values with their respective types
pub enum ExternalType {
    Func(FuncType),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}


/// [GlobalType] hold a global value
pub enum GlobalType {
    // Immutiable global value
    Const(ValueType),
    // Mutable global value
    Var(ValueType),
}

/// Classify ["Table"] over elements of ["RefType"] within the size range.
/// 
/// Note: The type must contain only the one type of RefType... (maybe)
/// TODO(Alec): Validate...
/// 
/// Like ["Memory"] tables have a size limit.
pub struct TableType {
    limit: Limit,
    ref_type: RefType,
}

/// Classify linear ['Memories'] and their size range.
/// 
/// Contains the min and max of memory size, given in units of Page Size.
pub struct MemType(Limit);

/// ["Limit"] size range of resizable storage. Associated with ["Memory"] and 
/// ["Table"] types. Max is optional.
pub struct Limit {
    min: u32,
    max: Option<u32>
}

/// ["FuncType"] is a classification signature of a function. It maps a vector
/// of result types (as parameters) to return types (as the return value).
/// 
/// They are also used to classify the input and outpus of ["Instructions"].
pub struct FuncType(Box<dyn Fn(ResultType) -> ResultType>);

/// ["ResultType"] contains the return values from exiting instructions or calling
/// a function. Its a sequence of values
pub struct ResultType(Vec<ValueType>);

// FunctionType is just a pointer to a function
type FunctionType = i32;
// ExternRef is just a pointer to a object
type ExternRef = i32;

/// ValueType are inddividual values that wasm can compute and a value that a
/// variable can use.
#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Num(NumType),
    VecType(VecType),
    RefType(RefType),
}

/// First class references to objects in the runtime ["Store"].
/// 
/// Reference types are opaque, meaning that neither their size nor their bit
/// pattern can be observed. Values of reference type can be stored in ["Tables"]
#[derive(Debug, Clone, PartialEq)]
pub enum RefType {
    // FunctionType must exist, however, we don't know what it takes and we don't
    // know what it returns. The Function must exist in the program.
    FuncRef(FunctionType),
    // A reference to a host resource. The resource should be owned by the ["Embedder"].
    // This is a type of pointer.
    ExternRef(ExternRef),
}

/// Vector types classify numeric values processed by SIMD instructions.
/// Also known as v128. It can be interperted as signed or unsigned,
/// floating point, integer numbers or a single 128 bit type.
/// 
/// These are transparent (like ["NumType"]), meaning their bit patterns can
/// be observed. Values of vector type can be stored in memory
#[derive(Debug, Clone, PartialEq)]
pub struct VecType([u8; 16]);
// TODO(Alec): Implement all the permutations of VecType. Because the value
// is "transparent"

pub type Boolean = ValueType::I32();
pub type Pointer = ValueType::I32();

/// Number types are transparent, meaning that their bit patterns can be observed.
/// Values of number type can be stored in ["Memory"].
#[derive(Debug, Clone, PartialEq)]
pub enum NumType {
    I32(i32), // servers as Booleans and Memory Addresses
    I64(i64),
    F32(f32),
    F64(f64),
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

    pub fn add(&self, other: ValueType) -> crate::error::Result<ValueType> {
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

    pub fn type_id_string(&self) -> String {
        match self {
            ValueType::I32(_) => "i32".to_string(),
            ValueType::I64(_) => "i64".to_string(),
            ValueType::F32(_) => "f32".to_string(),
            ValueType::F64(_) => "f64".to_string(),
        }
    }

    pub fn value_string(&self) -> String {
        match self {
            ValueType::I32(i) => i.to_string(),
            ValueType::I64(i) => i.to_string(),
            ValueType::F32(i) => i.to_string(),
            ValueType::F64(i) => i.to_string(),
        }
    }
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
