use crate::validation::{Context, Validation, ValidationError};

/// Type of sign an integer is meant to taken as
///
/// Some integer instructions come in two flavors, where a signedness distinguishes
/// whether the operands are to be interpreted as signed or unsigned. Use two compliment
/// for signed interpretation means that they behave the same regardless of signedness
pub enum SignType {
    Signed,
    Unsigned,
}

pub enum HalfType {
    Low,
    High
}

pub enum BlockType {
    Index(TypeIndex),
    Value(Option<ValueType>),
}

impl BlockType {
    pub fn get_function_type(&self, ctx: &Context) -> Result<FunctionType, ValidationError> {
        match self {
            BlockType::Index(index) => ctx
                .get_type(*index)
                .map(Clone::clone)
                .ok_or_else(ValidationError::new),
            BlockType::Value(Some(value)) => Ok(FunctionType::anonymous(value.clone())),
            BlockType::Value(None) => Ok(FunctionType::empty()),
        }        
    }
}

impl Validation<FunctionType> for BlockType {
    fn validate(&self, ctx: &Context, args: FunctionType) -> Result<(), ValidationError> {
        match self {
            BlockType::Index(index) => ctx
                .get_type(*index)
                .and_then(|ty| if *ty == args { Some(ty) } else { None })
                .ok_or_else(ValidationError::new)
                .map(|_| ()),
            // This one is always ok, because the type is defined on the block type
            // already meaning that if we have parsed it, it's valid...
            // short hand for function type [] -> [ValueType?]
            BlockType::Value(_) => Ok(()),
        }
    }
}

pub type TypeIndex = u32;
pub type FunctionIndex = u32;
pub type TableIndex = u32;
pub type MemoryIndex = u32;
pub type GlobalIndex = u32;
pub type ElementIndex = u32;
pub type DataIndex = u32;
pub type LocalIndex = u32;
pub type LabelIndex = u32;

/// Classify Imports and external values with their respective types
pub enum ExternalType {
    Func(FunctionType),
    Table(TableType),
    Mem(MemoryType),
    Global(GlobalType),
}

impl Validation<()> for ExternalType {
    fn validate(&self, ctx: &Context, args: ()) -> Result<(), ValidationError> {
        match self {
            ExternalType::Func(inner) => inner.validate(ctx, args),
            ExternalType::Table(inner) => inner.validate(ctx, args),
            ExternalType::Mem(inner) => inner.validate(ctx, args),
            ExternalType::Global(inner) => inner.validate(ctx, args),
        }
    }
}

/// [GlobalType] hold a global value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalType {
    // Immutiable global value
    Const(ValueType),
    // Mutable global value
    Var(ValueType),

}

impl Validation<()> for GlobalType {
    fn validate(&self, _: &Context, _: ()) -> Result<(), ValidationError> {
        Ok(())
    }
}

impl Validation<GlobalType> for GlobalType {
    fn validate(&self, _: &Context, g2: GlobalType) -> Result<(), ValidationError> {
        if *self == g2 {
            Ok(())
        } else {
            Err(ValidationError::new())
        }
    }
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

impl TableType {
    pub fn ref_type(&self) -> &RefType {
        &self.ref_type
    }
}

impl Validation<()> for TableType {
    fn validate(&self, ctx: &Context, _: ()) -> Result<(), ValidationError> {
        let max = 2_u32.pow(32) - 1;
        self.limit.validate(ctx, max)?;
        // Otherwise, reftype is always ok.
        Ok(())
    }
}

impl Validation<TableType> for TableType {
    fn validate(&self, ctx: &Context, t2: TableType) -> Result<(), ValidationError> {
        if self.ref_type == t2.ref_type {
            self.limit.validate(ctx, t2.limit)
        } else {
            Err(ValidationError::new())
        }
    }
}

pub enum MemoryLoadNumber {
    Load8,
    Load16,
    Load32,
}

impl MemoryLoadNumber {
    pub fn bit_width(&self) -> u32 {
        match self {
            MemoryLoadNumber::Load8 => 8 ,
            MemoryLoadNumber::Load16 => 16,
            MemoryLoadNumber::Load32 => 32,
        }
    }
}

pub enum MemoryWidth {
    I8,
    I16,
    I32,
    I64
}

impl MemoryWidth {
    pub fn bit_width(&self) -> u32 {
        match self {
            MemoryWidth::I8 => 8,
            MemoryWidth::I16 => 16,
            MemoryWidth::I32 => 32,
            MemoryWidth::I64 => 64,
        }
    }
}

pub enum MemoryZeroWidth {
    I32,
    I64
}

impl MemoryZeroWidth {
    pub fn bit_width(&self) -> u32 {
        match self {
            MemoryZeroWidth::I32 => 32,
            MemoryZeroWidth::I64 => 64,
        }
    }
}

pub struct MemoryArgument {
    offset: u32,
    align: u32,
}

impl MemoryArgument {
    pub fn align(&self) -> u32 {
        self.align
    }
}

/// Classify linear ['Memories'] and their size range.
///
/// Contains the min and max of memory size, given in units of Page Size.
pub struct MemoryType {
    limit: Limit,
}

impl Validation<()> for MemoryType {
    fn validate(&self, ctx: &Context, _: ()) -> Result<(), ValidationError> {
        let max = 2_u32.pow(16);
        self.limit.validate(ctx, max)
    }
}

impl Validation<MemoryType> for MemoryType {
    fn validate(&self, ctx: &Context, args: MemoryType) -> Result<(), ValidationError> {
        self.limit.validate(ctx, args.limit)
    }
}

/// ["Limit"] size range of resizable storage. Associated with ["Memory"] and
/// ["Table"] types. Max is optional.
pub struct Limit {
    min: u32,
    max: Option<u32>,
}

impl Validation<u32> for Limit {
    /// Validate the limit is within range of K.
    fn validate(&self, _ctx: &Context, k: u32) -> Result<(), ValidationError> {
        if self.min > k {
            Err(ValidationError::new())
        } else if let Some(max) = self.max.as_ref() {
            if *max > k || self.min > *max {
                Err(ValidationError::new())
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

impl Validation<Limit> for Limit {
    fn validate(&self, _: &Context, args: Limit) -> Result<(), ValidationError> {
        let min_invalid = self.min < args.min;
        if let (Some(max1), Some(max2)) = (&self.max, &args.max) {
            if *max1 > *max2 || min_invalid {
                Err(ValidationError::new())
            } else {
                Ok(())
            }
        } else if min_invalid {
            // https://webassembly.github.io/spec/core/valid/types.html#import-subtyping
            // Imported limit must less then or equal to current limit
            Err(ValidationError::new())
        } else {
            Ok(())
        }
        // https://webassembly.github.io/spec/core/valid/types.html
    }
}

/// ["FunctionType"] is a classification signature of a function. It maps a vector
/// of result types (as parameters) to return types (as the return value).
///
/// They are also used to classify the input and outputs of ["Instructions"].
///
/// Can be represented in rust as (Box<dyn Fn(ResultType) -> ResultType>)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionType {
    input: ResultType,
    output: ResultType,
}

impl FunctionType {
    pub fn empty() -> Self {
        Self { input: ResultType(vec![]), output: ResultType(vec![]) }
    }

    pub fn anonymous(output: ValueType) -> Self {
        Self { input: ResultType(vec![]), output: ResultType(vec![output])}
    }
    
    pub fn output(&self) -> &ResultType {
        &self.output
    }
    
    pub fn input(&self) -> &ResultType {
        &self.input
    }
}

impl Validation<()> for FunctionType {
    fn validate(&self, _: &Context, _: ()) -> Result<(), ValidationError> {
        Ok(())
    }
}

impl Validation<FunctionType> for FunctionType {
    fn validate(&self, _: &Context, f2: FunctionType) -> Result<(), ValidationError> {
        // Both FunctionTypes must be equal, otherwise error
        if *self == f2 {
            Ok(())
        } else {
            Err(ValidationError::new())
        }
    }
}

/// ["ResultType"] contains the return values from exiting instructions or calling
/// a function. Its a sequence of values
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResultType(pub Vec<ValueType>);

// FunctionReference is just a pointer to a function
pub type FunctionReference = i32;
// ExternRef is just a pointer to a object
pub type ExternRef = i32;

/// ValueType are individual values that wasm can compute and a value that a
/// variable can use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Num(NumType),
    VecType(VecType),
    RefType(RefType),
}

impl ValueType {
    /// Returns `true` if the value type is [`Num`].
    ///
    /// [`Num`]: ValueType::Num
    #[must_use]
    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(..))
    }

    /// Returns `true` if the value type is [`VecType`].
    ///
    /// [`VecType`]: ValueType::VecType
    #[must_use]
    pub fn is_vec_type(&self) -> bool {
        matches!(self, Self::VecType(..))
    }

    /// Returns `true` if the value type is [`RefType`].
    ///
    /// [`RefType`]: ValueType::RefType
    #[must_use]
    pub fn is_ref_type(&self) -> bool {
        matches!(self, Self::RefType(..))
    }

    pub fn try_into_ref_type(self) -> Result<RefType, ValidationError> {
        if let Self::RefType(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_vec_type(self) -> Result<VecType, ValidationError> {
        if let Self::VecType(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_num(self) -> Result<NumType, ValidationError> {
        if let Self::Num(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_value_type(self, other: &Self) -> Result<Self, ValidationError> {
        if self == *other {
            Ok(self)
        } else {
            Err(ValidationError::new())
        }
    }
}

/// First class references to objects in the runtime ["Store"].
///
/// Reference types are opaque, meaning that neither their size nor their bit
/// pattern can be observed. Values of reference type can be stored in ["Tables"]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefType {
    // FunctionReference must exist, however, we don't know what it takes and we don't
    // know what it returns. The Function must exist in the program.
    FuncRef(FunctionReference),
    // A reference to a host resource. The resource should be owned by the ["Embedder"].
    // This is a type of pointer.
    ExternRef(ExternRef),
}

impl RefType {
    pub fn try_into_func_ref(self) -> Result<FunctionReference, ValidationError> {
        if let Self::FuncRef(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }
}

/// Vector types classify numeric values processed by SIMD instructions.
/// Also known as v128. It can be interpreted as signed or unsigned,
/// floating point, integer numbers or a single 128 bit type.
///
/// These are transparent (like ["NumType"]), meaning their bit patterns can
/// be observed. Values of vector type can be stored in memory
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VecType;
// TODO(Alec): Implement all the permutations of VecType. Because the value
// is "transparent"


pub enum VectorMemoryOp {
    I8x8, I16x4, I32x2
}

impl VectorMemoryOp {
    pub fn bit_width(&self) -> u32 {
        match self {
            VectorMemoryOp::I8x8 => 8 / 8 * 8,
            VectorMemoryOp::I16x4 => 16 / 8 * 4,
            VectorMemoryOp::I32x2 => 32 / 8 * 2,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VectorShape {
    Int(IntegerVectorShape),
    Float(FloatVectorShape)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerVectorShape {
    I8x16,
    I16x8,
    I32x4,
    I64x2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatVectorShape {
    F32x4,
    F64x2
}

pub type Boolean = i32;
pub type Pointer = i32;

/// Number types are transparent, meaning that their bit patterns can be observed.
/// Values of number type can be stored in ["Memory"].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumType {
    I32, // servers as Booleans and Memory Addresses
    I64,
    F32,
    F64,
}

impl NumType {
    /// Returns `true` if the num type is [`I32`].
    ///
    /// [`I32`]: NumType::I32
    #[must_use]
    pub fn is_i32(&self) -> bool {
        matches!(self, Self::I32)
    }

    pub fn try_into_i32(self) -> Result<NumType, ValidationError> {
        if self.is_i32() {
            Ok(self)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_ty(self, ty: NumType) -> Result<NumType, ValidationError> {
        if self == ty {
            Ok(self)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            NumType::I32 => 32,
            NumType::I64 => 64,
            NumType::F32 => 32,
            NumType::F64 => 64,
        }
    }
}

// impl ValueType {
//     pub fn set_value(self, value: &str) -> crate::error::Result<ValueType> {
//         let err = WasmError::err(format!(
//             "value defined '{}' does not match defined type of {}",
//             value, self
//         ));
//         match self {
//             ValueType::I32(_) => Ok(ValueType::I32(i32::from_str(value).map_err(|_| err)?)),
//             ValueType::I64(_) => Ok(ValueType::I64(i64::from_str(value).map_err(|_| err)?)),
//             ValueType::F32(_) => Ok(ValueType::F32(f32::from_str(value).map_err(|_| err)?)),
//             ValueType::F64(_) => Ok(ValueType::F64(f64::from_str(value).map_err(|_| err)?)),
//         }
//     }

//     pub fn same_stack_types(p1: &[ValueType], p2: &[ValueType]) -> bool {
//         if p1.len() != p2.len() {
//             false
//         } else {
//             for (i1, i2) in p1.iter().zip(p2) {
//                 //TODO(Alec): This should return an error so we can let the user know
//                 if !i1.type_equality(i2) {
//                     return false;
//                 }
//             }
//             true
//         }
//     }

//     pub fn type_equality(&self, other: &ValueType) -> bool {
//         let mut result = false;
//         match self {
//             ValueType::I32(_) => {
//                 if let ValueType::I32(_) = other {
//                     result = true
//                 }
//             }
//             ValueType::I64(_) => {
//                 if let ValueType::I64(_) = other {
//                     result = true
//                 }
//             }
//             ValueType::F32(_) => {
//                 if let ValueType::F32(_) = other {
//                     result = true
//                 }
//             }
//             ValueType::F64(_) => {
//                 if let ValueType::F64(_) = other {
//                     result = true
//                 }
//             }
//         };
//         result
//     }

//     pub fn value_equality(&self, other: &ValueType) -> bool {
//         let mut result = false;
//         match self {
//             ValueType::I32(i1) => {
//                 if let ValueType::I32(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//             ValueType::I64(i1) => {
//                 if let ValueType::I64(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//             ValueType::F32(i1) => {
//                 if let ValueType::F32(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//             ValueType::F64(i1) => {
//                 if let ValueType::F64(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//         };
//         result
//     }

//     pub fn add(&self, other: ValueType) -> crate::error::Result<ValueType> {
//         match self {
//             ValueType::I32(i1) => {
//                 if let ValueType::I32(i2) = other {
//                     Ok(ValueType::I32(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//             ValueType::I64(i1) => {
//                 if let ValueType::I64(i2) = other {
//                     Ok(ValueType::I64(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//             ValueType::F32(i1) => {
//                 if let ValueType::F32(i2) = other {
//                     Ok(ValueType::F32(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//             ValueType::F64(i1) => {
//                 if let ValueType::F64(i2) = other {
//                     Ok(ValueType::F64(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//         }
//     }

//     pub fn type_id_string(&self) -> String {
//         match self {
//             ValueType::I32(_) => "i32".to_string(),
//             ValueType::I64(_) => "i64".to_string(),
//             ValueType::F32(_) => "f32".to_string(),
//             ValueType::F64(_) => "f64".to_string(),
//         }
//     }

//     pub fn value_string(&self) -> String {
//         match self {
//             ValueType::I32(i) => i.to_string(),
//             ValueType::I64(i) => i.to_string(),
//             ValueType::F32(i) => i.to_string(),
//             ValueType::F64(i) => i.to_string(),
//         }
//     }
// }

// impl Display for ValueType {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let content = match self {
//             ValueType::I32(i) => format!("i32 {}", i),
//             ValueType::I64(i) => format!("i64 {}", i),
//             ValueType::F32(i) => format!("f32 {}", i),
//             ValueType::F64(i) => format!("f64 {}", i),
//         };
//         write!(f, "{}", content)
//     }
// }

// impl FromStr for ValueType {
//     type Err = WasmError;

//     fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
//         match s {
//             "i32" => Ok(ValueType::I32(0)),
//             "i64" => Ok(ValueType::I64(0)),
//             "f32" => Ok(ValueType::F32(0.0)),
//             "f64" => Ok(ValueType::F64(0.0)),
//             _ => Err(WasmError::err(format!("did not expect type '{}'", s))),
//         }
//     }
// }
