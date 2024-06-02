use std::{fmt::Display, iter::Peekable};

use crate::{
    ast::Expr,
    parse::{
        ast::{read_u32, Error, Expect, Parse, TryGet},
        tokenize, Keyword, Token,
    },
    validation::{Context, Input, ValidateInstruction, Validation, ValidationError},
};

use super::module::{get_id, Data, Module};

/// Type of sign an integer is meant to taken as
///
/// Some integer instructions come in two flavors, where a signedness distinguishes
/// whether the operands are to be interpreted as signed or unsigned. Use two compliment
/// for signed interpretation means that they behave the same regardless of signedness
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignType {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HalfType {
    Low,
    High,
}

pub enum BlockType {
    Index(TypeIndex),
    Value(Option<ValueType>),
}

impl BlockType {
    pub fn get_function_type(&self, ctx: &Context) -> Result<FunctionType, ValidationError> {
        match self {
            BlockType::Index(index) => ctx.get_type(*index).cloned(),
            BlockType::Value(Some(value)) => Ok(FunctionType::anonymous(value.clone())),
            BlockType::Value(None) => Ok(FunctionType::empty()),
        }
    }
}

impl Validation<FunctionType> for BlockType {
    fn validate(&self, ctx: &Context, _: FunctionType) -> Result<(), ValidationError> {
        match self {
            BlockType::Index(index) => ctx.get_type(*index).map(|_| ()),
            // This one is always ok, because the type is defined on the block type
            // already meaning that if we have parsed it, it's valid...
            // short hand for function type [] -> [ValueType?]
            BlockType::Value(_) => Ok(()),
        }
    }
}

pub enum Index {
    Id(String),
    Index(u32),
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

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for GlobalType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        Ok(if let Some(_) = tokens.peek().copied().try_left_paran() {
            tokens.next().expect_left_paren()?;
            tokens.next().expect_keyword_token(Keyword::Mut)?;
            Self::Var(ValueType::parse(tokens)?)
        } else {
            Self::Const(ValueType::parse(tokens)?)
        })
    }
}

impl Validation<()> for GlobalType {
    fn validate(&self, _: &Context, _: ()) -> Result<(), ValidationError> {
        Ok(())
    }
}

impl Validation<ValueType> for GlobalType {
    fn validate(&self, _: &Context, args: ValueType) -> Result<(), ValidationError> {
        let value = match self {
            GlobalType::Const(value) => value,
            GlobalType::Var(value) => value,
        };
        if *value == args {
            Ok(())
        } else {
            Err(ValidationError::new())
        }
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
#[derive(Clone)]
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
            MemoryLoadNumber::Load8 => 8,
            MemoryLoadNumber::Load16 => 16,
            MemoryLoadNumber::Load32 => 32,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryWidth {
    I8,
    I16,
    I32,
    I64,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryZeroWidth {
    I32,
    I64,
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
#[derive(Clone)]
pub struct MemoryType {
    limit: Limit,
}

impl MemoryType {
    pub fn new(limit: Limit) -> Self {
        Self { limit }
    }
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
#[derive(Clone)]
pub struct Limit {
    min: u32,
    max: Option<u32>,
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for Limit {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let min = read_u32(tokens.next().expect_number()?)?;
        let max = if tokens.peek().copied().expect_number().is_ok() {
            Some(read_u32(tokens.next().expect_number()?)?)
        } else {
            None
        };
        Ok(Limit { min, max })
    }
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
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct FunctionType {
    input: ResultType,
    output: ResultType,
}

impl FunctionType {
    pub fn new(input: ResultType, output: ResultType) -> Self {
        Self { input, output }
    }

    pub fn empty() -> Self {
        Self {
            input: ResultType { values: vec![] },
            output: ResultType { values: vec![] },
        }
    }

    pub fn anonymous(output: ValueType) -> Self {
        Self {
            input: ResultType { values: vec![] },
            output: ResultType {
                values: vec![output],
            },
        }
    }

    pub fn output(&self) -> &ResultType {
        &self.output
    }

    pub fn input(&self) -> &ResultType {
        &self.input
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty() && self.output.is_empty()
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
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct ResultType {
    values: Vec<ValueType>,
}

impl ResultType {
    pub fn new(values: Vec<ValueType>) -> Self {
        Self { values }
    }

    pub fn values(&self) -> &[ValueType] {
        &self.values
    }

    pub fn take(self) -> Vec<ValueType> {
        self.values
    }

    pub fn extend(&mut self, list: Vec<ValueType>) {
        self.values.extend(list);
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

pub struct FuncParam(pub Vec<ValueType>);
impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for FuncParam {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Param)?;
        let values = match get_id(tokens) {
            Some(_) => vec![ValueType::parse(tokens)?],
            None => {
                let mut tys = vec![];
                while !tokens.peek().copied().expect_right_paren().is_ok() {
                    tys.push(ValueType::parse(tokens)?);
                }
                tys
            }
        };
        tokens.next().expect_right_paren()?;
        return Ok(FuncParam(values));
    }
}

pub struct FuncResult(pub Vec<ValueType>);
impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for FuncResult {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Result)?;
        let mut values = vec![];
        while !tokens.peek().copied().expect_right_paren().is_ok() {
            values.push(ValueType::parse(tokens)?);
        }
        tokens.next().expect_right_paren()?;
        return Ok(FuncResult(values));
    }
}

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

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for ValueType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let token = tokens.peek();
        let value = match token.copied().expect_keyword()? {
            Keyword::I32 => ValueType::Num(NumType::I32),
            Keyword::I64 => ValueType::Num(NumType::I64),
            Keyword::F32 => ValueType::Num(NumType::F32),
            Keyword::F64 => ValueType::Num(NumType::F64),
            _ => return Err(Error::new(token.cloned().cloned(), "Unexpected keyword")),
        };
        let _ = tokens.next();
        Ok(value)
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
    I8x8,
    I16x4,
    I32x2,
}

impl VectorMemoryOp {
    pub fn bit_width(&self) -> u32 {
        match self {
            VectorMemoryOp::I8x8 => 8 / (8 * 8),
            VectorMemoryOp::I16x4 => 16 / (8 * 4),
            VectorMemoryOp::I32x2 => 32 / (8 * 2),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VectorShape {
    Int(IntegerVectorShape),
    Float(FloatVectorShape),
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
    F64x2,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    I32,
    I64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Left,
    Rigth,
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

impl Display for NumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = match self {
            NumType::I32 => "i32",
            NumType::I64 => "i64",
            NumType::F32 => "f32",
            NumType::F64 => "f64",
        };
        write!(f, "{}", content)
    }
}

pub struct RelativeExport {
    pub name: String,
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for RelativeExport {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Export)?;
        let name = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        Ok(RelativeExport { name })
    }
}

pub struct RelativeImport {
    module: String,
    name: String,
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for RelativeImport {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Import)?;
        let module = tokens.next().expect_string()?;
        let name = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        Ok(RelativeImport { module, name })
    }
}

pub struct FuncType {
    id: Option<String>,
    params: Vec<ValueType>,
    result: Vec<ValueType>,
}

impl FuncType {
    pub fn into_parts(self) -> (Option<String>, FunctionType) {
        let params = ResultType::new(self.params);
        let results = ResultType::new(self.result);
        (self.id, FunctionType::new(params, results))
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for FuncType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Type)?;
        let id = get_id(tokens);
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Func)?;

        let mut params = vec![];
        let mut result = vec![];

        if tokens.peek().copied().try_right_paran().is_some() {
            tokens.next().expect_right_paren()?; // For (func)
            tokens.next().expect_right_paren()?; // For (type (func))
            return Ok(FuncType { id, params, result });
        }

        loop {
            tokens.peek().copied().expect_left_paren()?;
            match tokens.clone().nth(1).expect_keyword()? {
                Keyword::Param if result.is_empty() => params.extend(FuncParam::parse(tokens)?.0),
                Keyword::Result => result.extend(FuncResult::parse(tokens)?.0),
                _ => break,
            }
            if tokens.peek().copied().try_right_paran().is_some() {
                break;
            }
        }

        tokens.next().expect_right_paren()?; // For (func)
        tokens.next().expect_right_paren()?; // For (type (func))
        Ok(FuncType { id, params, result })
    }
}

pub struct StartOpts {
    func_id: Index,
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for StartOpts {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Start)?;
        let func_id = if let Some(id) = get_id(tokens) {
            Index::Id(id)
        } else if let Ok(index) = read_u32(tokens.peek().copied().expect_number()?) {
            Index::Index(index)
        } else {
            return Err(Error::new(
                tokens.next().cloned(),
                "'start' block expected index of number or function id.",
            ));
        };
        tokens.next().expect_right_paren()?;
        Ok(Self { func_id })
    }
}

pub struct UseMemory(Index);

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for UseMemory {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Memory)?;
        let id = if let Some(id) = get_id(tokens) {
            Index::Id(id)
        } else if let Ok(index) = read_u32(tokens.peek().copied().expect_number()?) {
            Index::Index(index)
        } else {
            return Err(Error::new(
                tokens.next().cloned(),
                "'memory' block expected index of number or memory id.",
            ));
        };

        Ok(Self(id))
    }
}

pub struct Instruction();
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Instruction {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        todo!("Parse instruction {:?}", tokens.next())
    }
}

pub struct OffsetExpr(Instruction);
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for OffsetExpr {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        let instr = match tokens.next().expect_keyword()? {
            Keyword::Offset => {
                tokens.next().expect_left_paren()?;
                Instruction::parse(tokens)?
            }
            key if key.has_unary_return_ty_i32() => Instruction::parse(tokens)?,
            key => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    format!("'{:?}' keyword not expected in offset block.", key),
                ))
            }
        };

        Ok(Self(instr))
    }
}

pub enum DataOpsMode {
    Passive,
    Active,
}

pub struct DataOps {
    id: Option<String>,
    data: String,
    mode: DataOpsMode,
    memory: UseMemory,
    offset: Option<OffsetExpr>,
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for DataOps {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self {
            id: None,
            data: String::new(),
            mode: DataOpsMode::Passive,
            memory: UseMemory(Index::Index(0)),
            offset: None,
        };

        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Data)?;
        this.id = get_id(tokens);
        if tokens.peek().copied().try_left_paran().is_some() {
            this.memory = UseMemory::parse(tokens)?;
            this.offset = Some(OffsetExpr::parse(tokens)?);
            this.mode = DataOpsMode::Active
        }
        this.data = tokens.peek().copied().expect_string().unwrap_or_default();
        tokens.next().expect_right_paren()?;

        Ok(this)
    }
}

pub struct MemoryOpts {
    id: Option<String>,
    limit: Limit,
    import: Vec<RelativeImport>,
    export: Vec<RelativeExport>,
    data: Vec<DataOps>,
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for MemoryOpts {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Memory)?;
        let id = get_id(tokens);
        // import / export
        let mut import = vec![];
        let mut export = vec![];
        let mut data = vec![];
        if tokens.peek().copied().try_left_paran().is_some() {
            match tokens.clone().nth(1).expect_keyword()? {
                Keyword::Import => import.push(RelativeImport::parse(tokens)?),
                Keyword::Export => export.push(RelativeExport::parse(tokens)?),
                Keyword::Data => data.push(DataOps::parse(tokens)?),
                key => {
                    return Err(Error::new(
                        tokens.next().cloned(),
                        format!(
                        "Memory inner block only expected 'import', 'export' or 'data'. Got {:?}",
                        key
                    ),
                    ))
                }
            }
        }
        let limit = Limit::parse(tokens)?;
        tokens.next().expect_right_paren()?;
        Ok(Self {
            id,
            limit,
            import,
            export,
            data,
        })
    }
}

pub struct BlockInstruction {
    id: Option<String>,

    result: Vec<ValueType>,
}

impl BlockInstruction {
    pub fn empty(id: Option<String>) -> Self {
        Self::new(id, vec![])
    }

    pub fn new(id: Option<String>, result: Vec<ValueType>) -> Self {
        Self { id, result }
    }
}

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BlockInstruction {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         tokens.next().expect_keyword_token(Keyword::Block)?;
//         let id = get_id(tokens);
//         if tokens.peek().copied().try_right_paran().is_some() {
//             return Ok(BlockInstruction::empty(id))
//         }

//         // Try and expect the result
//         tokens.peek().copied().expect_left_paren()?;
//         let result = match tokens.clone().nth(1).expect_keyword()? {
//             Keyword::Result => ResultType::parse(tokens)?.take(),
//             Keyword::Type => Ty
//             keyword => return Err(Error::new(None, format!("Did not expect '{keyword:?}' when parsing block")))
//         };
//         let result = if let Ok(_) =  {

//         } else {
//             vec![]
//         };
//         if tokens.peek().copied().try_right_paran().is_some() {
//             return Ok(BlockInstruction::new(id, result))
//         }

//         // 1.

//     }
// }

pub struct AssertMalformed {
    wat: String,
    expected_error: String,
}

impl AssertMalformed {
    pub fn test(self) -> Result<(), Error> {
        let module = format!("(module {})", self.wat);
        let tokens = tokenize(&module)?;
        let mut iter = tokens.iter().peekable();
        match Module::parse(&mut iter) {
            Ok(_) => Err(Error::new(
                None,
                "Module compiled successfully when it was expected to fail",
            )),
            Err(err) if err.error_ty() == self.expected_error => Ok(()),
            Err(err) => Err(Error::new(
                err.token().cloned(),
                format!(
                    "Error '{}' != '{}'. Expected '{}'",
                    self.expected_error,
                    err.error_ty(),
                    err.error()
                ),
            )),
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for AssertMalformed {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens
            .next()
            .expect_keyword_token(Keyword::AssertMalformed)?;
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Module)?;
        tokens.next().expect_keyword_token(Keyword::Quote)?;
        let wat = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        let expected_error = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        Ok(Self {
            wat,
            expected_error,
        })
    }
}

pub struct AssertInvalid {
    module: Module,
    expected_error: String,
}

impl AssertInvalid {
    pub fn test(self) -> Result<(), Error> {
        let mut ctx = Context::default();
        let mut input = Input::new();
        match self.module.validate(&mut ctx, &mut input) {
            Ok(_) => Err(Error::new(
                None,
                "Module validated successfully when it was expected to fail",
            )),
            Err(err) if err.error_ty() == self.expected_error => Ok(()),
            Err(err) => Err(Error::new(
                None,
                format!(
                    "Error '{}' != '{}'. Expected '{}'",
                    self.expected_error,
                    err.error_ty(),
                    err.error()
                ),
            )),
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for AssertInvalid {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::AssertInvalid)?;

        let module = Module::parse(tokens)?;
        let expected_error = tokens.next().expect_string()?;

        Ok(Self {
            module,
            expected_error,
        })
    }
}
