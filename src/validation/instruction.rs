use crate::structure::types::{FunctionIndex, NumType, RefType, SignType, ValueType};

use super::{Context, ValidationError};

pub type ValidateResult<T> = Result<T, ValidationError>;

pub struct Input(Vec<ValueType>);

impl Input {
    pub fn pop(&mut self) -> ValidateResult<ValueType> {
        self.pop().ok_or(ValidationError::new())
    }
}

pub trait ValidateInstruction {
    type Output: IntoIterator<Item = ValueType>;
    fn validate(&self, ctx: Context, inputs: &mut Input) -> ValidateResult<Self::Output>;
}

/**
 * Number Instructions
 */

macro_rules! const_instruction {
    ($name_ty:ident, $name:ident, $typ:ty) => {
        pub struct $name_ty($typ);
        impl ValidateInstruction for $name_ty {
            type Output = [ValueType; 1];
            fn validate(&self, _: Context, _: &mut Input) -> ValidateResult<Self::Output> {
                Ok([ValueType::Num(crate::structure::types::NumType::$name)])
            }
        }
    };
}

const_instruction!(ConstI32, I32, i32); // Create ConstI32 type
const_instruction!(ConstI64, I64, i64); // Create ConstI32 type
const_instruction!(ConstF32, F32, f32); // Create ConstI32 type
const_instruction!(ConstF64, F64, f64); // Create ConstI32 type

// Validate Const operations
pub struct Const(NumType);
impl ValidateInstruction for Const {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, _: &mut Input) -> ValidateResult<Self::Output> {
        Ok([ValueType::Num(self.0.clone())])
    }
}

// Validate Unary Operations. Only avaliable for numbers.
pub struct UnaryOperation;
impl ValidateInstruction for UnaryOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, input: &mut Input) -> ValidateResult<Self::Output> {
        let value = input.pop()?;
        if value.is_num() {
            Ok([value])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate Binary Operation. Only avaliable for numbers.
pub struct BinaryOperation;
impl ValidateInstruction for BinaryOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, inputs: &mut Input) -> ValidateResult<Self::Output> {
        let op1 = inputs.pop()?;
        let op2 = inputs.pop()?;
        if op1.is_num() && op2.is_num() && op1 == op2 {
            Ok([op1])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate test operations. Only avalible for numbers
pub struct TestOperation;
impl ValidateInstruction for TestOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, inputs: &mut Input) -> ValidateResult<Self::Output> {
        if inputs.pop()?.is_num() {
            Ok([ValueType::Num(NumType::I32)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate relop opertion. Only avalible to numbers
pub struct RelopOperation;
impl ValidateInstruction for RelopOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, inputs: &mut Input) -> ValidateResult<Self::Output> {
        let op1 = inputs.pop()?;
        let op2 = inputs.pop()?;
        if op1.is_num() && op2.is_num() && op1 == op2 {
            Ok([ValueType::Num(NumType::I32)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate cvt operation. Only avaliable on numbers.
pub struct CvtOperation {
    // This is the number types that is calling this function. Ex. i32 => i32.warp_i64_s
    // i32 is the input
    // i64 is the output
    // s is for signed
    input: NumType,
    output: NumType,
    sign: SignType,
}
impl ValidateInstruction for CvtOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, inputs: &mut Input) -> ValidateResult<Self::Output> {
        let op1 = inputs.pop()?;
        if ValueType::Num(self.input) == op1 {
            Ok([ValueType::Num(self.output)])
        } else {
            Err(ValidationError::new())
        }
    }
}

/**
 * Reference Instructions
 */
pub struct NullReferenceOperation(RefType);
impl ValidateInstruction for NullReferenceOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, _: &mut Input) -> ValidateResult<Self::Output> {
        Ok([ValueType::RefType(self.0.clone())])
    }
}

pub struct IsNullReferenceOperation;
impl ValidateInstruction for IsNullReferenceOperation {
    type Output = [ValueType; 1];
    fn validate(&self, _: Context, inputs: &mut Input) -> ValidateResult<Self::Output> {
        let op = inputs.pop()?;
        if op.is_ref_type() {
            Ok([ValueType::Num(NumType::I32)])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct FunctionReferenceOperation(FunctionIndex);
impl ValidateInstruction for FunctionReferenceOperation {
    type Output = [ValueType; 1];
    fn validate(&self, ctx: Context, inputs: &mut Input) -> ValidateResult<Self::Output> {
        if let Some(func) = ctx.get_function(self.0) {
            if ctx.contains_reference(self.0) {
                // Complete: https://webassembly.github.io/spec/core/valid/instructions.html#syntax-stacktype
            }
        }
    }
}
