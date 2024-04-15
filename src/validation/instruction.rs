use crate::structure::{instruction::Instruction, types::{BlockType, DataIndex, ElementIndex, FloatVectorShape, FunctionIndex, FunctionReference, GlobalType, HalfType, IntegerVectorShape, LabelIndex, LocalIndex, MemoryArgument, MemoryIndex, MemoryLoadNumber, MemoryWidth, MemoryZeroWidth, NumType, RefType, SignType, TableIndex, TypeIndex, ValueType, VecType, VectorMemoryOp, VectorShape}};

use super::{Context, Input, InstructionSequence, ValidateInstruction, ValidateResult, ValidationError};

/**
 * Number Instructions
 */

macro_rules! const_instruction {
    ($name_ty:ident, $name:ident, $typ:ty) => {
        pub struct $name_ty($typ);
        impl ValidateInstruction for $name_ty {
            // type Output = [ValueType; 1];
            fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
                Ok(vec![ValueType::Num(crate::structure::types::NumType::$name)])
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
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::Num(self.0.clone())])
    }
}

// Validate Unary Operations. Only avaliable for numbers.
pub struct UnaryOperation;
impl ValidateInstruction for UnaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, input: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let value = input.pop()?;
        if value.is_num() {
            Ok(vec![value])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate Binary Operation. Only avaliable for numbers.
pub struct BinaryOperation;
impl ValidateInstruction for BinaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let op1 = inputs.pop()?;
        let op2 = inputs.pop()?;
        if op1.is_num() && op2.is_num() && op1 == op2 {
            Ok(vec![op1])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate test operations. Only avalible for numbers
pub struct TestOperation;
impl ValidateInstruction for TestOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if inputs.pop()?.is_num() {
            Ok(vec![ValueType::Num(NumType::I32)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// Validate relop opertion. Only avalible to numbers
pub struct RelopOperation;
impl ValidateInstruction for RelopOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let op1 = inputs.pop()?;
        let op2 = inputs.pop()?;
        if op1.is_num() && op2.is_num() && op1 == op2 {
            Ok(vec![ValueType::Num(NumType::I32)])
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
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let op1 = inputs.pop()?;
        if ValueType::Num(self.input) == op1 {
            Ok(vec![ValueType::Num(self.output)])
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
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::RefType(self.0.clone())])
    }
}

pub struct IsNullReferenceOperation;
impl ValidateInstruction for IsNullReferenceOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let op = inputs.pop()?;
        if op.is_ref_type() {
            Ok(vec![ValueType::Num(NumType::I32)])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct FunctionReferenceOperation {
    function_index: FunctionIndex
}
impl ValidateInstruction for FunctionReferenceOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if let Some(_) = ctx.get_function(self.function_index) {
            if ctx.contains_reference(self.function_index) {
                // Reference: https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-ref-mathsf-ref-func-x
                // TODO(Alec): We are supposed to be returning a function ref
                // A function ref is basically a pointer to a function
                // At runtime we validate if the function call is given the correct
                // arguments. So i think just returning the index to the function
                // should be ok because it's technically a pointer.
                return Ok(vec![ValueType::RefType(RefType::FuncRef(self.function_index as FunctionReference))])
            }
        }
        Err(ValidationError::new())
    }
}

/**
 * Vector Instructions
 * https://webassembly.github.io/spec/core/valid/instructions.html#vector-instructions
 */
pub struct ConstV128(u128);
impl ValidateInstruction for ConstV128 {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::VecType(VecType)])
    }
}

pub struct VectorUnaryOperation;
impl ValidateInstruction for VectorUnaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::VecType(inputs.pop()?.try_into_vec_type()?)])
    }
}

pub struct VectorBinaryOperation;
impl ValidateInstruction for VectorBinaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

pub struct VectorTernaryOperation;
impl ValidateInstruction for VectorTernaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

pub struct VectorTestOperation;
impl ValidateInstruction for VectorTestOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#mathsf-i8x16-xref-syntax-instructions-syntax-instr-vec-mathsf-swizzle
// i8x16 swizzle
pub struct VectorSwizzleOperation;
impl ValidateInstruction for VectorSwizzleOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

pub struct LaneIndex(u32);
impl LaneIndex {
    pub fn validate(&self) -> bool {
        self.0 < 32
    }

    pub fn validate_against_shape(&self, shape: VectorShape) -> bool {
        match shape {
            VectorShape::Int(IntegerVectorShape::I8x16) => self.0 < 16,
            VectorShape::Int(IntegerVectorShape::I16x8) => self.0 < 8,
            VectorShape::Int(IntegerVectorShape::I32x4) => self.0 < 4,
            VectorShape::Int(IntegerVectorShape::I64x2) => self.0 < 2,
            VectorShape::Float(FloatVectorShape::F32x4) => self.0 < 4,
            VectorShape::Float(FloatVectorShape::F64x2) => self.0 < 2,
        }
    }

    pub fn validate_against_memory_width(&self, width: &MemoryWidth) -> bool {
        // https://webassembly.github.io/spec/core/valid/instructions.html#mathsf-v128-xref-syntax-instructions-syntax-instr-memory-mathsf-load-n-mathsf-lane-xref-syntax-instructions-syntax-memarg-mathit-memarg-xref-syntax-instructions-syntax-laneidx-mathit-laneidx
        match width {
            MemoryWidth::I8 => self.0 < 128 / 8,
            MemoryWidth::I16 => self.0 < 128 / 16,
            MemoryWidth::I32 => self.0 < 128 / 32,
            MemoryWidth::I64 => self.0 < 128 / 64,
        }
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#mathsf-i8x16-xref-syntax-instructions-syntax-instr-vec-mathsf-shuffle-xref-syntax-instructions-syntax-laneidx-mathit-laneidx-16
// i8x16
pub struct VectorShuffleOperation{
    // Index lane can't be larger then the number 32
    lane_index: LaneIndex
}
impl ValidateInstruction for VectorShuffleOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if !self.lane_index.validate() {
            Err(ValidationError::new())
        } else {
            inputs.pop()?.try_into_vec_type()?;
            inputs.pop()?.try_into_vec_type()?;
            Ok(vec![ValueType::VecType(VecType)])
        }
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-instr-vec-mathsf-splat
// shape.splat
// shape => i8x16, i16x8, 132x4, i64x2, f32x4, f64x2
// TODO(Alec): Come back to this
pub struct VectorSplatOperation{
    shape: VectorShape
}
impl ValidateInstruction for VectorSplatOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // unpacked(VectorShape), takes an V128 from the top and "unpacks" it?
        // Does unpack mean take one value, or all of them? For example, if I have
        // I32x4, Does that mean i pop 4 inputs off the top and pack them into an
        // V128? And is that the types i return?
        todo!("What does 'unpacked' mean?")
        // inputs.pop()?.try_into_vec_type()?.unpack
        // Ok(vec![ValueType::VecType(VecType)])
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-instr-vec-mathsf-extract-lane-mathsf-xref-syntax-instructions-syntax-sx-mathit-sx-xref-syntax-instructions-syntax-laneidx-mathit-laneidx
// shape.extract_lane_sx laneindex
pub struct VectorExtractLaneOperation {
    shape: VectorShape,
    sign: SignType,
    lane_index: LaneIndex,
}
impl ValidateInstruction for VectorExtractLaneOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if self.lane_index.validate_against_shape(self.shape) {
            Err(ValidationError::new())
        } else {
            inputs.pop()?.try_into_vec_type()?;
            // TODO(Alec): I don't believe we can validate the actual operation,
            // we'd need to do that at runtime.
            let ty = match self.shape {
                VectorShape::Int(IntegerVectorShape::I8x16) => NumType::I32,
                VectorShape::Int(IntegerVectorShape::I16x8) => NumType::I32,
                VectorShape::Int(IntegerVectorShape::I32x4) => NumType::I32,
                VectorShape::Int(IntegerVectorShape::I64x2) => NumType::I64,
                VectorShape::Float(FloatVectorShape::F32x4) => NumType::F32,
                VectorShape::Float(FloatVectorShape::F64x2) => NumType::F64,
            };
            Ok(vec![ValueType::Num(ty)])
        }
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-instr-vec-mathsf-replace-lane-xref-syntax-instructions-syntax-laneidx-mathit-laneidx
// shape.replace_lane lane index
pub struct VectorReplaceLaneOperation {
    shape: VectorShape,
    lane_index: LaneIndex,
}
impl ValidateInstruction for VectorReplaceLaneOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let shape = inputs.pop()?.try_into_num()?; // Get the value type from the first operand
        inputs.pop()?.try_into_vec_type()?;                 // Get the vector
        if self.lane_index.validate_against_shape(self.shape) || shape != shape {
            Err(ValidationError::new())
        } else {
            Ok(vec![ValueType::VecType(VecType)])
        }
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-vunop-mathit-vunop
pub struct VectorShapeUnaryOperation {
    _shape: VectorShape
}
impl ValidateInstruction for VectorShapeUnaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): DO i need to parse the actual value to validate that it
        // matches with the shape we have?
        Ok(vec![ValueType::VecType(inputs.pop()?.try_into_vec_type()?)])
    }
}

pub struct VectorShapeBinaryOperation {
    _shape: VectorShape
}
impl ValidateInstruction for VectorShapeBinaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): DO i need to parse the actual value to validate that it
        // matches with the shape we have?
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

// shape.vrelop
pub struct VectorShapeRelationOperation {
    _shape: VectorShape
}
impl ValidateInstruction for VectorShapeRelationOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): DO i need to parse the actual value to validate that it
        // matches with the shape we have?
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

// ishape.vishiftop
pub struct VectorShapeShiftOperation {
    _shape: IntegerVectorShape
}
impl ValidateInstruction for VectorShapeShiftOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): DO i need to parse the actual value to validate that it
        // matches with the shape we have?
        
        // Pop the first argument as a I32
        inputs.pop()?.try_into_num()?.try_into_i32()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

// shape.vtestop
// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-vtestop-mathit-vtestop
pub struct VectorShapeTestOperation {
    _shape: VectorShape
}
impl ValidateInstruction for VectorShapeTestOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): DO i need to parse the actual value to validate that it
        // matches with the shape we have?
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

// shape.vcvtop_(half)_(shape)_(signed)_zero
pub struct VectorShapeTopHalfZeroOperation {
    _shape: VectorShape,
    _sign: SignType,
    _half: HalfType
}
impl ValidateInstruction for VectorShapeTopHalfZeroOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::VecType(inputs.pop()?.try_into_vec_type()?)])
    }
}

// (ishape).narrow_(ipshape)_(sx)
// First ishape is _op_shape
pub struct VectorShapeNarrowOperation {
    _op_shape: IntegerVectorShape,
    _desired_shape: IntegerVectorShape,
    _desired_sign: SignType
}
impl ValidateInstruction for VectorShapeNarrowOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

// (ishape).bitmask
pub struct VectorShapeBitMaskOperation {
    _shape: IntegerVectorShape
}
impl ValidateInstruction for VectorShapeBitMaskOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}


// (ishape).dot_(ishape)_s
pub struct VectorShapeDotProductOperation {
    _op_shape: IntegerVectorShape,
    _desired_shape: IntegerVectorShape,
}
impl ValidateInstruction for VectorShapeDotProductOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

// (ishape).extmul_(half)_(ishape)_(sx)
pub struct VectorShapeExtendMultiplyOperation {
    _op_shape: IntegerVectorShape,
    _desired_shape: IntegerVectorShape,
    _desired_half: HalfType,
    _desired_sign: SignType
}
impl ValidateInstruction for VectorShapeExtendMultiplyOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

// (ishape).extadd_pairwise_(ishape)_(sx)
pub struct VectorShapeExtendAddPairWiseOperation {
    _op_shape: IntegerVectorShape,
    _desired_shape: IntegerVectorShape,
    _desired_sign: SignType
}
impl ValidateInstruction for VectorShapeExtendAddPairWiseOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        inputs.pop()?.try_into_vec_type()?;
        inputs.pop()?.try_into_vec_type()?;
        Ok(vec![ValueType::VecType(VecType)])
    }
}

/**
 * Parametric Instructions
 */

pub struct DropOperation;
impl ValidateInstruction for DropOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // Drop is a `value-polymorphic` instruction.
        // NOTE: I believe that means that **ANY NUMBER** can be popped off the stack
        // It doesn't not mean you can drop a vector...but that may not be correct.
        // It could be that inputs.pop()?, is all that is needed here.
        inputs.pop()?.try_into_num()?;
        Ok(vec![])
    }
}

pub struct SelectOperation{
    t: Option<NumType>
}
impl ValidateInstruction for SelectOperation {
    // type Output = [ValueType; 1];

    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // First we always pop an I32
        inputs.pop()?.try_into_num()?.try_into_i32()?;
        let ty1 = inputs.pop()?;
        let ty2 = inputs.pop()?;
        match self.t {
            Some(num) => {
                if ty1 == ty2 && ValueType::Num(num) == ty1 && ValueType::Num(num) == ty2 {
                    Ok(vec![ty1])
                } else {
                    Err(ValidationError::new())
                }
            },
            None => {
                if ty1 == ty2 && (ty1.is_num() || ty1.is_vec_type()) {
                    Ok(vec![ty1])
                } else {
                    Err(ValidationError::new())
                }
            },
        }
    }
}

/**
 * Variable Instructions
 */

pub struct LocalGetOperation {
    index: LocalIndex
}
impl ValidateInstruction for LocalGetOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ctx.get_local(self.index)?.clone()])
    }
}

pub struct LocalSetOperation {
    index: LocalIndex
}
impl ValidateInstruction for LocalSetOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = inputs.pop()?;
        ctx.set_local(self.index, ty)?; 
        Ok(vec![])
    }
}

pub struct LocalTeeOperation {
    index: LocalIndex
}
impl ValidateInstruction for LocalTeeOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = inputs.pop()?;
        ctx.set_local(self.index, ty.clone())?; 
        Ok(vec![ty])
    }
}


pub struct GlobalGetOperation {
    index: LocalIndex
}
impl ValidateInstruction for GlobalGetOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let global = ctx.get_global(self.index)?;
        match global {
            // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-global-get-x
            // Why can't we get a const?
            GlobalType::Const(_) => Err(ValidationError::new()),
            GlobalType::Var(ty) => Ok(vec![ty.clone()]),
        }
    }
}

pub struct GlobalSetOperation {
    index: LocalIndex
}
impl ValidateInstruction for GlobalSetOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = inputs.pop()?;
        ctx.set_global(self.index, ty)?; 
        Ok(vec![])
    }
}

/**
 * Table Instructions
 */
pub struct TableGetOperation {
    index: TableIndex
}
impl ValidateInstruction for TableGetOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(self.index)?;
        inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![ValueType::RefType(table.ref_type().clone())])
    }
}

pub struct TableSetOperation {
    index: TableIndex
}
impl ValidateInstruction for TableSetOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(self.index)?;
        let _tb_set_value = inputs.pop()?.try_into_ref_type()?;
        let _tbl_index = inputs.pop()?.try_into_num()?.try_into_i32()?;
        if _tb_set_value == *table.ref_type() {
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct TableSizeOperation {
    index: TableIndex
}
impl ValidateInstruction for TableSizeOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _table = ctx.get_table(self.index)?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

pub struct TableGrowOperation {
    index: TableIndex
}
impl ValidateInstruction for TableGrowOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(self.index)?;

        let _tbl_index = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _tb_set_value = inputs.pop()?.try_into_ref_type()?;

        if _tb_set_value == *table.ref_type() {
            Ok(vec![ValueType::Num(NumType::I32)])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct TableFillOperation {
    index: TableIndex
}
impl ValidateInstruction for TableFillOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(self.index)?;

        let _tbl_from = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _tb_set_value = inputs.pop()?.try_into_ref_type()?;
        let _tbl_to = inputs.pop()?.try_into_num()?.try_into_i32()?;

        if _tb_set_value == *table.ref_type() {
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct TableCopyOperation {
    table_x_index: TableIndex,
    table_y_index: TableIndex
}
impl ValidateInstruction for TableCopyOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table_x = ctx.get_table(self.table_x_index)?;
        let table_y = ctx.get_table(self.table_y_index)?;
        if table_x.ref_type() == table_y.ref_type() {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct TableInitOperation {
    table: TableIndex,
    element: ElementIndex
}
impl ValidateInstruction for TableInitOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(self.table)?;
        let element = ctx.get_element(self.element)?;
        if *table.ref_type() == *element {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct ElementDropOperation {
    element: ElementIndex
}
impl ValidateInstruction for ElementDropOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        ctx.get_element(self.element)?;
        Ok(vec![])
    }
}

/**
 * Memory Instructions
 */
// (t).load memarg
pub struct LoadMemoryOperation {
    ty: NumType,
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    args: MemoryArgument,
}
impl ValidateInstruction for LoadMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.ty.bit_width();
        let alignment = 2_u32.pow(self.args.align());
        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![ValueType::Num(self.ty.clone())])
        } else {
            Err(ValidationError::new())
        }
    }
}

// (t).load(N)_(Sign) memarg
// N => 8, 16, 32
pub struct LoadNMemoryOperation {
    ty: NumType,
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    load_n: MemoryLoadNumber,
    _sign: SignType,
    args: MemoryArgument,
}
impl ValidateInstruction for LoadNMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.load_n.bit_width();
        let alignment = 2_u32.pow(self.args.align());
        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![ValueType::Num(self.ty.clone())])
        } else {
            Err(ValidationError::new())
        }
    }
}

// (t).store memarg
pub struct StoreMemoryOperation {
    ty: NumType,
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    args: MemoryArgument,
}
impl ValidateInstruction for StoreMemoryOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.ty.bit_width();
        let alignment = 2_u32.pow(self.args.align());
        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_num()?.try_into_ty(self.ty)?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}


// (t).store(N) memarg
// N => 8, 16, 32
pub struct StoreNMemoryOperation {
    ty: NumType,
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    load_n: MemoryLoadNumber,
    args: MemoryArgument,
}
impl ValidateInstruction for StoreNMemoryOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.load_n.bit_width();
        let alignment = 2_u32.pow(self.args.align());
        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_num()?.try_into_ty(self.ty)?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

// v128.load(NxM)_(sx) memarg
// NxM => 8x8, 16x4, 32x2
pub struct VectorLoadMemoryOperation {
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    op: VectorMemoryOp,
    _sign: SignType,
    args: MemoryArgument
}
impl ValidateInstruction for VectorLoadMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.op.bit_width();
        let alignment = 2_u32.pow(self.args.align());
        if alignment <= bit_width {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![ValueType::VecType(VecType)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// v128.load(N)_splat memarg
// N => 8, 16, 32, 64
pub struct VectorLoadNSplatMemoryOperation {
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    width_n: MemoryWidth,
    args: MemoryArgument
}
impl ValidateInstruction for VectorLoadNSplatMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.width_n.bit_width();
        let alignment = 2_u32.pow(self.args.align());

        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![ValueType::VecType(VecType)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// v128.load(N)_zero memarg
// N => 32, 64
pub struct VectorLoadNZeroMemoryOperation {
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    width_n: MemoryZeroWidth,
    args: MemoryArgument
}
impl ValidateInstruction for VectorLoadNZeroMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.width_n.bit_width();
        let alignment = 2_u32.pow(self.args.align());

        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![ValueType::VecType(VecType)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// v128.load(N)_lane memarg laneidx
// N => 8, 16, 32, 64
pub struct VectorLoadNLaneMemoryOperation {
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    width_n: MemoryWidth,
    index: LaneIndex,
    args: MemoryArgument
}
impl ValidateInstruction for VectorLoadNLaneMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if !self.index.validate_against_memory_width(&self.width_n) {
            return Err(ValidationError::new())
        }

        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.width_n.bit_width();
        let alignment = 2_u32.pow(self.args.align());

        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_vec_type()?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![ValueType::VecType(VecType)])
        } else {
            Err(ValidationError::new())
        }
    }
}

// v128.store(N)_lane memarg laneidx
// N => 8, 16, 32, 64
pub struct VectorStoreNLaneMemoryOperation {
    // This is more for me, 99% of the time this is supposed to be None, as described
    // in the spec.
    memory: Option<MemoryIndex>,
    width_n: MemoryWidth,
    index: LaneIndex,
    args: MemoryArgument
}
impl ValidateInstruction for VectorStoreNLaneMemoryOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if !self.index.validate_against_memory_width(&self.width_n) {
            return Err(ValidationError::new())
        }

        let _memory = ctx.get_memory(self.memory)?;
        let bit_width = self.width_n.bit_width();
        let alignment = 2_u32.pow(self.args.align());

        if alignment <= (bit_width / 8) {
            inputs.pop()?.try_into_vec_type()?;
            inputs.pop()?.try_into_num()?.try_into_i32()?;
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

pub struct MemorySize(Option<MemoryIndex>);
impl ValidateInstruction for MemorySize {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0)?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

pub struct MemoryGrow(Option<MemoryIndex>);
impl ValidateInstruction for MemoryGrow {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

pub struct MemoryFill(Option<MemoryIndex>);
impl ValidateInstruction for MemoryFill {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![])
    }
}

pub struct MemoryCopy(Option<MemoryIndex>);
impl ValidateInstruction for MemoryCopy {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![])
    }
}

pub struct MemoryInit {
    memory: Option<MemoryIndex>,
    data: DataIndex,
}
impl ValidateInstruction for MemoryInit {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.memory)?;
        let _ = ctx.get_data(self.data)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![])
    }
}

pub struct DataDrop {
    data: DataIndex,
}
impl ValidateInstruction for DataDrop {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_data(self.data)?;
        Ok(vec![])
    }
}

/**
 * Control Instructions
 */
pub struct NopOperation;
impl ValidateInstruction for NopOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![])
    }
}

pub struct UnreachableOperation;
impl ValidateInstruction for UnreachableOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, _: &mut Context, input: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // What? I can take all types and return all types?
        // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-unreachable
        // I think what this means is that we don't take any types or values
        // What we really do is consume them all...
        todo!()
    }
}

// block _blocktype_ _instr_* end
pub struct BlockOperation {
    ty: BlockType,
    instructions: InstructionSequence
}
impl ValidateInstruction for BlockOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // Block type must be some function type
        let ty = self.ty.get_function_type(ctx)?;
        // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
        ctx.prepend_label(ty.output().clone());
        // TODO(Alec): Validate how to execute instructions
        let output = self.instructions.validate(ctx, inputs)?;
        // remove the label
        let label = ctx.remove_prepend_label()?;
        // validate the output matches the function output
        if *ty.output() == label && *ty.output().0 == output {
            // This should never trigger
            for input_ty in &ty.input().0 {
                let _ = inputs.pop()?.try_into_value_type(input_ty)?;
            }
            return Ok(ty.output().clone().0)
        }
        Err(ValidationError::new())
    }
}

// loop _blocktype_ _instr_* end
pub struct LoopOperation {
    ty: BlockType,
    instructions: InstructionSequence
}
impl ValidateInstruction for LoopOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // Block type must be some function type
        let ty = self.ty.get_function_type(ctx)?;
        // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
        ctx.prepend_label(ty.output().clone());
        // TODO(Alec): Validate how to execute instructions
        let output = self.instructions.validate(ctx, inputs)?;
        // remove the label
        let label = ctx.remove_prepend_label()?;
        if *ty.output() == label && *ty.output().0 == output {
            // This should never trigger
            for input_ty in &ty.input().0 {
                let _ = inputs.pop()?.try_into_value_type(input_ty)?;
            }
            return Ok(ty.output().clone().0)
        }
        Err(ValidationError::new())
    }
}

// if _blocktype_ _instr_* else _instr_* end
pub struct IfOperation {
    ty: BlockType,
    t1: InstructionSequence,
    t2: InstructionSequence,
}
impl ValidateInstruction for IfOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // Block type must be some function type
        let ty = self.ty.get_function_type(ctx)?;
        // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
        ctx.prepend_label(ty.output().clone());
        // TODO(Alec): Validate how to execute instructions
        let o1 = self.t1.validate(ctx, inputs)?;
        let o2 = self.t2.validate(ctx, inputs)?;
        // remove the label
        let label = ctx.remove_prepend_label()?;
        // validate the output matches the functions output
        if label.0 == o1 && label.0 == o2 && *ty.output().0 == o1 && *ty.output().0 == o2 {
            // validate that the input stack has an i32 and all of of the function types arguments
            let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
            for input_ty in &ty.input().0 {
                let _ = inputs.pop()?.try_into_value_type(input_ty)?;
            }
            return Ok(ty.output().clone().0)
        }
        Err(ValidationError::new())
    }
}

pub struct BrOperation {
    label: LabelIndex
}
impl ValidateInstruction for BrOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = ctx.get_label(self.label)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        for input_ty in &ty.0 {
            let _ = inputs.pop()?.try_into_value_type(input_ty)?;
        }
        Ok(ty.0.clone())
    }
}

pub struct BrIfOperation {
    label: LabelIndex
}
impl ValidateInstruction for BrIfOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = ctx.get_label(self.label)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        for input_ty in &ty.0 {
            let _ = inputs.pop()?.try_into_value_type(input_ty)?;
        }
        Ok(ty.0.clone())
    }
}

////////////////////////////////////////////////////////////////////////////////
/// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-table-l-ast-l-n
pub struct BrTableOperation {
    labels: Vec<LabelIndex>,
    label_n: LabelIndex
}
impl ValidateInstruction for BrTableOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): WHAT DOES IT ALL MEAN? WHAT IS THE MEANING OF LIFE?
        // performs an indirect branch through an operand indexing into the label
        // vector that is an immediate to the instruction, or to a default target
        // if the operand is out of bounds
        let ty_n = ctx.get_label(self.label_n)?;
        // validate the rest of the labels exist
        for label in self.labels.iter() {
            let ty = ctx.get_label(*label)?;
            if ty.0.len() != ty_n.0.len() {
                return Err(ValidationError::new())
            }
            for index in ty.0.len()..0 {
                let value = inputs.0.get(index - inputs.0.len()).ok_or_else(ValidationError::new)?;
                value.try_into_value_type(ty.0.get(index).ok_or_else(ValidationError::new)?)?;
            }
        }
        // There must be enough input as expected in `ty_n`
        if ty_n.0.len() > inputs.0.len() {
            return Err(ValidationError::new())
        }
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        for index in ty_n.0.len()..0 {
            let _ = inputs.pop()?.try_into_value_type(ty_n.0.get(index).unwrap())?;
        }

        Ok(ty_n.0.clone())
    }
}
////////////////////////////////////////////////////////////////////////////////

// Come back and review
// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-return
pub struct ReturnOperation;
impl ValidateInstruction for ReturnOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty_set = ctx.returning().ok_or_else(ValidationError::new)?;
        for ty in &ty_set.0 {
            let _ = inputs.pop()?.try_into_value_type(ty)?;
        }
        Ok(ty_set.0.clone())
    }
}

pub struct CallOperation {
    function: FunctionIndex
}
impl ValidateInstruction for CallOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let func = ctx.get_function(self.function).ok_or_else(ValidationError::new)?;
        // validate function input arguments
        for ty in &func.input().0 {
            let _ = inputs.pop()?.try_into_value_type(ty)?;
        }
        // if all valid, return output arguments
        Ok(func.output().0.clone())
    }
}

pub struct CallIndirectOperation {
    table: TableIndex,
    ty: TypeIndex,
}
impl ValidateInstruction for CallIndirectOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(self.table)?;
        // validate reference type is func ref
        table.ref_type().try_into_func_ref()?;
        // type ty must be defined
        let ty = ctx.get_type(self.ty).ok_or_else(ValidationError::new)?;
        // validate the function
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        for input_ty in &ty.input().0 {
            let _ = inputs.pop()?.try_into_value_type(input_ty)?;
        }
        Ok(ty.output().0.clone())
    }
}