use std::str::FromStr;

use crate::{ast::types::NumberType, lex::LexError};

pub enum Whitespace {
    Space,
    NewLine,
}

pub enum Comment {
    LineComment,
    BlockComment,
}

#[rustfmt::skip]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    //// Number
    I32,
    // I64,
    // F32,
    // F64,
    //// vector
    // V128,
    
    //// reference
    // FuncRef,
    // ExternRef,

    // Any,
    // Eq,
    // I31,
    // Struct,
    // Array,
    // None,
    // NoFunc,
    // NoExn,
    // Exn,
    // NoExtern,
    // Null,

    // HeapShortHand(ReferenceType),

    // VecShape(VectorShape),

    // Extern,
    // Mut,

    // Nop,
    // Unreachable,
    // Drop,
    // Block,
    // Loop,
    // End,
    // Br,
    // BrIf,
    // BrTable,
    // Return,
    // If,
    // Then,
    // Else,
    // Select,
    // Call,
    // CallIndirect,

    //// Local and global get and set
    LocalGet,
    // LocalSet,
    // LocalTee,
    // GlobalGet,
    // GlobalSet,

    // // Table
    // TableGet,
    // TableSet,
    // TableSize,
    // TableGrow,
    // TableFill,
    // TableCopy,
    // TableInit,
    // ElemDrop,

    // // Memory
    // MemorySize,
    // MemoryGrow,
    // MemoryFill,
    // MemoryCopy,
    // MemoryInit,
    // DataDrop,

    // // Load, Store
    // Load(NumType),
    // Store(NumType),

    // LoadN(IntType, SignType, MemoryLoadNumber),

    // I32Store8,
    // I32Store16,
    // I64Store8,
    // I64Store16,
    // I64Store32,

    // MemArgsAlign(String), // (u32),
    // MemArgsOffset(String), // (u32),

    // text block types
    Type,
    Func,
    Param,
    Result,
    Start,
    Local,
    Global,
    Table,
    Memory,
    Elem,
    Data,
    Declare,
    Offset,
    Item,
    Import,
    Export,

    // // const
    // V128Load,
    // V128Store,
    // VecLoad(VectorMemoryOp),
    // VecLoadSplat(MemoryWidth),
    // VecLoadZero(MemoryZeroWidth),
    // VecLoadLane(MemoryWidth),
    // VecStoreLane(MemoryWidth),

    // // const
    Const(NumberType),
    // ConstV128,

    // // Reference
    // RefNull,
    // RefFunc,
    // RefExtern,
    // RefIsNull,

    // // Number Types, Unary
    // IntClz(IntType),
    // IntCtz(IntType),
    // IntPopCnt(IntType),
    // IntExtend8Signed(IntType),
    // IntExtend16Signed(IntType),
    // I64Extend32Signed,

    // NegativeFloat(FloatType),
    // AbsoluteFloat(FloatType),
    // SquareRootFloat(FloatType),
    // CeilFloat(FloatType),
    // FloorFLoat(FloatType),
    // TruncateFloat(FloatType),
    // NearestFloat(FloatType),

    // // Number Types Binary
    Add(NumberType),
    // SubInt(IntType),
    // MultiplyInt(IntType),

    // AndInt(IntType),
    // OrInt(IntType),
    // XORInt(IntType),
    // ShiftLeftInt(IntType),

    // DivideInt { shape: IntType, sign: SignType },
    // RemainderInt { shape: IntType, sign: SignType },
    // ShiftRightInt { shape: IntType, sign: SignType },
    // RotateInt { shape: IntType, direction: Direction },

    // AddFloat(FloatType),
    // SubFloat(FloatType),
    // MultiplyFloat(FloatType),
    // DivideFloat(FloatType),
    // MinFloat(FloatType),
    // MaxFloat(FloatType),
    // CopySign(FloatType),


    // // Number Types Test
    // I32EqualTest,
    // I64EqualTest,

    // // Number Types Compare
    // CompareIntEqual(IntType),
    // CompareIntNotEqual(IntType),
    // CompareIntLessThen { shape: IntType, sign: SignType },
    // CompareIntLessOrEqual { shape: IntType, sign: SignType },
    // CompareIntGreaterThen { shape: IntType, sign: SignType },
    // CompareIntGreaterOrEqual { shape: IntType, sign: SignType },

    // CompareFloatEqual(FloatType),
    // CompareFloatNotEqual(FloatType),
    // CompareFloatLessThen(FloatType),
    // CompareFloatLessOrEqual(FloatType),
    // CompareFloatGreaterThen(FloatType),
    // CompareFloatGreaterOrEqual(FloatType),

    // // Number Types Convert
    // I32WrapI64,
    // I64ExtendI32(SignType),
    // F32DemoteF64,
    // F64PromoteF32,
    // I32TruncateF32(SignType),
    // I64TruncateF32(SignType),
    // I32TruncateF64(SignType),
    // I64TruncateF64(SignType),
    // I32TruncateSatF32(SignType),
    // I64TruncateSatF32(SignType),
    // I32TruncateSatF64(SignType),
    // I64TruncateSatF64(SignType),
    // F32ConvertI32(SignType),
    // F32ConvertI64(SignType),
    // F64ConvertI32(SignType),
    // F64ConvertI64(SignType),
    // F32ReinterpretI32,
    // F64ReinterpretI64,
    // I32ReinterpretF32,
    // I64ReinterpretF64,

    // // Vec Unary
    // V128Not,
    // V128And,
    // V128AndNot,
    // V128Or,
    // V128XOr,
    // V128BitSelect,
    // V128AnyTrue,

    // VecIntNegative(IntegerVectorShape),
    // VecIntAbsolute(IntegerVectorShape),
    // VecI8x16PopCnt,
    // VecI8x16AverageUnsigned,
    // VecI16x8AverageUnsigned,
    
    // VecFloatNegative(FloatVectorShape),
    // VecFloatAbsolute(FloatVectorShape),
    // VecFloatSquareRoot(FloatVectorShape),
    // VecFloatCeil(FloatVectorShape),
    // VecFloatFloor(FloatVectorShape),
    // VecFloatTruncate(FloatVectorShape),
    // VecFloatNearest(FloatVectorShape),

    // I32x4TruncSatF32x4(SignType),
    // I32x4TruncSatF64x2Zero(SignType),
    // F64x2PromoteLowF32x4,
    // F32x4PromoteF64x2Zero,
    // F32x4ConvertI32x4(SignType),
    // F64x2ConvertLowI32x4(SignType),
    // I16x8ExtendAddPairwiseI8x16(SignType),
    // I32x4ExtaddPairwiseI16x8(SignType),

    // // Vec Binary
    // VecIntEqual(IntegerVectorShape),
    // VecIntNotEqual(IntegerVectorShape),
    // VecIntLessThen { shape: IntegerVectorShape, sign: SignType, },
    // VecIntLessOrEqual { shape: IntegerVectorShape, sign: SignType, },
    // VecIntGreaterThen { shape: IntegerVectorShape, sign: SignType, },
    // VecIntGreaterOrEqual { shape: IntegerVectorShape, sign: SignType, },

    // VecEqualFloat(FloatVectorShape),
    // VecNotEqualFloat(FloatVectorShape),
    // VecLessThenFloat(FloatVectorShape),
    // VecLessOrEqualFloat(FloatVectorShape),
    // VecGreaterThenFloat(FloatVectorShape),
    // VecGreaterOrEqualFloat(FloatVectorShape),
    // VecSwizzleFloatI8x16,

    // VecIntAdd(IntegerVectorShape),
    // VecIntSub(IntegerVectorShape),
    // VecIntMultiply(IntegerVectorShape),
    // VecAddSatI16x8(SignType),
    // VecSubtractSatI16x8(SignType),
    // VecI32x4DotProductOfI16x8Signed,

    // VecMinInt { shape: IntegerVectorShape, sign: SignType, },
    // VecMaxInt { shape: IntegerVectorShape, sign: SignType, },
    // VecSubFloat(FloatVectorShape),
    // VecAddSatI8x16(SignType),
    // VecSubtractSatI8x16(SignType),
    // VecAddFloat(FloatVectorShape),
    // VecDivFloat(FloatVectorShape),
    // VecMulFloat(FloatVectorShape),
    // VecMinFloat(FloatVectorShape),
    // VecMaxFloat(FloatVectorShape),
    // VecPMin(FloatVectorShape),
    // VecPMax(FloatVectorShape),
    // I16x8Q15mulrSatS,
    // I8x16NarrowI16x8(SignType),
    // I16x8NarrowI32x4(SignType),

    // // Vec Unary
    // Extend { input: IntegerVectorShape, output: IntegerVectorShape, half: HalfType, sign: SignType },
    // ExtendMultiply { input: IntegerVectorShape, output: IntegerVectorShape, half: HalfType, sign: SignType },

    // // Something
    // VecTest(IntegerVectorShape),
    // VecBitmask(IntegerVectorShape),
    // VecShiftLeft(IntegerVectorShape),
    // VecShiftRight { shape: IntegerVectorShape, sign: SignType },
    // VecShuffle,
    // VecSplat(VectorShape),
    // VecExtract { shape: VectorShape, sign: Option<SignType> },
    // VecReplace(VectorShape),

    // More testing but also keywords
    Module,
    // Bin,
    // Quote,

    // // testing
    // Script,
    // Register,
    // Invoke,
    // Get,
    // AssertMalformed,
    // AssertInvalid,
    // AssertUnlinkable,
    // AssertReturn,
    // AssertTrap,
    // AssertExhaustion,
    // NaNCanonical,
    // NaNArithmetic(String),
    // Infinit,
    // NaN,
    // Input,
    // Output,
}

// impl Visit for Keyword {}

impl Keyword {
    /// Expect no input parameters and one return type that must be of I32.
    pub fn has_unary_return_ty_i32(&self) -> bool {
        // matches!(self, Keyword::Const(NumType::I32))
        return false;
    }

    pub fn is_constant(&self) -> bool {
        use Keyword::*;
        // matches!(self, Const(_) | RefNull | RefFunc | GlobalGet)
        return false;
    }

    pub fn is_block(&self) -> bool {
        use Keyword::*;
        // matches!(self, Block | Loop | If | End | Else | Then)
        return false;
    }

    pub fn is_control(&self) -> bool {
        use Keyword::*;
        // matches!(self, Then | Else | End)
        return false;
    }

    pub fn is_instruction(&self) -> bool {
        use Keyword::*;
        matches!(self, Add(_))
    }
}

#[rustfmt::skip]
impl FromStr for Keyword {
    type Err = LexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // use FloatVectorShape::*;
        // use HalfType::*;
        // use IntegerVectorShape::*;
        use Keyword::*;
        // use SignType::*;
        // use VectorShape::*;
        // use Direction::*;
        Ok(match s {
            // number type
            "i32" => I32,
            // "i64" => I64,
            // "f32" => F32,
            // "f64" => F64,
            // // vector type
            // "v128" => V128,
            // // reference type

            // "any" => Any,
            // "i31" => I31,
            // "struct" => Struct,
            // "array" => Array,
            // "none" => None,
            // "nofunc" => NoFunc,
            // "noexn" => NoExn,
            // "exn" => Exn,
            // "noextern" => NoExtern,
            // "null" => Null,

            // "anyref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Any)),
            // "eqref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Eq)),
            // "i31ref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::I31)),
            // "structref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Struct)),
            // "arrayref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Array)),
            // "nullref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::None)),
            // "funcref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Func)),
            // "nullfuncref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::NoFunc)),
            // "exnref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Exn)),
            // "nullexnref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::NoExn)),
            // "externref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::Extern)),
            // "nullexternref" => HeapShortHand(ReferenceType::shorthand(AbsoluteHeapType::NoExtern)),
            
            // instructions

            // "i8x16" => VecShape(VectorShape::Int(I8x16)),
            // "i16x8" => VecShape(VectorShape::Int(I16x8)),
            // "i32x4" => VecShape(VectorShape::Int(I32x4)),
            // "i64x2" => VecShape(VectorShape::Int(I64x2)),
            // "f32x4" => VecShape(VectorShape::Float(F32x4)),
            // "f64x2" => VecShape(VectorShape::Float(F64x2)),
            
            // "extern" => Extern,
            // "mut" => Mut,

            // "nop" => Nop,
            // "unreachable" => Unreachable,
            // "drop" => Drop,
            // "block" => Block,
            // "loop" => Loop,
            // "end" => End,
            // "br" => Br,
            // "br_if" => BrIf,
            // "br_table" => BrTable,
            // "return" => Return,
            // "if" => If,
            // "then" => Then,
            // "else" => Else,
            // "select" => Select,
            // "call" => Call,
            // "call_indirect" => CallIndirect,

            "local.get" => LocalGet,
            // "local.set" => LocalSet,
            // "local.tee" => LocalTee,
            // "global.get" => GlobalGet,
            // "global.set" => GlobalSet,

            // "table.get" => TableGet,
            // "table.set" => TableSet,
            // "table.size" => TableSize,
            // "table.grow" => TableGrow,
            // "table.fill" => TableFill,
            // "table.copy" => TableCopy,
            // "table.init" => TableInit,
            // "elem.drop" => ElemDrop,

            // "memory.size" => MemorySize,
            // "memory.grow" => MemoryGrow,
            // "memory.fill" => MemoryFill,
            // "memory.copy" => MemoryCopy,
            // "memory.init" => MemoryInit,
            // "data.drop" => DataDrop,

            // "i32.load" => Load(NumType::I32),
            // "i64.load" => Load(NumType::I64),
            // "f32.load" => Load(NumType::F32),
            // "f64.load" => Load(NumType::F64),
            // "i32.store" => Store(NumType::I32),
            // "i64.store" => Store(NumType::I64),
            // "f32.store" => Store(NumType::F32),
            // "f64.store" => Store(NumType::F64),

            // "i32.load8_u" => LoadN(IntType::I32, Unsigned, MemoryLoadNumber::Load8),
            // "i32.load8_s" => LoadN(IntType::I32, Signed, MemoryLoadNumber::Load8),
            // "i32.load16_u" => LoadN(IntType::I32, Unsigned, MemoryLoadNumber::Load16),
            // "i32.load16_s" => LoadN(IntType::I32, Signed, MemoryLoadNumber::Load16),
            // "i64.load8_u" => LoadN(IntType::I64, Unsigned, MemoryLoadNumber::Load8),
            // "i64.load8_s" => LoadN(IntType::I64, Signed, MemoryLoadNumber::Load8),
            // "i64.load16_u" => LoadN(IntType::I64, Unsigned, MemoryLoadNumber::Load16),
            // "i64.load16_s" => LoadN(IntType::I64, Signed, MemoryLoadNumber::Load16),
            // "i64.load32_u" => LoadN(IntType::I64, Unsigned, MemoryLoadNumber::Load32),
            // "i64.load32_s" => LoadN(IntType::I64, Signed, MemoryLoadNumber::Load32),

            // "i32.store8" => I32Store8,
            // "i32.store16" => I32Store16,
            // "i64.store8" => I64Store8,
            // "i64.store16" => I64Store16,
            // "i64.store32" => I64Store32,

            // // v128 load
            // "v128.load" => V128Load,
            // "v128.store" => V128Store,
            // "v128.load8x8_u" => VecLoad(VectorMemoryOp::I8x8(Unsigned)),
            // "v128.load8x8_s" => VecLoad(VectorMemoryOp::I8x8(Signed)),
            // "v128.load16x4_u" => VecLoad(VectorMemoryOp::I16x4(Unsigned)),
            // "v128.load16x4_s" => VecLoad(VectorMemoryOp::I16x4(Signed)),
            // "v128.load32x2_u" => VecLoad(VectorMemoryOp::I32x2(Unsigned)),
            // "v128.load32x2_s" => VecLoad(VectorMemoryOp::I32x2(Signed)),
            // "v128.load8_splat" => VecLoadSplat(MemoryWidth::I8),
            // "v128.load16_splat" => VecLoadSplat(MemoryWidth::I16),
            // "v128.load32_splat" => VecLoadSplat(MemoryWidth::I32),
            // "v128.load64_splat" => VecLoadSplat(MemoryWidth::I64),
            // "v128.load32_zero" => VecLoadZero(MemoryZeroWidth::I32),
            // "v128.load64_zero" => VecLoadZero(MemoryZeroWidth::I64),
            // "v128.load8_lane" => VecLoadLane(MemoryWidth::I8),
            // "v128.load16_lane" => VecLoadLane(MemoryWidth::I16),
            // "v128.load32_lane" => VecLoadLane(MemoryWidth::I32),
            // "v128.load64_lane" => VecLoadLane(MemoryWidth::I64),
            // "v128.store8_lane" => VecStoreLane(MemoryWidth::I8),
            // "v128.store16_lane" => VecStoreLane(MemoryWidth::I16),
            // "v128.store32_lane" => VecStoreLane(MemoryWidth::I32),
            // "v128.store64_lane" => VecStoreLane(MemoryWidth::I64),

            // // const
            "i32.const" => Const(NumberType::I32),
            // "i64.const" => Const(NumType::I64),
            // "f32.const" => Const(NumType::F32),
            // "f64.const" => Const(NumType::F64),
            // "v128.const" => ConstV128,

            // // Reference
            // "ref.null" => RefNull,
            // "ref.func" => RefFunc,
            // "ref.is_null" => RefIsNull,
            // "ref.extern" => RefExtern,

            // // Number unary
            // "i32.clz" => IntClz(IntType::I32),
            // "i32.ctz" => IntCtz(IntType::I32),
            // "i32.popcnt" => IntPopCnt(IntType::I32),
            // "i32.extend8_s" => IntExtend8Signed(IntType::I32),
            // "i32.extend16_s" => IntExtend16Signed(IntType::I32),
            // "i64.clz" => IntClz(IntType::I64),
            // "i64.ctz" => IntCtz(IntType::I64),
            // "i64.popcnt" => IntPopCnt(IntType::I64),
            // "i64.extend8_s" => IntExtend8Signed(IntType::I64),
            // "i64.extend16_s" => IntExtend16Signed(IntType::I64),
            // "i64.extend32_s" => I64Extend32Signed,

            // "f32.neg" => NegativeFloat(FloatType::F32),
            // "f32.abs" => AbsoluteFloat(FloatType::F32),
            // "f32.sqrt" => SquareRootFloat(FloatType::F32),
            // "f32.ceil" => CeilFloat(FloatType::F32),
            // "f32.floor" => FloorFLoat(FloatType::F32),
            // "f32.trunc" => TruncateFloat(FloatType::F32),
            // "f32.nearest" => NearestFloat(FloatType::F32),
            // "f64.neg" => NegativeFloat(FloatType::F64),
            // "f64.abs" => AbsoluteFloat(FloatType::F64),
            // "f64.sqrt" => SquareRootFloat(FloatType::F64),
            // "f64.ceil" => CeilFloat(FloatType::F64),
            // "f64.floor" => FloorFLoat(FloatType::F64),
            // "f64.trunc" => TruncateFloat(FloatType::F64),
            // "f64.nearest" => NearestFloat(FloatType::F64),

            // Number Binary
            "i32.add" => Add(NumberType::I32),
            // "i32.sub" => SubInt(IntType::I32),
            // "i32.mul" => MultiplyInt(IntType::I32),
            // "i32.div_u" => DivideInt { shape: IntType::I32, sign: Unsigned },
            // "i32.div_s" => DivideInt { shape: IntType::I32, sign: Signed },
            // "i32.rem_u" => RemainderInt { shape: IntType::I32, sign: Unsigned },
            // "i32.rem_s" => RemainderInt { shape: IntType::I32, sign: Signed },
            // "i32.and" => AndInt(IntType::I32),
            // "i32.or" => OrInt(IntType::I32),
            // "i32.xor" => XORInt(IntType::I32),
            // "i32.shl" => ShiftLeftInt(IntType::I32),
            // "i32.shr_u" => ShiftRightInt { shape: IntType::I32, sign: Unsigned },
            // "i32.shr_s" => ShiftRightInt { shape: IntType::I32, sign: Signed },
            // "i32.rotl" => RotateInt { shape: IntType::I32, direction: Left },
            // "i32.rotr" => RotateInt { shape: IntType::I32, direction: Right },
            "i64.add" => Add(NumberType::I64),
            // "i64.sub" => SubInt(IntType::I64),
            // "i64.mul" => MultiplyInt(IntType::I64),
            // "i64.div_u" => DivideInt { shape: IntType::I64, sign: Unsigned },
            // "i64.div_s" => DivideInt { shape: IntType::I64, sign: Signed },
            // "i64.rem_u" => RemainderInt { shape: IntType::I64, sign: Unsigned },
            // "i64.rem_s" => RemainderInt { shape: IntType::I64, sign: Signed },
            // "i64.and" => AndInt(IntType::I64),
            // "i64.or" => OrInt(IntType::I64),
            // "i64.xor" => XORInt(IntType::I64),
            // "i64.shl" => ShiftLeftInt(IntType::I64),
            // "i64.shr_u" => ShiftRightInt { shape: IntType::I64, sign: Unsigned },
            // "i64.shr_s" => ShiftRightInt { shape: IntType::I64, sign: Signed },
            // "i64.rotl" => RotateInt { shape: IntType::I64, direction: Left },
            // "i64.rotr" => RotateInt { shape: IntType::I64, direction: Right },
      
            // "f32.add" => AddFloat(FloatType::F32),
            // "f32.sub" => SubFloat(FloatType::F32),
            // "f32.mul" => MultiplyFloat(FloatType::F32),
            // "f32.div" => DivideFloat(FloatType::F32),
            // "f32.min" => MinFloat(FloatType::F32),
            // "f32.max" => MaxFloat(FloatType::F32),
            // "f32.copysign" => CopySign(FloatType::F32),
            // "f64.add" => AddFloat(FloatType::F64),
            // "f64.sub" => SubFloat(FloatType::F64),
            // "f64.mul" => MultiplyFloat(FloatType::F64),
            // "f64.div" => DivideFloat(FloatType::F64),
            // "f64.min" => MinFloat(FloatType::F64),
            // "f64.max" => MaxFloat(FloatType::F64),
            // "f64.copysign" => CopySign(FloatType::F64),

            // Number Test
            // "i32.eqz" => I32EqualTest,
            // "i64.eqz" => I64EqualTest,

            // Number Type, Convert
            // "i32.eq" => CompareIntEqual(IntType::I32),
            // "i32.ne" => CompareIntNotEqual(IntType::I32),
            // "i32.lt_u" => CompareIntLessThen { shape: IntType::I32, sign: Unsigned },
            // "i32.lt_s" => CompareIntLessThen { shape: IntType::I32, sign: Signed },
            // "i32.le_u" => CompareIntLessOrEqual { shape: IntType::I32, sign: Unsigned },
            // "i32.le_s" => CompareIntLessOrEqual { shape: IntType::I32, sign: Signed },
            // "i32.gt_u" => CompareIntGreaterThen { shape: IntType::I32, sign: Unsigned },
            // "i32.gt_s" => CompareIntGreaterThen { shape: IntType::I32, sign: Signed },
            // "i32.ge_u" => CompareIntGreaterOrEqual { shape: IntType::I32, sign: Unsigned },
            // "i32.ge_s" => CompareIntGreaterOrEqual { shape: IntType::I32, sign: Signed },
            // "i64.eq" => CompareIntEqual(IntType::I64),
            // "i64.ne" => CompareIntNotEqual(IntType::I64),
            // "i64.lt_u" => CompareIntLessThen { shape: IntType::I64, sign: Unsigned },
            // "i64.lt_s" => CompareIntLessThen { shape: IntType::I64, sign: Signed },
            // "i64.le_u" => CompareIntLessOrEqual { shape: IntType::I64, sign: Unsigned },
            // "i64.le_s" => CompareIntLessOrEqual { shape: IntType::I64, sign: Signed },
            // "i64.gt_u" => CompareIntGreaterThen { shape: IntType::I64, sign: Unsigned },
            // "i64.gt_s" => CompareIntGreaterThen { shape: IntType::I64, sign: Signed },
            // "i64.ge_u" => CompareIntGreaterOrEqual { shape: IntType::I64, sign: Unsigned },
            // "i64.ge_s" => CompareIntGreaterOrEqual { shape: IntType::I64, sign: Signed },
      
            // "f32.eq" => CompareFloatEqual(FloatType::F32),
            // "f32.ne" => CompareFloatNotEqual(FloatType::F32),
            // "f32.lt" => CompareFloatLessThen(FloatType::F32),
            // "f32.le" => CompareFloatLessOrEqual(FloatType::F32),
            // "f32.gt" => CompareFloatGreaterThen(FloatType::F32),
            // "f32.ge" => CompareFloatGreaterOrEqual(FloatType::F32),
            // "f64.eq" => CompareFloatEqual(FloatType::F64),
            // "f64.ne" => CompareFloatNotEqual(FloatType::F64),
            // "f64.lt" => CompareFloatLessThen(FloatType::F64),
            // "f64.le" => CompareFloatLessOrEqual(FloatType::F64),
            // "f64.gt" => CompareFloatGreaterThen(FloatType::F64),
            // "f64.ge" => CompareFloatGreaterOrEqual(FloatType::F64),

            // "i32.wrap_i64" => I32WrapI64,
            // "i64.extend_i32_s" => I64ExtendI32(Signed),
            // "i64.extend_i32_u" => I64ExtendI32(Unsigned),
            // "f32.demote_f64" => F32DemoteF64,
            // "f64.promote_f32" => F64PromoteF32,
            // "i32.trunc_f32_u" => I32TruncateF32(Unsigned),
            // "i32.trunc_f32_s" => I32TruncateF32(Signed),
            // "i64.trunc_f32_u" => I64TruncateF32(Unsigned),
            // "i64.trunc_f32_s" => I64TruncateF32(Signed),
            // "i32.trunc_f64_u" => I32TruncateF64(Unsigned),
            // "i32.trunc_f64_s" => I32TruncateF64(Signed),
            // "i64.trunc_f64_u" => I64TruncateF64(Unsigned),
            // "i64.trunc_f64_s" => I64TruncateF64(Signed),
            // "i32.trunc_sat_f32_u" => I32TruncateSatF32(Unsigned),
            // "i32.trunc_sat_f32_s" => I32TruncateSatF32(Signed),
            // "i64.trunc_sat_f32_u" => I64TruncateSatF32(Unsigned),
            // "i64.trunc_sat_f32_s" => I64TruncateSatF32(Signed),
            // "i32.trunc_sat_f64_u" => I32TruncateSatF64(Unsigned),
            // "i32.trunc_sat_f64_s" => I32TruncateSatF64(Signed),
            // "i64.trunc_sat_f64_u" => I64TruncateSatF64(Unsigned),
            // "i64.trunc_sat_f64_s" => I64TruncateSatF64(Signed),
            // "f32.convert_i32_u" => F32ConvertI32(Unsigned),
            // "f32.convert_i32_s" => F32ConvertI32(Signed),
            // "f64.convert_i32_u" => F32ConvertI64(Unsigned),
            // "f64.convert_i32_s" => F32ConvertI64(Signed),
            // "f32.convert_i64_u" => F64ConvertI32(Unsigned),
            // "f32.convert_i64_s" => F64ConvertI32(Signed),
            // "f64.convert_i64_u" => F64ConvertI64(Unsigned),
            // "f64.convert_i64_s" => F64ConvertI64(Signed),
            // "f32.reinterpret_i32" => F32ReinterpretI32,
            // "f64.reinterpret_i64" => F64ReinterpretI64,
            // "i32.reinterpret_f32" => I32ReinterpretF32,
            // "i64.reinterpret_f64" => I64ReinterpretF64,
      
            // // Vec Unary
            // "v128.not" => V128Not,
            // "v128.and" => V128And,
            // "v128.andnot" => V128AndNot,
            // "v128.or" => V128Or,
            // "v128.xor" => V128XOr,
            // "v128.bitselect" => V128BitSelect,
            // "v128.any_true" => V128AnyTrue,

            // "i8x16.neg" => VecIntNegative(I8x16),
            // "i16x8.neg" => VecIntNegative(I16x8),
            // "i32x4.neg" => VecIntNegative(I32x4),
            // "i64x2.neg" => VecIntNegative(I64x2),
            // "i8x16.abs" => VecIntAbsolute(I8x16),
            // "i16x8.abs" => VecIntAbsolute(I16x8),
            // "i32x4.abs" => VecIntAbsolute(I32x4),
            // "i64x2.abs" => VecIntAbsolute(I64x2),
            // "i8x16.popcnt" => VecI8x16PopCnt,
            // "i8x16.avgr_u" => VecI8x16AverageUnsigned,
            // "i16x8.avgr_u" => VecI16x8AverageUnsigned,
            
            // "f32x4.neg" => VecFloatNegative(F32x4),
            // "f64x2.neg" => VecFloatNegative(F64x2),
            // "f32x4.abs" => VecFloatAbsolute(F32x4),
            // "f64x2.abs" => VecFloatAbsolute(F64x2),
            // "f32x4.sqrt" => VecFloatSquareRoot(F32x4),
            // "f64x2.sqrt" => VecFloatSquareRoot(F64x2),
            // "f32x4.ceil" => VecFloatCeil(F32x4),
            // "f64x2.ceil" => VecFloatCeil(F64x2),
            // "f32x4.floor" => VecFloatFloor(F32x4),
            // "f64x2.floor" => VecFloatFloor(F64x2),
            // "f32x4.trunc" => VecFloatTruncate(F32x4),
            // "f64x2.trunc" => VecFloatTruncate(F64x2),
            // "f32x4.nearest" => VecFloatNearest(F32x4),
            // "f64x2.nearest" => VecFloatNearest(F64x2),

            // "i32x4.trunc_sat_f32x4_u" => I32x4TruncSatF32x4(Unsigned),
            // "i32x4.trunc_sat_f32x4_s" => I32x4TruncSatF32x4(Signed),
            // "i32x4.trunc_sat_f64x2_u_zero" => I32x4TruncSatF64x2Zero(Unsigned),
            // "i32x4.trunc_sat_f64x2_s_zero" => I32x4TruncSatF64x2Zero(Signed),
            // "f64x2.promote_low_f32x4" => F64x2PromoteLowF32x4,
            // "f32x4.demote_f64x2_zero" => F32x4PromoteF64x2Zero,
            // "f32x4.convert_i32x4_u" => F32x4ConvertI32x4(Unsigned),
            // "f32x4.convert_i32x4_s" => F32x4ConvertI32x4(Signed),
            // "f64x2.convert_low_i32x4_u" => F64x2ConvertLowI32x4(Unsigned),
            // "f64x2.convert_low_i32x4_s" => F64x2ConvertLowI32x4(Signed),
            // "i16x8.extadd_pairwise_i8x16_u" => I16x8ExtendAddPairwiseI8x16(Unsigned),
            // "i16x8.extadd_pairwise_i8x16_s" => I16x8ExtendAddPairwiseI8x16(Signed),
            // "i32x4.extadd_pairwise_i16x8_u" => I32x4ExtaddPairwiseI16x8(Unsigned),
            // "i32x4.extadd_pairwise_i16x8_s" => I32x4ExtaddPairwiseI16x8(Signed),

            // // Vector Binary
            // "i8x16.eq" => VecIntEqual (I8x16),
            // "i16x8.eq" => VecIntEqual (I16x8),
            // "i32x4.eq" => VecIntEqual (I32x4),
            // "i64x2.eq" => VecIntEqual (I64x2),
            // "i8x16.ne" => VecIntNotEqual (I8x16),
            // "i16x8.ne" => VecIntNotEqual (I16x8),
            // "i32x4.ne" => VecIntNotEqual (I32x4),
            // "i64x2.ne" => VecIntNotEqual (I64x2),
            // "i8x16.lt_u" => VecIntLessThen { shape: I8x16, sign: Unsigned },
            // "i8x16.lt_s" => VecIntLessThen { shape: I8x16, sign: Signed },
            // "i16x8.lt_u" => VecIntLessThen { shape: I16x8, sign: Unsigned },
            // "i16x8.lt_s" => VecIntLessThen { shape: I16x8, sign: Signed },
            // "i32x4.lt_u" => VecIntLessThen { shape: I32x4, sign: Unsigned },
            // "i32x4.lt_s" => VecIntLessThen { shape: I32x4, sign: Signed },
            // "i64x2.lt_s" => VecIntLessThen { shape: I64x2, sign: Signed },
            // "i8x16.le_u" => VecIntLessOrEqual { shape: I8x16, sign: Unsigned },
            // "i8x16.le_s" => VecIntLessOrEqual { shape: I8x16, sign: Signed },
            // "i16x8.le_u" => VecIntLessOrEqual { shape: I16x8, sign: Unsigned },
            // "i16x8.le_s" => VecIntLessOrEqual { shape: I16x8, sign: Signed },
            // "i32x4.le_u" => VecIntLessOrEqual { shape: I32x4, sign: Unsigned },
            // "i32x4.le_s" => VecIntLessOrEqual { shape: I32x4, sign: Signed },
            // "i64x2.le_s" => VecIntLessOrEqual { shape: I64x2, sign: Signed },
            // "i8x16.gt_u" => VecIntGreaterThen { shape: I8x16, sign: Unsigned },
            // "i8x16.gt_s" => VecIntGreaterThen { shape: I8x16, sign: Signed },
            // "i16x8.gt_u" => VecIntGreaterThen { shape: I16x8, sign: Unsigned },
            // "i16x8.gt_s" => VecIntGreaterThen { shape: I16x8, sign: Signed },
            // "i32x4.gt_u" => VecIntGreaterThen { shape: I32x4, sign: Unsigned },
            // "i32x4.gt_s" => VecIntGreaterThen { shape: I32x4, sign: Signed },
            // "i64x2.gt_s" => VecIntGreaterThen { shape: I64x2, sign: Signed },
            // "i8x16.ge_u" => VecIntGreaterOrEqual { shape: I8x16, sign: Unsigned },
            // "i8x16.ge_s" => VecIntGreaterOrEqual { shape: I8x16, sign: Signed },
            // "i16x8.ge_u" => VecIntGreaterOrEqual { shape: I16x8, sign: Unsigned },
            // "i16x8.ge_s" => VecIntGreaterOrEqual { shape: I16x8, sign: Signed },
            // "i32x4.ge_u" => VecIntGreaterOrEqual { shape: I32x4, sign: Unsigned },
            // "i32x4.ge_s" => VecIntGreaterOrEqual { shape: I32x4, sign: Signed },
            // "i64x2.ge_s" => VecIntGreaterOrEqual { shape: I64x2, sign: Signed },

            // "f32x4.eq" => VecEqualFloat(F32x4),
            // "f64x2.eq" => VecEqualFloat(F64x2),
            // "f32x4.ne" => VecNotEqualFloat(F32x4),
            // "f64x2.ne" => VecNotEqualFloat(F64x2),
            // "f32x4.lt" => VecLessThenFloat(F32x4),
            // "f64x2.lt" => VecLessThenFloat(F64x2),
            // "f32x4.le" => VecLessOrEqualFloat(F32x4),
            // "f64x2.le" => VecLessOrEqualFloat(F64x2),
            // "f32x4.gt" => VecGreaterThenFloat(F32x4),
            // "f64x2.gt" => VecGreaterThenFloat(F64x2),
            // "f32x4.ge" => VecGreaterOrEqualFloat(F32x4),
            // "f64x2.ge" => VecGreaterOrEqualFloat(F64x2),
            // "i8x16.swizzle" => VecSwizzleFloatI8x16,

            // "i8x16.add" => VecIntAdd(I8x16),
            // "i16x8.add" => VecIntAdd(I16x8),
            // "i32x4.add" => VecIntAdd(I32x4),
            // "i64x2.add" => VecIntAdd(I64x2),
            // "i8x16.sub" => VecIntSub(I8x16),
            // "i16x8.sub" => VecIntSub(I16x8),
            // "i32x4.sub" => VecIntSub(I32x4),
            // "i64x2.sub" => VecIntSub(I64x2),
            // "i16x8.mul" => VecIntMultiply(I16x8),
            // "i32x4.mul" => VecIntMultiply(I32x4),
            // "i64x2.mul" => VecIntMultiply(I64x2),
            // "i8x16.add_sat_u" => VecAddSatI8x16(Unsigned),
            // "i8x16.add_sat_s" => VecAddSatI8x16(Signed),
            // "i16x8.add_sat_u" => VecAddSatI16x8(Unsigned),
            // "i16x8.add_sat_s" => VecAddSatI16x8(Signed),
            // "i8x16.sub_sat_u" => VecSubtractSatI8x16(Unsigned),
            // "i8x16.sub_sat_s" => VecSubtractSatI8x16(Signed),
            // "i16x8.sub_sat_u" => VecSubtractSatI16x8(Unsigned),
            // "i16x8.sub_sat_s" => VecSubtractSatI16x8(Signed),
            // "i32x4.dot_i16x8_s" => VecI32x4DotProductOfI16x8Signed,

            // "i8x16.min_u" => VecMinInt { shape: I8x16, sign: Unsigned },
            // "i16x8.min_u" => VecMinInt { shape: I16x8, sign: Unsigned },
            // "i32x4.min_u" => VecMinInt { shape: I32x4, sign: Unsigned },
            // "i8x16.min_s" => VecMinInt { shape: I8x16, sign: Signed },
            // "i16x8.min_s" => VecMinInt { shape: I16x8, sign: Signed },
            // "i32x4.min_s" => VecMinInt { shape: I32x4, sign: Signed },
            // "i8x16.max_u" => VecMaxInt { shape: I8x16, sign: Unsigned },
            // "i16x8.max_u" => VecMaxInt { shape: I16x8, sign: Unsigned },
            // "i32x4.max_u" => VecMaxInt { shape: I32x4, sign: Unsigned },
            // "i8x16.max_s" => VecMaxInt { shape: I8x16, sign: Signed },
            // "i16x8.max_s" => VecMaxInt { shape: I16x8, sign: Signed },
            // "i32x4.max_s" => VecMaxInt { shape: I32x4, sign: Signed },

            // "f32x4.add" => VecAddFloat(F32x4),
            // "f64x2.add" => VecAddFloat(F64x2),
            // "f32x4.sub" => VecSubFloat(F32x4),
            // "f64x2.sub" => VecSubFloat(F64x2),
            // "f32x4.mul" => VecMulFloat(F32x4),
            // "f64x2.mul" => VecMulFloat(F64x2),
            // "f32x4.div" => VecDivFloat(F32x4),
            // "f64x2.div" => VecDivFloat(F64x2),

            // "f32x4.min" => VecMinFloat(F32x4),
            // "f64x2.min" => VecMinFloat(F64x2),
            // "f32x4.max" => VecMaxFloat(F32x4),
            // "f64x2.max" => VecMaxFloat(F64x2),
            // "f32x4.pmin" => VecPMin(F32x4),
            // "f64x2.pmin" => VecPMin(F64x2),
            // "f32x4.pmax" => VecPMax(F32x4),
            // "f64x2.pmax" => VecPMax(F64x2),

            // "i16x8.q15mulr_sat_s" => I16x8Q15mulrSatS,
            // "i8x16.narrow_i16x8_u" => I8x16NarrowI16x8(Unsigned),
            // "i8x16.narrow_i16x8_s" => I8x16NarrowI16x8(Signed),
            // "i16x8.narrow_i32x4_u" => I16x8NarrowI32x4(Unsigned),
            // "i16x8.narrow_i32x4_s" => I16x8NarrowI32x4(Signed),

            // // Vectory unary
            // "i16x8.extend_low_i8x16_u" => Extend { input: I16x8, output: I8x16, half: Low, sign: Unsigned },
            // "i16x8.extend_low_i8x16_s" => Extend { input: I16x8, output: I8x16, half: Low, sign: Signed },
            // "i16x8.extend_high_i8x16_u" => Extend { input: I16x8, output: I8x16, half: High, sign: Unsigned },
            // "i16x8.extend_high_i8x16_s" => Extend { input: I16x8, output: I8x16, half: High, sign: Signed },
            // "i32x4.extend_low_i16x8_u" => Extend { input: I32x4, output: I16x8, half: Low, sign: Unsigned },
            // "i32x4.extend_low_i16x8_s" => Extend { input: I32x4, output: I16x8, half: Low, sign: Signed },
            // "i32x4.extend_high_i16x8_u" => Extend { input: I32x4, output: I16x8, half: High, sign: Unsigned },
            // "i32x4.extend_high_i16x8_s" => Extend { input: I32x4, output: I16x8, half: High, sign: Signed },
            // "i64x2.extend_low_i32x4_u" => Extend { input: I64x2, output: I32x4, half: Low, sign: Unsigned },
            // "i64x2.extend_low_i32x4_s" => Extend { input: I64x2, output: I32x4, half: Low, sign: Signed },
            // "i64x2.extend_high_i32x4_u" => Extend { input: I64x2, output: I32x4, half: High, sign: Unsigned },
            // "i64x2.extend_high_i32x4_s" => Extend { input: I64x2, output: I32x4, half: High, sign: Signed },
            // "i16x8.extmul_low_i8x16_u" => ExtendMultiply { input: I16x8, output: I8x16, half: Low, sign: Unsigned },
            // "i16x8.extmul_low_i8x16_s" => ExtendMultiply { input: I16x8, output: I8x16, half: Low, sign: Signed },
            // "i16x8.extmul_high_i8x16_u" => ExtendMultiply { input: I16x8, output: I8x16, half: High, sign: Unsigned },
            // "i16x8.extmul_high_i8x16_s" => ExtendMultiply { input: I16x8, output: I8x16, half: High, sign: Signed },
            // "i32x4.extmul_low_i16x8_u" => ExtendMultiply { input: I32x4, output: I16x8, half: Low, sign: Unsigned },
            // "i32x4.extmul_low_i16x8_s" => ExtendMultiply { input: I32x4, output: I16x8, half: Low, sign: Signed },
            // "i32x4.extmul_high_i16x8_u" => ExtendMultiply { input: I32x4, output: I16x8, half: High, sign: Unsigned },
            // "i32x4.extmul_high_i16x8_s" => ExtendMultiply { input: I32x4, output: I16x8, half: High, sign: Signed },
            // "i64x2.extmul_low_i32x4_u" => ExtendMultiply { input: I64x2, output: I32x4, half: Low, sign: Unsigned },
            // "i64x2.extmul_low_i32x4_s" => ExtendMultiply { input: I64x2, output: I32x4, half: Low, sign: Signed },
            // "i64x2.extmul_high_i32x4_u" => ExtendMultiply { input: I64x2, output: I32x4, half: High, sign: Unsigned },
            // "i64x2.extmul_high_i32x4_s" => ExtendMultiply { input: I64x2, output: I32x4, half: High, sign: Signed },

            // // More vector
            // "i8x16.all_true" => VecTest(I8x16),
            // "i16x8.all_true" => VecTest(I16x8),
            // "i32x4.all_true" => VecTest(I32x4),
            // "i64x2.all_true" => VecTest(I64x2),
            // "i8x16.bitmask" => VecBitmask(I8x16),
            // "i16x8.bitmask" => VecBitmask(I16x8),
            // "i32x4.bitmask" => VecBitmask(I32x4),
            // "i64x2.bitmask" => VecBitmask(I64x2),
            // "i8x16.shl" => VecShiftLeft(I8x16),
            // "i16x8.shl" => VecShiftLeft(I16x8),
            // "i32x4.shl" => VecShiftLeft(I32x4),
            // "i64x2.shl" => VecShiftLeft(I64x2),
            // "i8x16.shr_u" => VecShiftRight { shape: I8x16, sign: Unsigned },
            // "i8x16.shr_s" => VecShiftRight { shape: I8x16, sign: Signed },
            // "i16x8.shr_u" => VecShiftRight { shape: I16x8, sign: Unsigned },
            // "i16x8.shr_s" => VecShiftRight { shape: I16x8, sign: Signed },
            // "i32x4.shr_u" => VecShiftRight { shape: I32x4, sign: Unsigned },
            // "i32x4.shr_s" => VecShiftRight { shape: I32x4, sign: Signed },
            // "i64x2.shr_u" => VecShiftRight { shape: I64x2, sign: Unsigned },
            // "i64x2.shr_s" => VecShiftRight { shape: I64x2, sign: Signed },
            // "i8x16.shuffle" => VecShuffle,

            // splat and extract_lane
            // "i8x16.splat" => VecSplat(Int(I8x16)),
            // "i16x8.splat" => VecSplat(Int(I16x8)),
            // "i32x4.splat" => VecSplat(Int(I32x4)),
            // "i64x2.splat" => VecSplat(Int(I64x2)),
            // "f32x4.splat" => VecSplat(Float(F32x4)),
            // "f64x2.splat" => VecSplat(Float(F64x2)),
            // "i8x16.extract_lane_u" => VecExtract { shape: Int(I8x16), sign: Some(Unsigned) },
            // "i8x16.extract_lane_s" => VecExtract { shape: Int(I8x16), sign: Some(Signed) },
            // "i16x8.extract_lane_u" => VecExtract { shape: Int(I16x8), sign: Some(Unsigned) },
            // "i16x8.extract_lane_s" => VecExtract { shape: Int(I16x8), sign: Some(Signed) },
            // "i32x4.extract_lane" => VecExtract { shape: Int(I32x4), sign: Option::None },
            // "i64x2.extract_lane" => VecExtract { shape: Int(I64x2), sign: Option::None },
            // "f32x4.extract_lane" => VecExtract { shape: Float(F32x4), sign: Option::None },
            // "f64x2.extract_lane" => VecExtract { shape: Float(F64x2), sign: Option::None },
            // "i8x16.replace_lane" => VecReplace(Int(I8x16)),
            // "i16x8.replace_lane" => VecReplace(Int(I16x8)),
            // "i32x4.replace_lane" => VecReplace(Int(I32x4)),
            // "i64x2.replace_lane" => VecReplace(Int(I64x2)),
            // "f32x4.replace_lane" => VecReplace(Float(F32x4)),
            // "f64x2.replace_lane" => VecReplace(Float(F64x2)),

            // Block types
            "type" => Type,
            "func" => Func,
            "param" => Param,
            "result" => Result,
            "start" => Start,
            "local" => Local,
            "global" => Global,
            "table" => Table,
            "memory" => Memory,
            "elem" => Elem,
            "data" => Data,
            "declare" => Declare,
            "offset" => Offset,
            "item" => Item,
            "import" => Import,
            "export" => Export,

            // More testing
            "module" => Module,
            // "binary" => Bin,
            // "quote" => Quote,

            // Enabled for testing
            // "script" => Script,
            // "register" => Register,
            // "invoke" => Invoke,
            // "get" => Get,
            // "assert_malformed" => AssertMalformed,
            // "assert_invalid" => AssertInvalid,
            // "assert_unlinkable" => AssertUnlinkable,
            // "assert_return" => AssertReturn,
            // "assert_trap" => AssertTrap,
            // "assert_exhaustion" => AssertExhaustion,
            // "nan:canonical" => NaNCanonical,
            // "inf" => Infinit,
            // "nan" => NaN,
            // "input" => Input,
            // "output" => Output,
            str if str.starts_with("nan:") => {
                let amount = match str.split(':').nth(1) {
                    Some(amount) => amount,
                    Option::None => {
                        return Err(LexError::new(format!("Parsing `nan:`. Determine to be not be a number. Failed to parse {}", str)))
                    },
                };
                todo!("first todo")
                // NaNArithmetic(amount.to_string())
            }
            str if str.starts_with("align=") =>  {
                let amount = match str.split('=').nth(1) {
                    Some(amount) => amount, // Parse Hex Number
                    Option::None => {
                        return Err(LexError::new(format!("Parsing `align` of memory args was determine to be not be a number. Failed to parse {}", str)))
                    },
                };
                todo!("second todo")
                // MemArgsAlign(amount.to_string())
            }
            str if str.starts_with("offset=") =>  {
                let amount = match str.split('=').nth(1) {
                    Some(amount) => amount,
                    Option::None => {
                        return Err(LexError::new(format!("Parsing `offset` of memory args was determine to be not be a number. Failed to parse {}", str)))
                    },
                };
                todo!("third todo")
                // MemArgsOffset(amount.to_string())
            }
            str => return Err(LexError::new(format!("{} is not a keyword", str))),
        })
    }
}
