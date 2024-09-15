pub mod ast;

use std::{iter::Peekable, str::FromStr};

use ast::Error;

use crate::structure::types::{
    Direction, FloatType, FloatVectorShape, HalfType, IntType, IntegerVectorShape, MemoryWidth,
    MemoryZeroWidth, NumType, SignType, VectorShape,
};

#[derive(Debug)]
pub struct Location {
    index: usize,
    line: usize,
    column: usize,
}

#[derive(Debug)]
pub struct ParseError {
    // spec_error
    pub location: Option<Location>,
    pub error: String,
    tokens: Option<Vec<Token>>,
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Scan Error: {:?} at location {:?}",
            self.error, self.location
        )
    }
}

impl From<ParseError> for Error {
    fn from(value: ParseError) -> Self {
        ParseError::into(value)
    }
}

impl ParseError {
    pub fn new(str: impl Into<String>) -> Self {
        Self {
            error: str.into(),
            location: None,
            tokens: None,
        }
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = Some(location);
        self
    }

    pub fn with_tokens(mut self, token: Vec<Token>) -> Self {
        self.tokens = Some(token);
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Number,
    String,
    Id,
    Reserved,
    Keyword(Keyword),
}

#[rustfmt::skip]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    // Number
    I32,
    I64,
    F32,
    F64,
    // vector
    V128,
    // reference
    FuncRef,
    ExternRef,

    VecShape(VectorShape),

    Extern,
    Mut,

    Nop,
    Unreachable,
    Drop,
    Block,
    Loop,
    End,
    Br,
    BrIf,
    BrTable,
    Return,
    If,
    Then,
    Else,
    Select,
    Call,
    CallIndirect,

    // Local and global get and set
    LocalGet,
    LocalSet,
    LocalTee,
    GlobalGet,
    GlobalSet,

    // Table
    TableGet,
    TableSet,
    TableSize,
    TableGrow,
    TableFill,
    TableCopy,
    TableInit,
    ElemDrop,

    // Memory
    MemorySize,
    MemoryGrow,
    MemoryFill,
    MemoryCopy,
    MemoryInit,
    DataDrop,

    // Load, Store
    Load(NumType),
    Store(NumType),

    I32Load8(SignType),
    I32Load16(SignType),
    I64Load8(SignType),
    I64Load16(SignType),
    I64Load32(SignType),

    I32Store8,
    I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,

    MemArgsAlign(String), // (u32),
    MemArgsOffset(String), // (u32),

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

    // const
    V128Load,
    V128Store,
    VecLoad8x8(SignType),
    VecLoad16x4(SignType),
    VecLoad32x2(SignType),
    VecLoadSplat(MemoryWidth),
    VecLoadZero(MemoryZeroWidth),
    VecLoadLane(MemoryWidth),
    VecStoreLane(MemoryWidth),

    // const
    Const(NumType),
    ConstV128,

    // Reference
    RefNull,
    RefFunc,
    RefExtern,
    RefIsNull,

    // Number Types, Unary
    IntClz(IntType),
    IntCtz(IntType),
    IntPopCnt(IntType),
    IntExtend8Signed(IntType),
    IntExtend16Signed(IntType),
    I64Extend32Signed,

    NegativeFloat(FloatType),
    AbsoluteFloat(FloatType),
    SquareRootFloat(FloatType),
    CeilFloat(FloatType),
    FloorFLoat(FloatType),
    TruncateFloat(FloatType),
    NearestFloat(FloatType),

    // Number Types Binary
    AddInt(IntType),
    SubInt(IntType),
    MultiplyInt(IntType),

    AndInt(IntType),
    OrInt(IntType),
    XORInt(IntType),
    ShiftLeftInt(IntType),

    DivideInt { shape: IntType, sign: SignType },
    RemainderInt { shape: IntType, sign: SignType },
    ShiftRightInt { shape: IntType, sign: SignType },
    RotateInt { shape: IntType, direction: Direction },

    AddFloat(FloatType),
    SubFloat(FloatType),
    MultiplyFloat(FloatType),
    DivideFloat(FloatType),
    MinFloat(FloatType),
    MaxFloat(FloatType),
    CopySign(FloatType),


    // Number Types Test
    I32EqualTest,
    I64EqualTest,

    // Number Types Compare
    CompareIntEqual(IntType),
    CompareIntNotEqual(IntType),
    CompareIntLessThen { shape: IntType, sign: SignType },
    CompareIntLessOrEqual { shape: IntType, sign: SignType },
    CompareIntGreaterThen { shape: IntType, sign: SignType },
    CompareIntGreaterOrEqual { shape: IntType, sign: SignType },

    CompareFloatEqual(FloatType),
    CompareFloatNotEqual(FloatType),
    CompareFloatLessThen(FloatType),
    CompareFloatLessOrEqual(FloatType),
    CompareFloatGreaterThen(FloatType),
    CompareFloatGreaterOrEqual(FloatType),

    // Number Types Convert
    I32WrapI64,
    I64ExtendI32(SignType),
    F32DemoteF64,
    F64PromoteF32,
    I32TruncateF32(SignType),
    I64TruncateF32(SignType),
    I32TruncateF64(SignType),
    I64TruncateF64(SignType),
    I32TruncateSatF32(SignType),
    I64TruncateSatF32(SignType),
    I32TruncateSatF64(SignType),
    I64TruncateSatF64(SignType),
    F32ConvertI32(SignType),
    F32ConvertI64(SignType),
    F64ConvertI32(SignType),
    F64ConvertI64(SignType),
    F32ReinterpretI32,
    F64ReinterpretI64,
    I32ReinterpretI32,
    I64ReinterpretI64,

    // Vec Unary
    V128Not,
    V128And,
    V128AndNot,
    V128Or,
    V128XOr,
    V128BitSelect,
    V128AnyTrue,

    VecIntNegative(IntegerVectorShape),
    VecIntAbsolute(IntegerVectorShape),
    VecI8x16PopCnt,
    VecI8x16AverageUnsigned,
    VecI16x8AverageUnsigned,
    
    VecFloatNegative(FloatVectorShape),
    VecFloatAbsolute(FloatVectorShape),
    VecFloatSquareRoot(FloatVectorShape),
    VecFloatCeil(FloatVectorShape),
    VecFloatFloor(FloatVectorShape),
    VecFloatTruncate(FloatVectorShape),
    VecFloatNearest(FloatVectorShape),

    I32x4TruncSatF32x4(SignType),
    I32x4TruncSatF64x2Zero(SignType),
    F64x2PromoteLowF32x4,
    F32x4PemoteF64x2Zero,
    F32x4ConvertI32x4(SignType),
    F64x2ConvertLowI32x4(SignType),
    I16x8ExtendAddPairwiseI8x16(SignType),
    I32x4ExtaddPairwiseI16x8(SignType),

    // Vec Binary
    VecIntEqual(IntegerVectorShape),
    VecIntNotEqual(IntegerVectorShape),
    VecIntLessThen { shape: IntegerVectorShape, sign: SignType, },
    VecIntLessOrEqual { shape: IntegerVectorShape, sign: SignType, },
    VecIntGreaterThen { shape: IntegerVectorShape, sign: SignType, },
    VecIntGreaterOrEqual { shape: IntegerVectorShape, sign: SignType, },

    VecEqualFloat(FloatVectorShape),
    VecNotEqualFloat(FloatVectorShape),
    VecLessThenFloat(FloatVectorShape),
    VecLessOrEqualFloat(FloatVectorShape),
    VecGreaterThenFloat(FloatVectorShape),
    VecGreaterOrEqualFloat(FloatVectorShape),
    VecSwizzleFloatI8x16,

    VecIntAdd(IntegerVectorShape),
    VecIntSub(IntegerVectorShape),
    VecIntMultiplyI16x8,
    VecIntMultiplyI32x4,
    VecIntMultiplyI64x2,
    VecAddSatI16x8(SignType),
    VecSubtractSatI16x8(SignType),
    VecI32x4DotProductOfI16x8Signed,

    VecMinInt { shape: IntegerVectorShape, sign: SignType, },
    VecMaxInt { shape: IntegerVectorShape, sign: SignType, },
    VecSubFloat(FloatVectorShape),
    VecAddSatI8x16(SignType),
    VecSubtractSatI8x16(SignType),
    VecAddFloat(FloatVectorShape),
    VecDivFloat(FloatVectorShape),
    VecMulFloat(FloatVectorShape),
    VecMinFloat(FloatVectorShape),
    VecMaxFloat(FloatVectorShape),
    VecPMin(FloatVectorShape),
    VecPMax(FloatVectorShape),
    I16x8Q15mulrSatS,
    I8x16NarrowI16x8(SignType),
    I16x8NarrowI32x4(SignType),

    // Vec Unary
    I16x8ExtendI8x16 { half: HalfType, sign: SignType },
    I32x4ExtendI16x8 { half: HalfType, sign: SignType },
    I64x2ExtendI32x4 { half: HalfType, sign: SignType },
    I16x8ExtendMultiplyI8x16 { half: HalfType, sign: SignType },
    I32x4ExtendMultiplyI16x8 { half: HalfType, sign: SignType },
    I64x2ExtendMultiplyI32x4 { half: HalfType, sign: SignType },

    // Something
    VecTest(IntegerVectorShape),
    VecBitmask(IntegerVectorShape),
    VecShiftLeft(IntegerVectorShape),
    VecShiftRight { shape: IntegerVectorShape, sign: SignType },
    VecShuffle,
    VecSplat(VectorShape),
    VecExtract { shape: VectorShape, sign: Option<SignType> },
    VecReplate(VectorShape),

    // More testing but also keywords
    Module,
    Bin,
    Quote,

    // testing
    Script,
    Register,
    Invoke,
    Get,
    AssertMalformed,
    AssertInvalid,
    AssertUnlinkable,
    AssertReturn,
    AssertTrap,
    AssertExhaustion,
    NaNCanonical,
    NaNArithmetic(String),
    Infinit,
    NaN,
    Input,
    Output,
}

// impl Visit for Keyword {}

impl Keyword {
    /// Expect no input parameters and one return type that must be of I32.
    pub fn has_unary_return_ty_i32(&self) -> bool {
        matches!(self, Keyword::Const(NumType::I32))
    }

    pub fn is_constant(&self) -> bool {
        use Keyword::*;
        matches!(self, Const(_) | RefNull | RefFunc | GlobalGet)
    }
}

#[rustfmt::skip]
impl FromStr for Keyword {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use FloatVectorShape::*;
        use HalfType::*;
        use IntegerVectorShape::*;
        use Keyword::*;
        use SignType::*;
        use VectorShape::*;
        use Direction::*;
        Ok(match s {
            // number type
            "i32" => I32,
            "i64" => I64,
            "f32" => F32,
            "f64" => F64,
            // vector type
            "v128" => V128,
            // reference type
            "funcref" => FuncRef,
            "externref" => ExternRef,
            // instructions

            "i8x16" => VecShape(VectorShape::Int(I8x16)),
            "i16x8" => VecShape(VectorShape::Int(I16x8)),
            "i32x4" => VecShape(VectorShape::Int(I32x4)),
            "i64x2" => VecShape(VectorShape::Int(I64x2)),
            "f32x4" => VecShape(VectorShape::Float(F32x4)),
            "f64x2" => VecShape(VectorShape::Float(F64x2)),
            
            "extern" => Extern,
            "mut" => Mut,

            "nop" => Nop,
            "unreachable" => Unreachable,
            "drop" => Drop,
            "block" => Block,
            "loop" => Loop,
            "end" => End,
            "br" => Br,
            "br_if" => BrIf,
            "br_table" => BrTable,
            "return" => Return,
            "if" => If,
            "then" => Then,
            "else" => Else,
            "select" => Select,
            "call" => Call,
            "call_indirect" => CallIndirect,

            "local.get" => LocalGet,
            "local.set" => LocalSet,
            "local.tee" => LocalTee,
            "global.get" => GlobalGet,
            "global.set" => GlobalSet,

            "table.get" => TableGet,
            "table.set" => TableSet,
            "table.size" => TableSize,
            "table.grow" => TableGrow,
            "table.fill" => TableFill,
            "table.copy" => TableCopy,
            "table.init" => TableInit,
            "elem.drop" => ElemDrop,

            "memory.size" => MemorySize,
            "memory.grow" => MemoryGrow,
            "memory.fill" => MemoryFill,
            "memory.copy" => MemoryCopy,
            "memory.init" => MemoryInit,
            "data.drop" => DataDrop,

            "i32.load" => Load(NumType::I32),
            "i64.load" => Load(NumType::I64),
            "f32.load" => Load(NumType::F32),
            "f64.load" => Load(NumType::F64),
            "i32.store" => Store(NumType::I32),
            "i64.store" => Store(NumType::I64),
            "f32.store" => Store(NumType::F32),
            "f64.store" => Store(NumType::F64),

            "i32.load8_u" => I32Load8(Unsigned),
            "i32.load8_s" => I32Load8(Signed),
            "i32.load16_u" => I32Load16(Unsigned),
            "i32.load16_s" => I32Load16(Signed),
            "i64.load8_u" => I64Load8(Unsigned),
            "i64.load8_s" => I64Load8(Signed),
            "i64.load16_u" => I64Load16(Unsigned),
            "i64.load16_s" => I64Load16(Signed),
            "i64.load32_u" => I64Load32(Unsigned),
            "i64.load32_s" => I64Load32(Signed),

            "i32.store8" => I32Store8,
            "i32.store16" => I32Store16,
            "i64.store8" => I64Store8,
            "i64.store16" => I64Store16,
            "i64.store32" => I64Store32,

            // v128 load
            "v128.load" => V128Load,
            "v128.store" => V128Store,
            "v128.load8x8_u" => VecLoad8x8(Unsigned),
            "v128.load8x8_s" => VecLoad8x8(Signed),
            "v128.load16x4_u" => VecLoad16x4(Unsigned),
            "v128.load16x4_s" => VecLoad16x4(Signed),
            "v128.load32x2_u" => VecLoad32x2(Unsigned),
            "v128.load32x2_s" => VecLoad32x2(Signed),
            "v128.load8_splat" => VecLoadSplat(MemoryWidth::I8),
            "v128.load16_splat" => VecLoadSplat(MemoryWidth::I16),
            "v128.load32_splat" => VecLoadSplat(MemoryWidth::I32),
            "v128.load64_splat" => VecLoadSplat(MemoryWidth::I64),
            "v128.load32_zero" => VecLoadZero(MemoryZeroWidth::I32),
            "v128.load64_zero" => VecLoadZero(MemoryZeroWidth::I64),
            "v128.load8_lane" => VecLoadLane(MemoryWidth::I8),
            "v128.load16_lane" => VecLoadLane(MemoryWidth::I16),
            "v128.load32_lane" => VecLoadLane(MemoryWidth::I32),
            "v128.load64_lane" => VecLoadLane(MemoryWidth::I64),
            "v128.store8_lane" => VecStoreLane(MemoryWidth::I8),
            "v128.store16_lane" => VecStoreLane(MemoryWidth::I16),
            "v128.store32_lane" => VecStoreLane(MemoryWidth::I32),
            "v128.store64_lane" => VecStoreLane(MemoryWidth::I64),

            // const
            "i32.const" => Const(NumType::I32),
            "i64.const" => Const(NumType::I64),
            "f32.const" => Const(NumType::F32),
            "f64.const" => Const(NumType::F64),
            "v128.const" => ConstV128,

            // Reference
            "ref.null" => RefNull,
            "ref.func" => RefFunc,
            "ref.is_null" => RefIsNull,
            "ref.extern" => RefExtern,

            // Number unary
            "i32.clz" => IntClz(IntType::I32),
            "i32.ctz" => IntCtz(IntType::I32),
            "i32.popcnt" => IntPopCnt(IntType::I32),
            "i32.extend8_s" => IntExtend8Signed(IntType::I32),
            "i32.extend16_s" => IntExtend16Signed(IntType::I32),
            "i64.clz" => IntClz(IntType::I64),
            "i64.ctz" => IntCtz(IntType::I64),
            "i64.popcnt" => IntPopCnt(IntType::I64),
            "i64.extend8_s" => IntExtend8Signed(IntType::I64),
            "i64.extend16_s" => IntExtend16Signed(IntType::I64),
            "i64.extend32_s" => I64Extend32Signed,

            "f32.neg" => NegativeFloat(FloatType::F32),
            "f32.abs" => AbsoluteFloat(FloatType::F32),
            "f32.sqrt" => SquareRootFloat(FloatType::F32),
            "f32.ceil" => CeilFloat(FloatType::F32),
            "f32.floor" => FloorFLoat(FloatType::F32),
            "f32.trunc" => TruncateFloat(FloatType::F32),
            "f32.nearest" => NearestFloat(FloatType::F32),
            "f64.neg" => NegativeFloat(FloatType::F64),
            "f64.abs" => AbsoluteFloat(FloatType::F64),
            "f64.sqrt" => SquareRootFloat(FloatType::F64),
            "f64.ceil" => CeilFloat(FloatType::F64),
            "f64.floor" => FloorFLoat(FloatType::F64),
            "f64.trunc" => TruncateFloat(FloatType::F64),
            "f64.nearest" => NearestFloat(FloatType::F64),

            // Number Binary
            "i32.add" => AddInt(IntType::I32),
            "i32.sub" => SubInt(IntType::I32),
            "i32.mul" => MultiplyInt(IntType::I32),
            "i32.div_u" => DivideInt { shape: IntType::I32, sign: Unsigned },
            "i32.div_s" => DivideInt { shape: IntType::I32, sign: Signed },
            "i32.rem_u" => RemainderInt { shape: IntType::I32, sign: Unsigned },
            "i32.rem_s" => RemainderInt { shape: IntType::I32, sign: Signed },
            "i32.and" => AndInt(IntType::I32),
            "i32.or" => OrInt(IntType::I32),
            "i32.xor" => XORInt(IntType::I32),
            "i32.shl" => ShiftLeftInt(IntType::I32),
            "i32.shr_u" => ShiftRightInt { shape: IntType::I32, sign: Unsigned },
            "i32.shr_s" => ShiftRightInt { shape: IntType::I32, sign: Signed },
            "i32.rotl" => RotateInt { shape: IntType::I32, direction: Left },
            "i32.rotr" => RotateInt { shape: IntType::I32, direction: Rigth },
            "i64.add" => AddInt(IntType::I64),
            "i64.sub" => SubInt(IntType::I64),
            "i64.mul" => MultiplyInt(IntType::I64),
            "i64.div_u" => DivideInt { shape: IntType::I64, sign: Unsigned },
            "i64.div_s" => DivideInt { shape: IntType::I64, sign: Signed },
            "i64.rem_u" => RemainderInt { shape: IntType::I64, sign: Unsigned },
            "i64.rem_s" => RemainderInt { shape: IntType::I64, sign: Signed },
            "i64.and" => AndInt(IntType::I64),
            "i64.or" => OrInt(IntType::I64),
            "i64.xor" => XORInt(IntType::I64),
            "i64.shl" => ShiftLeftInt(IntType::I64),
            "i64.shr_u" => ShiftRightInt { shape: IntType::I64, sign: Unsigned },
            "i64.shr_s" => ShiftRightInt { shape: IntType::I64, sign: Signed },
            "i64.rotl" => RotateInt { shape: IntType::I64, direction: Left },
            "i64.rotr" => RotateInt { shape: IntType::I64, direction: Rigth },
      
            "f32.add" => AddFloat(FloatType::F32),
            "f32.sub" => SubFloat(FloatType::F32),
            "f32.mul" => MultiplyFloat(FloatType::F32),
            "f32.div" => DivideFloat(FloatType::F32),
            "f32.min" => MinFloat(FloatType::F32),
            "f32.max" => MaxFloat(FloatType::F32),
            "f32.copysign" => CopySign(FloatType::F32),
            "f64.add" => AddFloat(FloatType::F64),
            "f64.sub" => SubFloat(FloatType::F64),
            "f64.mul" => MultiplyFloat(FloatType::F64),
            "f64.div" => DivideFloat(FloatType::F64),
            "f64.min" => MinFloat(FloatType::F64),
            "f64.max" => MaxFloat(FloatType::F64),
            "f64.copysign" => CopySign(FloatType::F64),

            // Number Test
            "i32.eqz" => I32EqualTest,
            "i64.eqz" => I64EqualTest,

            // Number Type, Convert
            "i32.eq" => CompareIntEqual(IntType::I32),
            "i32.ne" => CompareIntNotEqual(IntType::I32),
            "i32.lt_u" => CompareIntLessThen { shape: IntType::I32, sign: Unsigned },
            "i32.lt_s" => CompareIntLessThen { shape: IntType::I32, sign: Signed },
            "i32.le_u" => CompareIntLessOrEqual { shape: IntType::I32, sign: Unsigned },
            "i32.le_s" => CompareIntLessOrEqual { shape: IntType::I32, sign: Signed },
            "i32.gt_u" => CompareIntGreaterThen { shape: IntType::I32, sign: Unsigned },
            "i32.gt_s" => CompareIntGreaterThen { shape: IntType::I32, sign: Signed },
            "i32.ge_u" => CompareIntGreaterOrEqual { shape: IntType::I32, sign: Unsigned },
            "i32.ge_s" => CompareIntGreaterOrEqual { shape: IntType::I32, sign: Signed },
            "i64.eq" => CompareIntEqual(IntType::I64),
            "i64.ne" => CompareIntNotEqual(IntType::I64),
            "i64.lt_u" => CompareIntLessThen { shape: IntType::I64, sign: Unsigned },
            "i64.lt_s" => CompareIntLessThen { shape: IntType::I64, sign: Signed },
            "i64.le_u" => CompareIntLessOrEqual { shape: IntType::I64, sign: Unsigned },
            "i64.le_s" => CompareIntLessOrEqual { shape: IntType::I64, sign: Signed },
            "i64.gt_u" => CompareIntGreaterThen { shape: IntType::I64, sign: Unsigned },
            "i64.gt_s" => CompareIntGreaterThen { shape: IntType::I64, sign: Signed },
            "i64.ge_u" => CompareIntGreaterOrEqual { shape: IntType::I64, sign: Unsigned },
            "i64.ge_s" => CompareIntGreaterOrEqual { shape: IntType::I64, sign: Signed },
      
            "f32.eq" => CompareFloatEqual(FloatType::F32),
            "f32.ne" => CompareFloatNotEqual(FloatType::F32),
            "f32.lt" => CompareFloatLessThen(FloatType::F32),
            "f32.le" => CompareFloatLessOrEqual(FloatType::F32),
            "f32.gt" => CompareFloatGreaterThen(FloatType::F32),
            "f32.ge" => CompareFloatGreaterOrEqual(FloatType::F32),
            "f64.eq" => CompareFloatEqual(FloatType::F64),
            "f64.ne" => CompareFloatNotEqual(FloatType::F64),
            "f64.lt" => CompareFloatLessThen(FloatType::F64),
            "f64.le" => CompareFloatLessOrEqual(FloatType::F64),
            "f64.gt" => CompareFloatGreaterThen(FloatType::F64),
            "f64.ge" => CompareFloatGreaterOrEqual(FloatType::F64),

            "i32.wrap_i64" => I32WrapI64,
            "i64.extend_i32_s" => I64ExtendI32(Signed),
            "i64.extend_i32_u" => I64ExtendI32(Unsigned),
            "f32.demote_f64" => F32DemoteF64,
            "f64.promote_f32" => F64PromoteF32,
            "i32.trunc_f32_u" => I32TruncateF32(Unsigned),
            "i32.trunc_f32_s" => I32TruncateF32(Signed),
            "i64.trunc_f32_u" => I64TruncateF32(Unsigned),
            "i64.trunc_f32_s" => I64TruncateF32(Signed),
            "i32.trunc_f64_u" => I32TruncateF64(Unsigned),
            "i32.trunc_f64_s" => I32TruncateF64(Signed),
            "i64.trunc_f64_u" => I64TruncateF64(Unsigned),
            "i64.trunc_f64_s" => I64TruncateF64(Signed),
            "i32.trunc_sat_f32_u" => I32TruncateSatF32(Unsigned),
            "i32.trunc_sat_f32_s" => I32TruncateSatF32(Signed),
            "i64.trunc_sat_f32_u" => I64TruncateSatF32(Unsigned),
            "i64.trunc_sat_f32_s" => I64TruncateSatF32(Signed),
            "i32.trunc_sat_f64_u" => I32TruncateSatF64(Unsigned),
            "i32.trunc_sat_f64_s" => I32TruncateSatF64(Signed),
            "i64.trunc_sat_f64_u" => I64TruncateSatF64(Unsigned),
            "i64.trunc_sat_f64_s" => I64TruncateSatF64(Signed),
            "f32.convert_i32_u" => F32ConvertI32(Unsigned),
            "f32.convert_i32_s" => F32ConvertI32(Signed),
            "f64.convert_i32_u" => F32ConvertI64(Unsigned),
            "f64.convert_i32_s" => F32ConvertI64(Signed),
            "f32.convert_i64_u" => F64ConvertI32(Unsigned),
            "f32.convert_i64_s" => F64ConvertI32(Signed),
            "f64.convert_i64_u" => F64ConvertI64(Unsigned),
            "f64.convert_i64_s" => F64ConvertI64(Signed),
            "f32.reinterpret_i32" => F32ReinterpretI32,
            "f64.reinterpret_i64" => F64ReinterpretI64,
            "i32.reinterpret_f32" => I32ReinterpretI32,
            "i64.reinterpret_f64" => I64ReinterpretI64,
      
            // Vec Unary
            "v128.not" => V128Not,
            "v128.and" => V128And,
            "v128.andnot" => V128AndNot,
            "v128.or" => V128Or,
            "v128.xor" => V128XOr,
            "v128.bitselect" => V128BitSelect,
            "v128.any_true" => V128AnyTrue,

            "i8x16.neg" => VecIntNegative(I8x16),
            "i16x8.neg" => VecIntNegative(I16x8),
            "i32x4.neg" => VecIntNegative(I32x4),
            "i64x2.neg" => VecIntNegative(I64x2),
            "i8x16.abs" => VecIntAbsolute(I8x16),
            "i16x8.abs" => VecIntAbsolute(I16x8),
            "i32x4.abs" => VecIntAbsolute(I32x4),
            "i64x2.abs" => VecIntAbsolute(I64x2),
            "i8x16.popcnt" => VecI8x16PopCnt,
            "i8x16.avgr_u" => VecI8x16AverageUnsigned,
            "i16x8.avgr_u" => VecI16x8AverageUnsigned,
            
            "f32x4.neg" => VecFloatNegative(F32x4),
            "f64x2.neg" => VecFloatNegative(F64x2),
            "f32x4.abs" => VecFloatAbsolute(F32x4),
            "f64x2.abs" => VecFloatAbsolute(F64x2),
            "f32x4.sqrt" => VecFloatSquareRoot(F32x4),
            "f64x2.sqrt" => VecFloatSquareRoot(F64x2),
            "f32x4.ceil" => VecFloatCeil(F32x4),
            "f64x2.ceil" => VecFloatCeil(F64x2),
            "f32x4.floor" => VecFloatFloor(F32x4),
            "f64x2.floor" => VecFloatFloor(F64x2),
            "f32x4.trunc" => VecFloatTruncate(F32x4),
            "f64x2.trunc" => VecFloatTruncate(F64x2),
            "f32x4.nearest" => VecFloatNearest(F32x4),
            "f64x2.nearest" => VecFloatNearest(F64x2),

            "i32x4.trunc_sat_f32x4_u" => I32x4TruncSatF32x4(Unsigned),
            "i32x4.trunc_sat_f32x4_s" => I32x4TruncSatF32x4(Signed),
            "i32x4.trunc_sat_f64x2_u_zero" => I32x4TruncSatF64x2Zero(Unsigned),
            "i32x4.trunc_sat_f64x2_s_zero" => I32x4TruncSatF64x2Zero(Signed),
            "f64x2.promote_low_f32x4" => F64x2PromoteLowF32x4,
            "f32x4.demote_f64x2_zero" => F32x4PemoteF64x2Zero,
            "f32x4.convert_i32x4_u" => F32x4ConvertI32x4(Unsigned),
            "f32x4.convert_i32x4_s" => F32x4ConvertI32x4(Signed),
            "f64x2.convert_low_i32x4_u" => F64x2ConvertLowI32x4(Unsigned),
            "f64x2.convert_low_i32x4_s" => F64x2ConvertLowI32x4(Signed),
            "i16x8.extadd_pairwise_i8x16_u" => I16x8ExtendAddPairwiseI8x16(Unsigned),
            "i16x8.extadd_pairwise_i8x16_s" => I16x8ExtendAddPairwiseI8x16(Signed),
            "i32x4.extadd_pairwise_i16x8_u" => I32x4ExtaddPairwiseI16x8(Unsigned),
            "i32x4.extadd_pairwise_i16x8_s" => I32x4ExtaddPairwiseI16x8(Signed),

            // Vector Binary
            "i8x16.eq" => VecIntEqual (I8x16),
            "i16x8.eq" => VecIntEqual (I16x8),
            "i32x4.eq" => VecIntEqual (I32x4),
            "i64x2.eq" => VecIntEqual (I64x2),
            "i8x16.ne" => VecIntNotEqual (I8x16),
            "i16x8.ne" => VecIntNotEqual (I16x8),
            "i32x4.ne" => VecIntNotEqual (I32x4),
            "i64x2.ne" => VecIntNotEqual (I64x2),
            "i8x16.lt_u" => VecIntLessThen { shape: I8x16, sign: Unsigned },
            "i8x16.lt_s" => VecIntLessThen { shape: I8x16, sign: Signed },
            "i16x8.lt_u" => VecIntLessThen { shape: I16x8, sign: Unsigned },
            "i16x8.lt_s" => VecIntLessThen { shape: I16x8, sign: Signed },
            "i32x4.lt_u" => VecIntLessThen { shape: I32x4, sign: Unsigned },
            "i32x4.lt_s" => VecIntLessThen { shape: I32x4, sign: Signed },
            "i64x2.lt_s" => VecIntLessThen { shape: I64x2, sign: Signed },
            "i8x16.le_u" => VecIntLessOrEqual { shape: I8x16, sign: Unsigned },
            "i8x16.le_s" => VecIntLessOrEqual { shape: I8x16, sign: Signed },
            "i16x8.le_u" => VecIntLessOrEqual { shape: I16x8, sign: Unsigned },
            "i16x8.le_s" => VecIntLessOrEqual { shape: I16x8, sign: Signed },
            "i32x4.le_u" => VecIntLessOrEqual { shape: I32x4, sign: Unsigned },
            "i32x4.le_s" => VecIntLessOrEqual { shape: I32x4, sign: Signed },
            "i64x2.le_s" => VecIntLessOrEqual { shape: I64x2, sign: Signed },
            "i8x16.gt_u" => VecIntGreaterThen { shape: I8x16, sign: Unsigned },
            "i8x16.gt_s" => VecIntGreaterThen { shape: I8x16, sign: Signed },
            "i16x8.gt_u" => VecIntGreaterThen { shape: I16x8, sign: Unsigned },
            "i16x8.gt_s" => VecIntGreaterThen { shape: I16x8, sign: Signed },
            "i32x4.gt_u" => VecIntGreaterThen { shape: I32x4, sign: Unsigned },
            "i32x4.gt_s" => VecIntGreaterThen { shape: I32x4, sign: Signed },
            "i64x2.gt_s" => VecIntGreaterThen { shape: I64x2, sign: Signed },
            "i8x16.ge_u" => VecIntGreaterOrEqual { shape: I8x16, sign: Unsigned },
            "i8x16.ge_s" => VecIntGreaterOrEqual { shape: I8x16, sign: Signed },
            "i16x8.ge_u" => VecIntGreaterOrEqual { shape: I16x8, sign: Unsigned },
            "i16x8.ge_s" => VecIntGreaterOrEqual { shape: I16x8, sign: Signed },
            "i32x4.ge_u" => VecIntGreaterOrEqual { shape: I32x4, sign: Unsigned },
            "i32x4.ge_s" => VecIntGreaterOrEqual { shape: I32x4, sign: Signed },
            "i64x2.ge_s" => VecIntGreaterOrEqual { shape: I64x2, sign: Signed },

            "f32x4.eq" => VecEqualFloat(F32x4),
            "f64x2.eq" => VecEqualFloat(F64x2),
            "f32x4.ne" => VecNotEqualFloat(F32x4),
            "f64x2.ne" => VecNotEqualFloat(F64x2),
            "f32x4.lt" => VecLessThenFloat(F32x4),
            "f64x2.lt" => VecLessThenFloat(F64x2),
            "f32x4.le" => VecLessOrEqualFloat(F32x4),
            "f64x2.le" => VecLessOrEqualFloat(F64x2),
            "f32x4.gt" => VecGreaterThenFloat(F32x4),
            "f64x2.gt" => VecGreaterThenFloat(F64x2),
            "f32x4.ge" => VecGreaterOrEqualFloat(F32x4),
            "f64x2.ge" => VecGreaterOrEqualFloat(F64x2),
            "i8x16.swizzle" => VecSwizzleFloatI8x16,

            "i8x16.add" => VecIntAdd(I8x16),
            "i16x8.add" => VecIntAdd(I16x8),
            "i32x4.add" => VecIntAdd(I32x4),
            "i64x2.add" => VecIntAdd(I64x2),
            "i8x16.sub" => VecIntSub(I8x16),
            "i16x8.sub" => VecIntSub(I16x8),
            "i32x4.sub" => VecIntSub(I32x4),
            "i64x2.sub" => VecIntSub(I64x2),
            "i16x8.mul" => VecIntMultiplyI16x8,
            "i32x4.mul" => VecIntMultiplyI32x4,
            "i64x2.mul" => VecIntMultiplyI64x2,
            "i8x16.add_sat_u" => VecAddSatI8x16(Unsigned),
            "i8x16.add_sat_s" => VecAddSatI8x16(Signed),
            "i16x8.add_sat_u" => VecAddSatI16x8(Unsigned),
            "i16x8.add_sat_s" => VecAddSatI16x8(Signed),
            "i8x16.sub_sat_u" => VecSubtractSatI8x16(Unsigned),
            "i8x16.sub_sat_s" => VecSubtractSatI8x16(Signed),
            "i16x8.sub_sat_u" => VecSubtractSatI16x8(Unsigned),
            "i16x8.sub_sat_s" => VecSubtractSatI16x8(Signed),
            "i32x4.dot_i16x8_s" => VecI32x4DotProductOfI16x8Signed,

            "i8x16.min_u" => VecMinInt { shape: I8x16, sign: Unsigned },
            "i16x8.min_u" => VecMinInt { shape: I16x8, sign: Unsigned },
            "i32x4.min_u" => VecMinInt { shape: I32x4, sign: Unsigned },
            "i8x16.min_s" => VecMinInt { shape: I8x16, sign: Signed },
            "i16x8.min_s" => VecMinInt { shape: I16x8, sign: Signed },
            "i32x4.min_s" => VecMinInt { shape: I32x4, sign: Signed },
            "i8x16.max_u" => VecMaxInt { shape: I8x16, sign: Unsigned },
            "i16x8.max_u" => VecMaxInt { shape: I16x8, sign: Unsigned },
            "i32x4.max_u" => VecMaxInt { shape: I32x4, sign: Unsigned },
            "i8x16.max_s" => VecMaxInt { shape: I8x16, sign: Signed },
            "i16x8.max_s" => VecMaxInt { shape: I16x8, sign: Signed },
            "i32x4.max_s" => VecMaxInt { shape: I32x4, sign: Signed },

            "f32x4.add" => VecAddFloat(F32x4),
            "f64x2.add" => VecAddFloat(F64x2),
            "f32x4.sub" => VecSubFloat(F32x4),
            "f64x2.sub" => VecSubFloat(F64x2),
            "f32x4.mul" => VecMulFloat(F32x4),
            "f64x2.mul" => VecMulFloat(F64x2),
            "f32x4.div" => VecDivFloat(F32x4),
            "f64x2.div" => VecDivFloat(F64x2),

            "f32x4.min" => VecMinFloat(F32x4),
            "f64x2.min" => VecMinFloat(F64x2),
            "f32x4.max" => VecMaxFloat(F32x4),
            "f64x2.max" => VecMaxFloat(F64x2),
            "f32x4.pmin" => VecPMin(F32x4),
            "f64x2.pmin" => VecPMin(F64x2),
            "f32x4.pmax" => VecPMax(F32x4),
            "f64x2.pmax" => VecPMax(F64x2),

            "i16x8.q15mulr_sat_s" => I16x8Q15mulrSatS,
            "i8x16.narrow_i16x8_u" => I8x16NarrowI16x8(Unsigned),
            "i8x16.narrow_i16x8_s" => I8x16NarrowI16x8(Signed),
            "i16x8.narrow_i32x4_u" => I16x8NarrowI32x4(Unsigned),
            "i16x8.narrow_i32x4_s" => I16x8NarrowI32x4(Signed),

            // Vectory unary
            "i16x8.extend_low_i8x16_u" => I16x8ExtendI8x16 { half: Low, sign: Unsigned },
            "i16x8.extend_low_i8x16_s" => I16x8ExtendI8x16 { half: Low, sign: Signed },
            "i16x8.extend_high_i8x16_u" => I16x8ExtendI8x16 { half: High, sign: Unsigned },
            "i16x8.extend_high_i8x16_s" => I16x8ExtendI8x16 { half: High, sign: Signed },
            "i32x4.extend_low_i16x8_u" => I32x4ExtendI16x8 { half: Low, sign: Unsigned },
            "i32x4.extend_low_i16x8_s" => I32x4ExtendI16x8 { half: Low, sign: Signed },
            "i32x4.extend_high_i16x8_u" => I32x4ExtendI16x8 { half: High, sign: Unsigned },
            "i32x4.extend_high_i16x8_s" => I32x4ExtendI16x8 { half: High, sign: Signed },
            "i64x2.extend_low_i32x4_u" => I64x2ExtendI32x4 { half: Low, sign: Unsigned },
            "i64x2.extend_low_i32x4_s" => I64x2ExtendI32x4 { half: Low, sign: Signed },
            "i64x2.extend_high_i32x4_u" => I64x2ExtendI32x4 { half: High, sign: Unsigned },
            "i64x2.extend_high_i32x4_s" => I64x2ExtendI32x4 { half: High, sign: Signed },
            "i16x8.extmul_low_i8x16_u" => I16x8ExtendMultiplyI8x16 { half: Low, sign: Unsigned },
            "i16x8.extmul_low_i8x16_s" => I16x8ExtendMultiplyI8x16 { half: Low, sign: Signed },
            "i16x8.extmul_high_i8x16_u" => I16x8ExtendMultiplyI8x16 { half: High, sign: Unsigned },
            "i16x8.extmul_high_i8x16_s" => I16x8ExtendMultiplyI8x16 { half: High, sign: Signed },
            "i32x4.extmul_low_i16x8_u" => I32x4ExtendMultiplyI16x8 { half: Low, sign: Unsigned },
            "i32x4.extmul_low_i16x8_s" => I32x4ExtendMultiplyI16x8 { half: Low, sign: Signed },
            "i32x4.extmul_high_i16x8_u" => I32x4ExtendMultiplyI16x8 { half: High, sign: Unsigned },
            "i32x4.extmul_high_i16x8_s" => I32x4ExtendMultiplyI16x8 { half: High, sign: Signed },
            "i64x2.extmul_low_i32x4_u" => I64x2ExtendMultiplyI32x4 { half: Low, sign: Unsigned },
            "i64x2.extmul_low_i32x4_s" => I64x2ExtendMultiplyI32x4 { half: Low, sign: Signed },
            "i64x2.extmul_high_i32x4_u" => I64x2ExtendMultiplyI32x4 { half: High, sign: Unsigned },
            "i64x2.extmul_high_i32x4_s" => I64x2ExtendMultiplyI32x4 { half: High, sign: Signed },

            // More vector
            "i8x16.all_true" => VecTest(I8x16),
            "i16x8.all_true" => VecTest(I16x8),
            "i32x4.all_true" => VecTest(I32x4),
            "i64x2.all_true" => VecTest(I64x2),
            "i8x16.bitmask" => VecBitmask(I8x16),
            "i16x8.bitmask" => VecBitmask(I16x8),
            "i32x4.bitmask" => VecBitmask(I32x4),
            "i64x2.bitmask" => VecBitmask(I64x2),
            "i8x16.shl" => VecShiftLeft(I8x16),
            "i16x8.shl" => VecShiftLeft(I16x8),
            "i32x4.shl" => VecShiftLeft(I32x4),
            "i64x2.shl" => VecShiftLeft(I64x2),
            "i8x16.shr_u" => VecShiftRight { shape: I8x16, sign: Unsigned },
            "i8x16.shr_s" => VecShiftRight { shape: I8x16, sign: Signed },
            "i16x8.shr_u" => VecShiftRight { shape: I16x8, sign: Unsigned },
            "i16x8.shr_s" => VecShiftRight { shape: I16x8, sign: Signed },
            "i32x4.shr_u" => VecShiftRight { shape: I32x4, sign: Unsigned },
            "i32x4.shr_s" => VecShiftRight { shape: I32x4, sign: Signed },
            "i64x2.shr_u" => VecShiftRight { shape: I64x2, sign: Unsigned },
            "i64x2.shr_s" => VecShiftRight { shape: I64x2, sign: Signed },
            "i8x16.shuffle" => VecShuffle,

            // splat and extract_lane
            "i8x16.splat" => VecSplat(Int(I8x16)),
            "i16x8.splat" => VecSplat(Int(I16x8)),
            "i32x4.splat" => VecSplat(Int(I32x4)),
            "i64x2.splat" => VecSplat(Int(I64x2)),
            "f32x4.splat" => VecSplat(Float(F32x4)),
            "f64x2.splat" => VecSplat(Float(F64x2)),
            "i8x16.extract_lane_u" => VecExtract { shape: Int(I8x16), sign: Some(Unsigned) },
            "i8x16.extract_lane_s" => VecExtract { shape: Int(I8x16), sign: Some(Signed) },
            "i16x8.extract_lane_u" => VecExtract { shape: Int(I16x8), sign: Some(Unsigned) },
            "i16x8.extract_lane_s" => VecExtract { shape: Int(I16x8), sign: Some(Signed) },
            "i32x4.extract_lane" => VecExtract { shape: Int(I32x4), sign: None },
            "i64x2.extract_lane" => VecExtract { shape: Int(I64x2), sign: None },
            "f32x4.extract_lane" => VecExtract { shape: Float(F32x4), sign: None },
            "f64x2.extract_lane" => VecExtract { shape: Float(F64x2), sign: None },
            "i8x16.replace_lane" => VecReplate(Int(I8x16)),
            "i16x8.replace_lane" => VecReplate(Int(I16x8)),
            "i32x4.replace_lane" => VecReplate(Int(I32x4)),
            "i64x2.replace_lane" => VecReplate(Int(I64x2)),
            "f32x4.replace_lane" => VecReplate(Float(F32x4)),
            "f64x2.replace_lane" => VecReplate(Float(F64x2)),

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
            "binary" => Bin,
            "quote" => Quote,

            // Enabled for testing
            "script" => Script,
            "register" => Register,
            "invoke" => Invoke,
            "get" => Get,
            "assert_malformed" => AssertMalformed,
            "assert_invalid" => AssertInvalid,
            "assert_unlinkable" => AssertUnlinkable,
            "assert_return" => AssertReturn,
            "assert_trap" => AssertTrap,
            "assert_exhaustion" => AssertExhaustion,
            "nan:canonical" => NaNCanonical,
            "inf" => Infinit,
            "nan" => NaN,
            "input" => Input,
            "output" => Output,
            str if str.starts_with("nan:") => {
                let amount = match str.split(':').nth(1) {
                    Some(amount) => amount,
                    None => {
                        return Err(ParseError::new(format!("Parsing `nan:`. Determine to be not be a number. Failed to parse {}", str)))
                    },
                };
                NaNArithmetic(amount.to_string())
            }
            str if str.starts_with("align=") =>  {
                let amount = match str.split('=').nth(1) {
                    Some(amount) => amount, // Parse Hex Number
                    None => {
                        return Err(ParseError::new(format!("Parsing `align` of memory args was determine to be not be a number. Failed to parse {}", str)))
                    },
                };
                MemArgsAlign(amount.to_string())
            }
            str if str.starts_with("offset=") =>  {
                let amount = match str.split('=').nth(1) {
                    Some(amount) => amount,
                    None => {
                        return Err(ParseError::new(format!("Parsing `offset` of memory args was determine to be not be a number. Failed to parse {}", str)))
                    },
                };
                MemArgsOffset(amount.to_string())
            }
            str => return Err(ParseError::new(format!("{} is not a keyword", str))),
        })
    }
}

pub enum Whitespace {
    Space,
    NewLine,
}

pub enum Comment {
    LineComment,
    BlockComment,
}

pub struct BufferedReader<I: Iterator<Item = char>> {
    source: Peekable<I>,

    index: usize,
    line: usize,
    column: usize,
}

pub fn tokenize(wasm: &str) -> Result<Vec<Token>, ParseError> {
    let reader = BufferedReader::new(wasm.chars());
    let mut tokenizer = Tokenizer::new(reader);
    match tokenizer.parse() {
        Ok(_) => Ok(tokenizer.tokens),
        Err(err) => Err(err
            .with_location(tokenizer.reader.location())
            .with_tokens(tokenizer.tokens)),
    }
}

#[derive(Debug)]
pub enum SoftToken {
    Char(char),
    Whitespace,
    NewLine,
}

impl SoftToken {
    pub fn as_char(&self) -> Option<&char> {
        if let Self::Char(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

const SPACE: char = ' ';
const TAB: char = 0x09 as char;
const NEW_LINE: char = 0x0A as char;
const NEW_LINE_2: char = 0x0D as char;
const CLCR: char = /* 0x0D0A */ 0xDA as char;
const ID_CHAR: [char; 23] = [
    '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '_',
    '`', '|', '~', '\'',
];
const ESCAPE_CHARACTERS: [char; 6] = ['n', 'r', 't', '\\', '\'', '\"'];
const DIGIT: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const HEX_DIGIT: [char; 22] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9',
];
const ALL_NUMBER: [char; 13] = [
    '_', '+', '-', 'x', '.', 'e', 'E', 'p', 'P', 'n', 'f', 'a', 'i',
];
// let hexnum = hexdigit ('_'? hexdigit)*

enum NumberString {
    Hex(String),
    Int(String),
    Float(String),
}

/*
let sign = '+' | '-'
let digit = ['0'-'9']
let hexdigit = ['0'-'9''a'-'f''A'-'F']
let num = digit ('_'? digit)*
let hexnum = hexdigit ('_'? hexdigit)*

let letter = ['a'-'z''A'-'Z']
let symbol =
  ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`''.''\'']

let ascii_newline = ['\x0a''\x0d']
let newline = ascii_newline | "\x0a\x0d"
let space = [' ''\x09''\x0a''\x0d']
let control = ['\x00'-'\x1f'] # space
let ascii = ['\x00'-'\x7f']
let ascii_no_nl = ascii # ascii_newline
let utf8cont = ['\x80'-'\xbf']
let utf8enc =
    ['\xc2'-'\xdf'] utf8cont
  ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xed'] ['\x80'-'\x9f'] utf8cont
  | ['\xe1'-'\xec''\xee'-'\xef'] utf8cont utf8cont
  | ['\xf0'] ['\x90'-'\xbf'] utf8cont utf8cont
  | ['\xf4'] ['\x80'-'\x8f'] utf8cont utf8cont
  | ['\xf1'-'\xf3'] utf8cont utf8cont utf8cont
let utf8 = ascii | utf8enc
let utf8_no_nl = ascii_no_nl | utf8enc
let escape = ['n''r''t''\\''\'''\"']
let character =
    [^'"''\\''\x00'-'\x1f''\x7f'-'\xff']
  utf8enc
  | utf8enc
  | '\\'escape
  | '\\'hexdigit hexdigit
  | "\\u{" hexnum '}'

let nat = num | "0x" hexnum
let int = sign nat
let frac = num
let hexfrac = hexnum
let float =
    sign? num '.' frac?
  | sign? num ('.' frac?)? ('e' | 'E') sign? num
  | sign? "0x" hexnum '.' hexfrac?
  | sign? "0x" hexnum ('.' hexfrac?)? ('p' | 'P') sign? num
  | sign? "inf"
  | sign? "nan"
  | sign? "nan:" "0x" hexnum
let string = '"' character* '"'

let idchar = letter | digit | '_' | symbol
let name = idchar+
let id = '$' name

let keyword = ['a'-'z'] (letter | digit | '_' | '.' | ':')+
let reserved = (idchar | string)+ | ',' | ';' | '[' | ']' | '{' | '}'

let ixx = "i" ("32" | "64")
let fxx = "f" ("32" | "64")
let nxx = ixx | fxx
let vxxx = "v128"
let mixx = "i" ("8" | "16" | "32" | "64")
let mfxx = "f" ("32" | "64")
let sign = "s" | "u"
let mem_size = "8" | "16" | "32"
let v128_int_shape = "i8x16" | "i16x8" | "i32x4" | "i64x2"
let v128_float_shape = "f32x4" | "f64x2"
let v128_shape = v128_int_shape | v128_float_shape
*/

impl<I> BufferedReader<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(source: I) -> Self {
        let source = source.peekable();
        Self {
            source,
            index: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    pub fn peek_token(&mut self) -> Option<SoftToken> {
        match self.source.peek() {
            Some(character) => {
                if [NEW_LINE, NEW_LINE_2, CLCR].contains(character) {
                    Some(SoftToken::NewLine)
                } else if [SPACE, TAB].contains(character) {
                    Some(SoftToken::Whitespace)
                } else {
                    Some(SoftToken::Char(*character))
                }
            }
            None => None,
        }
    }

    pub fn read(&mut self) -> Option<char> {
        if let Some(char) = self.source.next() {
            self.index += 1;
            // If new line: https://doc.rust-lang.org/stable/std/io/trait.BufRead.html#method.read_line
            if char == NEW_LINE || char == NEW_LINE_2 || char == CLCR {
                self.line += 1;
                self.column = 0;
            } else if char == SPACE || char == TAB {
                self.column += 1;
            } else {
                self.column += 1;
            }
            Some(char)
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Option<SoftToken> {
        if let Some(char) = self.source.next() {
            self.index += 1;
            // If new line: https://doc.rust-lang.org/stable/std/io/trait.BufRead.html#method.read_line
            if char == NEW_LINE || char == NEW_LINE_2 || char == CLCR {
                self.line += 1;
                self.column = 0;
                Some(SoftToken::NewLine)
            } else if char == SPACE || char == TAB {
                self.column += 1;
                Some(SoftToken::Whitespace)
            } else {
                self.column += 1;
                Some(SoftToken::Char(char))
            }
        } else {
            None
        }
    }

    pub fn is_next(&mut self, char: impl Into<char>) -> bool {
        if let Some(next_char) = self.peek() {
            if *next_char == char.into() {
                return true;
            }
        }
        false
    }

    pub fn eof(&mut self) -> bool {
        self.source.peek().is_none()
    }

    fn location(&self) -> Location {
        Location {
            index: self.index,
            line: self.line,
            column: self.column,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    line: usize,
    column: usize,
    index: usize,
    source: String,
    ty: TokenType,
}

impl Token {
    pub fn ty(&self) -> &TokenType {
        &self.ty
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}

pub struct Tokenizer<I: Iterator<Item = char>> {
    reader: BufferedReader<I>,
    tokens: Vec<Token>,
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(reader: BufferedReader<I>) -> Self {
        Self {
            reader,
            tokens: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        while let Some(character) = self.reader.pop() {
            let c = match character {
                SoftToken::Char(char) => char,
                _ => continue,
            };
            let token = match c {
                ';' if self.reader.is_next(';') => {
                    // Comment, read until end of line
                    self.read_until_new_line();
                    None
                }
                '(' if self.reader.is_next(';') => {
                    // Comment thats large, read until closing comment
                    let _ = self.reader.pop(); // TODO(Alec): .expect(';')
                    self.read_until_closing_comment(); // read until closing ';)'
                    None
                }
                '(' | ')' => Some(Token {
                    ty: if c == '(' {
                        TokenType::LeftParen
                    } else {
                        TokenType::RightParen
                    },
                    line: self.reader.line,
                    column: self.reader.column,
                    index: self.reader.index,
                    source: String::from(c),
                }),
                '$' => Some(self.read_id()),
                '"' => Some(self.read_string()?),
                c if c.is_ascii_digit() || c == '+' || c == '-' => {
                    let line = self.reader.line;
                    let column = self.reader.column;
                    let index = self.reader.index;

                    let ty = TokenType::Number;
                    let source = self.read_until_delimiter(c);

                    Some(Token {
                        ty,
                        line,
                        column,
                        index,
                        source,
                    })
                }
                c if c.is_ascii_lowercase() => Some(self.read_keyword(c)?),

                _ => None,
            };
            if let Some(token) = token {
                self.tokens.push(token);
            }
        }
        Ok(())
    }

    fn read_hex_digit(
        &mut self,
        starting_char: Option<char>,
        is_hex: bool,
    ) -> Result<String, ParseError> {
        let mut chars = if let Some(c) = starting_char {
            vec![c]
        } else {
            vec![]
        };
        while let Some(character) = self.reader.peek() {
            // First character must be a hex digit
            if !(HEX_DIGIT.contains(character) || *character == '_') {
                if chars.is_empty() {
                    return Err(ParseError::new(format!(
                        "character {} is not valid. List of chars {:?}",
                        character, chars
                    )));
                } else {
                    break;
                }
            }
            match self.reader.pop() {
                Some(SoftToken::Char('_')) => chars.push('_'),
                Some(SoftToken::Char(c)) if HEX_DIGIT.contains(&c) && is_hex => chars.push(c),
                Some(SoftToken::Char(c)) if DIGIT.contains(&c) && !is_hex => chars.push(c),
                char => {
                    let str = chars.iter().collect::<String>();
                    let err = format!(
                        "Parsing Hex Number {} failed when encountered {:?}",
                        str, char
                    );
                    return Err(ParseError::new(err));
                }
            }
        }
        Ok(chars.iter().collect::<String>())
    }

    fn read_hex_number(&mut self, is_hex: bool) -> Result<NumberString, ParseError> {
        let mut chars = vec![];
        while let Some(character) = self.reader.peek() {
            // First character must be a hex digit
            if !HEX_DIGIT.contains(character) {
                if chars.is_empty() {
                    return Err(ParseError::new(format!(
                        "character {} is not valid. List of chars {:?}",
                        character, chars
                    )));
                } else {
                    break;
                }
            }
            match self.reader.pop() {
                Some(SoftToken::Char('_')) => continue,
                Some(SoftToken::Char(c)) if HEX_DIGIT.contains(&c) => chars.push(c),
                char => {
                    let str = chars.iter().collect::<String>();
                    let err = format!(
                        "Parsing Hex Number {} failed when encountered {:?}",
                        str, char
                    );
                    return Err(ParseError::new(err));
                }
            }
        }
        let string = chars.iter().collect::<String>();
        if is_hex {
            Ok(NumberString::Hex(string))
        } else if string.chars().all(|c| DIGIT.contains(&c)) {
            Ok(NumberString::Int(string))
        } else {
            Ok(NumberString::Hex(string))
        }
    }

    fn read_string(&mut self) -> Result<Token, ParseError> {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;

        let mut chars = vec![];
        // Strings represent both textual and binary data. They are enclosed in
        // quotations ("), and can contain anything expect control characters or
        // backslashed (unless used for escape character)
        while let Some(c) = self.reader.read() {
            if c == '"' {
                break;
            } else if c.is_ascii_control() {
                return Err(ParseError::new(
                    "Failed to read string because it contains a control character",
                ));
            } else if c == '\\' {
                match self.reader.read() {
                    Some('n') => chars.push('\x0a'),
                    Some('r') => chars.push('\x0d'),
                    Some('\\') => chars.push('\\'),
                    Some('\'') => chars.push('\''),
                    Some('\"') => chars.push('\"'),
                    Some('u') => {
                        let number = self.read_hex_number(false)?;
                        panic!("hi, please give me the code that errored here, thanks.")
                    }
                    Some(num1) if num1.is_ascii_hexdigit() => {
                        use std::convert::TryFrom;
                        let num1 = num1.to_digit(16).unwrap();
                        let num2 = self.reader.read().unwrap().to_digit(16).unwrap();
                        let number = u8::try_from(16 * num1 + num1).unwrap();
                        chars.push(number as char);
                    }
                    Some(char) => {
                        let string = chars.iter().collect::<String>();
                        format!(
                            "Was reading string {} and encountered invalid escape character \\{:?}",
                            string, char
                        );
                        return Err(ParseError::new(string));
                    }
                    _ => panic!(),
                }
            } else {
                chars.push(c)
            }
        }
        let source = chars.iter().collect::<String>();
        Ok(Token {
            line,
            column,
            index,
            source,
            ty: TokenType::String,
        })
    }

    fn read_id(&mut self) -> Token {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![];
        while let Some(c) = self.reader.peek_token() {
            match c {
                SoftToken::Char(c) if c.is_ascii_alphanumeric() || ID_CHAR.contains(&c) => {
                    let _ = self.reader.pop();
                    chars.push(c)
                }
                _ => break,
            }
        }
        let source = chars.iter().collect::<String>();
        Token {
            line,
            column,
            index,
            source,
            ty: TokenType::Id,
        }
    }

    fn read_keyword(&mut self, char: char) -> Result<Token, ParseError> {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![char];
        while let Some(peek) = self.reader.peek() {
            if peek.is_ascii_alphanumeric() || ID_CHAR.contains(peek) {
                chars.push(*self.reader.pop().unwrap().as_char().unwrap())
            } else {
                break;
            }
        }
        let source = chars.iter().collect::<String>();
        let ty = Keyword::from_str(&source)?;
        Ok(Token {
            line,
            column,
            index,
            source,
            ty: TokenType::Keyword(ty),
        })
    }

    fn read_until_delimiter(&mut self, first: char) -> String {
        let mut tokens = vec![first];
        while let Some(c) = self.reader.peek() {
            if c.is_ascii_hexdigit() || ALL_NUMBER.contains(c) {
                tokens.push(self.reader.read().unwrap());
            } else {
                break;
            }
        }
        tokens.iter().collect::<String>()
    }

    fn read_until_new_line(&mut self) {
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::NewLine => break,
                _ => continue,
            }
        }
    }

    fn read_until_closing_comment(&mut self) {
        let mut counter = 0;
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::Char('(') if self.reader.is_next(';') => {
                    counter += 1;
                    self.reader.pop();
                }
                SoftToken::Char(';') if self.reader.is_next(')') => {
                    let _ = self.reader.pop(); // Todo(Alec): .expect(')')
                    if counter == 0 {
                        break;
                    } else {
                        counter -= 1;
                    }
                }
                _ => continue,
            }
        }
    }
}
