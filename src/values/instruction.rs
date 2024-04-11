

//! Wasm consists of a sequence of instructions. It uses a stack machine to manupulate
//! values on an implicit operand stack. It consumes (pops) values and processes
//! them and returns (push) the result.
//! 
//! In addition, some instructions have static _immediate arguments_. Typically
//! Indices or Type annotations.

enum IBitSize {
    N32,
    N64,
}

enum Sign {
    Unsigned,
    Signed
}

pub enum Operand {}


pub enum Instruction {
    // Constant. Return a static constant
    I32Const, // takes i32
    I64Const, // takes i64
    F32Const, // takes f32
    F64Const, // takes f64

    // Unary Operation. Consume one operand and produce one reuslt of the respective type
    I32Clz,
    I32Ctz,
    I32PopCnt,

    I64Clz,
    I64Ctz,
    I64PopCnt,

    F32Abs,
    F32Neg,
    F32Sqrt,
    F32Ceil,
    F32Floor,
    F32Truncate,
    F32Nearest,

    F64Abs,
    F64Neg,
    F64Sqrt,
    F64Ceil,
    F64Floor,
    F64Truncate,
    F64Nearest,

    // Binary Operation. consume two operands and produce one result of the respecitve type
    I32Add,
    I32Subtract,
    I32Multiply,
    I32DivideSigned,
    I32DivideUnsigned,
    I32RemainderSigned,
    I32RemainderUnsigned,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrSigned,
    I32ShrUnsigned,
    I32Rotl,
    I32Rotr,

    I64Add,
    I64Subtract,
    I64Multiply,
    I64DivideSigned,
    I64DivideUnsigned,
    I64RemainderSigned,
    I64RemainderUnsigned,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrSigned,
    I64ShrUnsigned,
    I64Rotl,
    I64Rotr,

    F32Add,
    F32Subtract,
    F32Multiply,
    F32Divide,
    F32Minimum,
    F32Maximum,
    F32Copysign,

    F64Add,
    F64Subtract,
    F64Multiply,
    F64Divide,
    F64Minimum,
    F64Maximum,
    F64Copysign,

    // Tests. Consume one operand of the respective type and produce a boolean integer result
    I32Eqz,
    I64Eqz,

    // Comparisons. Consume two operands of the respective type and produce a boolean int result
    I32Equal,
    I32NotEqual,
    I32LessThenSigned,
    I32LessThenUnsigned,
    I32GreaterThenSigned,
    I32GreaterThenUnsigned,
    I32LessOrEqualSigned,
    I32LessOrEqualUnsigned,
    I32GreaterOrEqualSigned,
    I32GreaterOrEqualUnsigned,

    I64Equal,
    I64NotEqual,
    I64LessThenSigned,
    I64LessThenUnsigned,
    I64GreaterThenSigned,
    I64GreaterThenUnsigned,
    I64LessOrEqualSigned,
    I64LessOrEqualUnsigned,
    I64GreaterOrEqualSigned,
    I64GreaterOrEqualUnsigned,

    F32Equal,
    F32NotEqual,
    F32LessThen,
    F32GreaterThen,
    F32LessOrEqual,
    F32GreaterOrEqual,

    F64Equal,
    F64NotEqual,
    F64LessThen,
    F64GreaterThen,
    F64LessOrEqual,
    F64GreaterOrEqual,

    // Conversions. consume a value of one type and produce a result of another
    I32ExtendSigned8,
    I32ExtendSigned16,

    I64ExtendSigned8,
    I64ExtendSigned16,
    I64ExtendSigned32,

    // Note(Alec): Some of these are not in the correct direction
    WrapI64IntoI32,
    ExtendSignedI32IntoI64,
    ExtendUnsignedI32IntoI64,

    TruncateF32IntoI32Signed,
    TruncateF64IntoI32Signed,
    TruncateF32IntoI32Unsigned,
    TruncateF64IntoI32Unsigned,
    TruncateF32IntoI64Signed,
    TruncateF64IntoI64Signed,
    TruncateF32IntoI64Unsigned,
    TruncateF64IntoI64Unsigned,

    TruncateSatF32IntoI32Signed,
    TruncateSatF64IntoI32Signed,
    TruncateSatF32IntoI32Unsigned,
    TruncateSatF64IntoI32Unsigned,
    TruncateSatF32IntoI64Signed,
    TruncateSatF64IntoI64Signed,
    TruncateSatF32IntoI64Unsigned,
    TruncateSatF64IntoI64Unsigned,

    DemoteF64ToF32,
    PromoteF32ToF64,

    ConvertSignedI32IntoF32,
    ConvertSignedI64IntoF32,
    ConvertUnsignedI32IntoF32,
    ConvertUnsignedI64IntoF32,
    ConvertSignedI32IntoF64,
    ConvertSignedI64IntoF64,
    ConvertUnsignedI32IntoF64,
    ConvertUnsignedI64IntoF64,

    ReinterpretF32IntoI32,
    ReinterpretF32IntoI64,
    ReinterpretF64IntoI32,
    ReinterpretF64IntoI64,

    ReinterpretI32IntoF32,
    ReinterpretI64IntoF32,
    ReinterpretI32IntoF64,
    ReinterpretI64IntoF64,
}

#[rustfmt::skip]
pub enum V128 {
    F64(f64, f64),
    F32(f32, f32, f32, f32),

    I64(i64, i64),
    I32(i32, i32, i32, i32),
    I16(i16, i16,i16, i16, i16, i16, i16, i16),
    I8(i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8, i8),
}

pub enum VectorInstruction {
    V128Const, // takes i128

    V128Not,

    V128And,
    V128AndNot,
    V128Or,
    V128Xor,

    V128AnyTrue,

    V128AllTrue,

    V128ShuffleI8x16, // takes u8 as id

    V128SwizzleI8x16,

    V128SplatI8x16,
    V128SplatI16x8,
    V128SplatI32x4,
    V128SplatI64x2,
    V128SplatF32x4,
    V128SplatF64x2,

    V128ExtractLaneSignedI8x16, // takes u8 as id
    V128ExtractLaneUnsignedI8x16, // takes u8 as id

    V128ExtractLaneSignedI16x8, // takes u8 as id
    V128ExtractLaneUnsignedI16x8, // takes u8 as id

    V128ExtractLaneSignedI32x4, // takes u8 as id
    V128ExtractLaneUnsignedI32x4, // takes u8 as id

    V128ExtractLaneSignedI64x2, // takes u8 as id
    V128ExtractLaneUnsignedI64x2, // takes u8 as id

    V128ExtractLaneF32x4, // takes u8 as id
    V128ExtractLaneF64x2, // takes u8 as id

    // I8x16
    // I16x8
    // I32x4
    // I64x2
    // F32x4
    // F64x2
    // Shape.replace_lane
    V128ReplaceLaneI8x16, // takes u8 as id
    V128ReplaceLaneI16x8, // takes u8 as id
    V128ReplaceLaneI32x4, // takes u8 as id
    V128ReplaceLaneI64x2, // takes u8 as id
    V128ReplaceLaneF32x4, // takes u8 as id
    V128ReplaceLaneF64x2, // takes u8 as id
  
}