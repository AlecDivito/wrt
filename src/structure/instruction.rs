//! Wasm consists of a sequence of instructions. It uses a stack machine to manupulate
//! values on an implicit operand stack. It consumes (pops) values and processes
//! them and returns (push) the result.
//! 
//! In addition, some instructions have static _immediate arguments_. Typically
//! Indices or Type annotations.

// A list of instruction with a mark of "end" to mark the end of the expression.
pub type Expression = Vec<Instruction>;

pub enum Instruction {
    Numeric(NumericInstruction),
    Vector(VectorInstruction),
    Reference(ReferenceInstruction),
    Parametric(ParametricInstruction),
    Variable(VariableInstruction),
    Table(TableInstruction),
    Memory(MemoryInstruction),
    Control(ControlInstruction),
}


pub enum NumericInstruction {
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


/**
 * ## Vector instructions can be grouped into several subcategories:
 * Constants: return a static constant.
 * Unary Operations: consume one V128 operand and produce one V128 result.
 * Binary Operations: consume two V128 operands and produce one V128 result.
 * Ternary Operations: consume three V128 operands and produce one V128 result.
 * Tests: consume one V128 operand and produce a Boolean integer result.
 * Shifts: consume a V128 operand and a i32 operand, producing one V128 result.
 * Splats: consume a value of numeric type and produce a V128 result of a specified shape.
 * Extract lanes: consume a V128 operand and return the numeric value in a given lane.
 * Replace lanes: consume a V128 operand and a numeric value for a given lane, and produce a V128 result.
 */
pub enum VectorInstruction {
    // constant (return value)
    V128Const, // takes i128

    // vvunop (Unary)
    V128Not,

    // vvbinop
    V128And,
    V128AndNot,
    V128Or,
    V128Xor,

    // vvternop
    V128AnyTrue,
    // vvtestop
    V128AllTrue,
     
    V128ShuffleI8x16, // takes u8 as id
    V128SwizzleI8x16,

    // shape.splat
    V128SplatI8x16,
    V128SplatI16x8,
    V128SplatI32x4,
    V128SplatI64x2,
    V128SplatF32x4,
    V128SplatF64x2,

    // Extract Lane (get values from V128)
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

    // Shape.replace_lane (replace values in V128)
    V128ReplaceLaneI8x16, // takes u8 as id
    V128ReplaceLaneI16x8, // takes u8 as id
    V128ReplaceLaneI32x4, // takes u8 as id
    V128ReplaceLaneI64x2, // takes u8 as id
    V128ReplaceLaneF32x4, // takes u8 as id
    V128ReplaceLaneF64x2, // takes u8 as id

    // virelop (RelOp)
    V128EqualI8x16,
    V128NotEqualI8x16,
    V128LessThenSignedI8x16,
    V128LessThenUnsignedI8x16,
    V128GreaterThenSignedI8x16,
    V128GreaterThenUnsignedI8x16,
    V128LessOrEqualSignedI8x16,
    V128LessOrEqualUnsignedI8x16,
    V128GreaterOrEqualSignedI8x16,
    V128GreaterOrEqualUnsignedI8x16,

    V128EqualI16x8,
    V128NotEqualI16x8,
    V128LessThenSignedI16x8,
    V128LessThenUnsignedI16x8,
    V128GreaterThenSignedI16x8,
    V128GreaterThenUnsignedI16x8,
    V128LessOrEqualSignedI16x8,
    V128LessOrEqualUnsignedI16x8,
    V128GreaterOrEqualSignedI16x8,
    V128GreaterOrEqualUnsignedI16x8,

    V128EqualI32x4,
    V128NotEqualI32x4,
    V128LessThenSignedI32x4,
    V128LessThenUnsignedI32x4,
    V128GreaterThenSignedI32x4,
    V128GreaterThenUnsignedI32x4,
    V128LessOrEqualSignedI32x4,
    V128LessOrEqualUnsignedI32x4,
    V128GreaterOrEqualSignedI32x4,
    V128GreaterOrEqualUnsignedI32x4,

    V128EqualI64x2,
    V128NotEqualI64x2,
    V128LessThenSignedI64x2,
    V128GreaterThenSignedI64x2,
    V128LessOrEqualSignedI64x2,
    V128GreaterOrEqualSignedI64x2,
    
    // vfelop (RelOp)
    V128EqualF64x2,
    V128NotEqualF64x2,
    V128LessThenF64x2,
    V128GreaterThenF64x2,
    V128LessOrEqualF64x2,
    V128GreaterOrEqualF64x2,

    V128EqualF32x4,
    V128NotEqualF32x4,
    V128LessThenF32x4,
    V128GreaterThenF32x4,
    V128LessOrEqualF32x4,
    V128GreaterOrEqualF32x4,

    // vivnop
    V128AbsoluteI64x2,
    V128AbsoluteI32x4,
    V128AbsoluteI16x8,
    V128AbsoluteI8x16,

    V128NegativeI64x2,
    V128NegativeI32x4,
    V128NegativeI16x8,
    V128NegativeI8x16,

    // popcnt (Unary)
    V128PopCntI8x16,
    
    // q15mulr_sat (Binary operation)
    V128Q15MulrSatSignedI16x8,

    // dot product
    V128DotProductOfSignedI16x8FromSignedI32x4,

    // fshape.vfunop (Unary)
    V128AbsoluteF64x2,
    V128NegativeF64x2,
    V128SqrtF64x2,
    V128CeilF64x2,
    V128FloorF64x2,
    V128TruncateF64x2,
    V128NearestF64x2,

    V128AbsoluteF32x4,
    V128NegativeF32x4,
    V128SqrtF32x4,
    V128CeilF32x4,
    V128FloorF32x4,
    V128TruncateF32x4,
    V128NearestF32x4,

    // ishape.vitestop (Tests Operation)
    V128AllTrueI8x16,
    V128AllTrueI16x8,
    V128AllTrueI32x4,
    V128AllTrueI64x2,

    // ishape.bitmask
    V128BitMaskI8x16,
    V128BitMaskI16x8,
    V128BitMaskI32x4,
    V128BitMaskI64x2,

    // narrow
    V128NarrowI16x8ToI8x16Signed,
    V128NarrowI16x8ToI8x16Unsigned,
    V128NarrowI32x4ToI16x8Signed,
    V128NarrowI32x4ToI16x8Unsigned,

    // extend_half (vcvtop)
    V128ExtendLowI8x16ToI16x8Signed,
    V128ExtendLowI8x16ToI16x8Unsigned,
    V128ExtendHighI8x16ToI16x8Signed,
    V128ExtendHighI8x16ToI16x8Unsigned,

    V128ExtendLowI16x8ToI32x4Signed,
    V128ExtendLowI16x8ToI32x4Unsigned,
    V128ExtendHighI16x8ToI32x4Signed,
    V128ExtendHighI16x8ToI32x4Unsigned,

    V128ExtendLowI32x4ToI64x2Signed,
    V128ExtendLowI32x4ToI64x2Unsigned,
    V128ExtendHighI32x4ToI64xxSigned,
    V128ExtendHighI32x4ToI64xxUnsigned,

    // ishape.vishiftop
    V128ShlI8x16,
    V128ShlI16x8,
    V128ShlI32x4,
    V128ShlI64x2,

    V128ShrI8x16Signed,
    V128ShrI16x8Signed,
    V128ShrI32x4Signed,
    V128ShrI64x2Signed,

    V128ShrI8x16Unsigned,
    V128ShrI16x8Unsigned,
    V128ShrI32x4Unsigned,
    V128ShrI64x2Unsigned,

    // ishape.vibinop (Binary operation)
    V128AddI8x16,
    V128AddI16x8,
    V128AddI32x4,
    V128AddI64x2,
    
    V128SubtractI8x16,
    V128SubtractI16x8,
    V128SubtractI32x4,
    V128SubtractI64x2,

    // viminmaxop (Binary operation)
    V128MinimumI8x16Unsigned,
    V128MaximumI8x16Unsigned,
    V128MinimumI8x16Signed,
    V128MaximumI8x16Signed,

    V128MinimumI16x8Unsigned,
    V128MinimumI16x8Signed,
    V128MaximumI16x8Unsigned,
    V128MaximumI16x8Signed,

    V128MinimumI32x4Unsigned,
    V128MaximumI32x4Unsigned,
    V128MinimumI32x4Signed,
    V128MaximumI32x4Signed,

    // visatbinop (Binary operation)
    V128AddSatI8x16Signed,
    V128AddSatI8x16Unsigned,
    V128SubtractSatI8x16Signed,
    V128SubtractSatI8x16Unsigned,

    V128AddSatI16x8Signed,
    V128AddSatI16x8Unsigned,
    V128SubtractSatI16x8Signed,
    V128SubtractSatI16x8Unsigned,

    // mul (Binary operation)
    V128MultiplyI16x8,
    V128MultiplyI32x4,
    V128MultiplyI64x2,

    // avgr_u (Binary operation)
    V128AverageUnsignedI8x16,
    V128AverageUnsignedI16x8,

    // extmul 
    V128ExtendAndMultiplyHighI8x16ToI16x8Signed,
    V128ExtendAndMultiplyHighI8x16ToI16x8Unsigned,
    V128ExtendAndMultiplyLowI8x16ToI16x8Signed,
    V128ExtendAndMultiplyLowI8x16ToI16x8Unsigned,

    V128ExtendAndMultiplyHighI16x8ToI32x4Signed,
    V128ExtendAndMultiplyHighI16x8ToI32x4Unsigned,
    V128ExtendAndMultiplyLowI16x8ToI32x4Signed,
    V128ExtendAndMultiplyLowI16x8ToI32x4Unsigned,

    V128ExtendAndMultiplyHighI32x4ToI64x2Signed,
    V128ExtendAndMultiplyHighI32x4ToI64x2Unsigned,
    V128ExtendAndMultiplyLowI32x4ToI64x2Signed,
    V128ExtendAndMultiplyLowI32x4ToI64x2Unsigned,

    // extadd_pairwise
    V128ExtendAndAddPairwiseI8x16ToI16x8Signed,
    V128ExtendAndAddPairwiseI8x16ToI16x8Unsigned,

    V128ExtendAndAddPairwiseI16x8ToI32x4Signed,
    V128ExtendAndAddPairwiseI16x8ToI32x4Unsigned,

    // fshape.vfbinop (Binary operation)
    V128AddF32x4,
    V128SubtractF32x4,
    V128MultipleF32x4,
    V128DivideF32x4,
    V128MinimumF32x4,
    V128MaximumF32x4,
    V128PMinF32x4,
    V128PMaxF32x4,

    V128AddF64x2,
    V128SubtractF64x2,
    V128MultipleF64x2,
    V128DivideF64x2,
    V128MinimumF64x2,
    V128MaximumF64x2,
    V128PMinF64x2,
    V128PMaxF64x2,

    // trunc_sat (vcvtop)
    V128TruncateSatF32x4ToI32x4Signed,
    V128TruncateSatF32x4ToI32x4Unsigned,
    
    V128TruncateSatF62x2ToI32x4SignedZero,
    V128TruncateSatF62x2ToI32x4UnsignedZero,

    // convert, demote, promote (vcvtop)
    V128ConvertI32x4SignedToF32x4,
    V128ConvertI32x4UnsignedToF32x4,

    V128DemoteF64x2ToF32x4Zero,

    V128ConvertLowI32x4SignedToF64x2,
    V128ConvertLowI32x4UnsignedToF64x2,

    V128PromoteLowF32x4ToF64x2,
}


/// Instructions produce a null value, check null value or produce a reference to
/// a given function.
pub enum ReferenceInstruction {
    NullReference, // reftype (reference type)
    IsNullReference,
    FunctionReference, // funcidx (function index)
}

/// Operate on operands of any ['ValueType']
pub enum ParametricInstruction {
    // Simply throw away single operand
    Drop,
    // Select one of it's first two operands based on whether it's third operand is
    // zero or not. May include a ['ValueType'] determining the type of these
    // operands. If missing, the operands must be of numeric type
    Select,  // (valtype*)?
}

/// Access local or global variables. Get or set values respectively
pub enum VariableInstruction {
    GetLocal, // Local Index
    SetLocal, // Local Index
    TeeLocal, // Local Index (Set and return new value)

    GetGlobal, // Global index
    SetGlobal, // Global index
}

pub enum TableInstruction {
    // load or store element in table
    GetTable, // table index
    SetTable, // table index

    // get size of table
    SizeTable, // table index

    // grow table by delta, return previous size, -1 if space can't be allocated.
    // Take initialization value for new entires
    GrowTable, // table index

    // Set all entires in range with value given
    FillTable, // table index

    // Copy elements from source table to another region?
    CopyTable, // table index, table index

    // Copy elements from passive element segment into a table
    InitTable, // table index, element index

    // Prevent further use of a passive element segment. Intended to be used as
    // optimization hint. Free memory from this segment of the table.
    DropElement, // element index
}

/// Memory is accessed with load and store instructions for different ['NumberTypes']
/// They all take memarg which is the address offset and the expected alignment
/// (expressed as the exponent of a power of 2).
/// 
/// Integer loads and store can optional specify a storage size that is smaller
/// than the bit width of the respective value type.
/// 
/// Vector loads can specify a shape that is half the width of a V128. A "splat"
/// uses a single lane of specified storage size is loaded and duplicated to all
/// lanes.
/// 
/// Static address offset is added to dynamic address operand, yielding 33 bit
/// effective address that is zero based index at which the memory is accessed.
/// All values are read and written in little endian byte order. A trap results if
/// any accessed memory bytes lies outside the address range implied by the memories
/// current size. 
pub enum MemoryInstruction {
    LoadI32, // { offset u32, align u32 }
    LoadI64, // { offset u32, align u32 }
    LoadF32, // { offset u32, align u32 }
    LoadF64, // { offset u32, align u32 }
    LoadV128, // { offset u32, align u32 }

    StoreI32, // { offset u32, align u32 }
    StoreI64, // { offset u32, align u32 }
    StoreF32, // { offset u32, align u32 }
    StoreF64, // { offset u32, align u32 }
    StoreV128, // { offset u32, align u32 }

    // load
    Load8SignedI32,   // { offset u32, align u32 }
    Load8UnsignedI32, // { offset u32, align u32 }
    Load8SignedI64,   // { offset u32, align u32 }
    Load8UnsignedI64, // { offset u32, align u32 }

    Load16SignedI32,   // { offset u32, align u32 }
    Load16UnsignedI32, // { offset u32, align u32 }
    Load16SignedI64,   // { offset u32, align u32 }
    Load16UnsignedI64, // { offset u32, align u32 }

    Load32SignedI64,   // { offset u32, align u32 }
    Load32UnsignedI64, // { offset u32, align u32 }

    // store
    Store8SignedI32,   // { offset u32, align u32 }
    Store8UnsignedI32, // { offset u32, align u32 }
    Store8SignedI64,   // { offset u32, align u32 }
    Store8UnsignedI64, // { offset u32, align u32 }

    Store16SignedI32,   // { offset u32, align u32 }
    Store16UnsignedI32, // { offset u32, align u32 }
    Store16SignedI64,   // { offset u32, align u32 }
    Store16UnsignedI64, // { offset u32, align u32 }

    Store32SignedI64,   // { offset u32, align u32 }
    Store32UnsignedI64, // { offset u32, align u32 }

    // Load V128
    V128Load8x8Signed,   // { offset u32, align u32 }
    V128Load8x8Unsigned, // { offset u32, align u32 }

    V128Load16x4Signed,   // { offset u32, align u32 }
    V128Load16x4Unsigned, // { offset u32, align u32 }

    V128Load32x2Signed,   // { offset u32, align u32 }
    V128Load32x2Unsigned, // { offset u32, align u32 }

    V128Load32AndZero, // { offset u32, align u32 }
    V128Load64AndZero, // { offset u32, align u32 }

    V128Load8Splat,  // { offset u32, align u32 }
    V128Load16Splat, // { offset u32, align u32 }
    V128Load32Splat, // { offset u32, align u32 }
    V128Load64Splat, // { offset u32, align u32 }

    V128Load8Lane,  // { offset u32, align u32 }, Lane Index
    V128Load16Lane, // { offset u32, align u32 }, Lane Index
    V128Load32Lane, // { offset u32, align u32 }, Lane Index
    V128Load64Lane, // { offset u32, align u32 }, Lane Index

    // V128 Store
    V128Store8Lane,  // { offset u32, align u32 }, Lane Index
    V128Store16Lane, // { offset u32, align u32 }, Lane Index
    V128Store32Lane, // { offset u32, align u32 }, Lane Index
    V128Store64Lane, // { offset u32, align u32 }, Lane Index

    // operations
    MemorySize, // size of memory
    GrowMemory, // grow memory by delta or return -1. Operate in units of page size
    FillMemory, // Set all values in region to given byte
    CopyMemory, // copy data from source to destination (can overlap)
    InitMemory, // data index, copy data from passive data segment into memory
    DropData,   // data index, drop memory
}

pub enum ControlInstruction {
    // do nothing
    Nop,

    // cause a ['Trap']
    Unreachable,

    // structured instructions. nested sequences of instructions. This can take
    // input and produce output. Each structured control instruction introduces an
    // implicit label. Labels are targets for branch instructions that reference
    // them with label indices. Label indexes are relative to nesting depth. Ex.
    // 0 refers to the innermost structured control instruction. Increasing indices
    // refer to farther out. 
    Block, // block ([TypeIndex] | [ValueType]?) [Instruction]* end
    Loop,  // loop  ([TypeIndex] | [ValueType]?) [Instruction]* end
    If,    // if    ([TypeIndex] | [ValueType]?) [Instruction]* else [Instruction]* end

    // perform unconditional branch
    Br, // label index

    // perform conditional branch
    BrIf, // label index

    // perform indirect branch through an operand indexing into the label vector
    // that is an immediate to the instruction, or to a default target if the
    // operand is out of bounds.
    BrTable, // vec(label index) label index

    // shortcut for unconditional branch to the outermost block.
    Return,

    // Invoke another function, consume necessary arguments from the stack and
    // return the result values of the call.
    Call, // function index

    // call function indirectly through an operand indexing into a table. The
    // table index must be a [FunctionReference]. Function is dynamically checked
    // against the function type.
    CallIndirect, // Table index, type index

}