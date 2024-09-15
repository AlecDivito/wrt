use std::iter::Peekable;

use crate::{
    execution::{Number, Stack, Trap},
    parse::{
        ast::{read_number, read_u32, Error, Expect, Parse},
        Keyword, Token,
    },
    structure::{
        module::{get_id, get_next_keyword},
        types::{
            BlockType, DataIndex, ElementIndex, FloatVectorShape, FuncResult, FunctionIndex,
            GlobalType, HalfType, HeapType, Index, Instruction, IntType, IntegerVectorShape,
            LocalIndex, MemoryArgument, MemoryIndex, MemoryLoadNumber, MemoryWidth,
            MemoryZeroWidth, NumType, RefType, SignType, TableIndex, TypeIndex, ValueType, VecType,
            VectorMemoryOp, VectorShape,
        },
    },
};

use super::{Context, Input, ValidateInstruction, ValidateResult, ValidationError};

#[derive(Clone, Debug)]
pub enum Operation {
    Const(Const),
    Nop(NopOperation),
    GlobalGet(GlobalGetOperation),
    LocalGet(LocalGetOperation),
    Unreachable(UnreachableOperation),

    // Parametric
    Drop(DropOperation),
    Select(SelectOperation),

    // Control
    Block(BlockOperation),
    Call(CallOperation),
    Loop(LoopOperation),
    If(IfOperation),
    Break(BrOperation),
    BreakIf(BrIfOperation),
    BreakTable(BrTableOperation),
    Return(ReturnOperation),

    // Unary
    Test(TestOperation),
    Unary(UnaryOperation),

    // Binary
    Binary(BinaryOperation),

    // Reference
    RefNull(RefNull),
    RefIsNull(RefIsNull),
    RefFunc(RefFunc),
}
impl Default for Operation {
    fn default() -> Self {
        Operation::Nop(NopOperation {})
    }
}
impl ValidateInstruction for Operation {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        match self {
            Operation::Const(v) => v.validate(ctx, inputs),
            Operation::Nop(v) => v.validate(ctx, inputs),
            Operation::GlobalGet(v) => v.validate(ctx, inputs),
            Operation::LocalGet(v) => v.validate(ctx, inputs),
            Operation::Unreachable(v) => v.validate(ctx, inputs),
            Operation::Drop(v) => v.validate(ctx, inputs),
            Operation::Select(v) => v.validate(ctx, inputs),
            Operation::Block(v) => v.validate(ctx, inputs),
            Operation::Call(v) => v.validate(ctx, inputs),
            Operation::Loop(v) => v.validate(ctx, inputs),
            Operation::If(v) => v.validate(ctx, inputs),
            Operation::Break(v) => v.validate(ctx, inputs),
            Operation::BreakIf(v) => v.validate(ctx, inputs),
            Operation::BreakTable(v) => v.validate(ctx, inputs),
            Operation::Return(v) => v.validate(ctx, inputs),
            Operation::Test(v) => v.validate(ctx, inputs),
            Operation::Unary(v) => v.validate(ctx, inputs),
            Operation::Binary(v) => v.validate(ctx, inputs),
            Operation::RefNull(v) => v.validate(ctx, inputs),
            Operation::RefFunc(v) => v.validate(ctx, inputs),
            Operation::RefIsNull(v) => v.validate(ctx, inputs),
        }
    }
}

enum Literal {
    // These also double as constants
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

// enum Expression {
//     Literal(Literal),
//     Variable(String),

//     UnaryOp(UnaryOperator, Box<Expression>),
//     BinaryOp(BinaryOperator, Box<Expression>, Box<Expression>),
//     TestOp(TestOperator, Box<Expression>),
//     ComparisonOp(CompareOperator, Box<Expression>),
//     ConvertOp(ConvertOperator, Box<Expression>),

//     FunctionCall(String, Vec<Expression>),
// }

enum BlockInstruction {
    Block,
    If,
    Loop,
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Const(cst) => write!(f, "{}", cst),
            Operation::Nop(opt) => write!(f, "{}", opt),
            Operation::GlobalGet(opt) => write!(f, "{}", opt),
            Operation::LocalGet(opt) => write!(f, "{}", opt),
            Operation::Unreachable(opt) => write!(f, "{}", opt),
            Operation::Drop(opt) => write!(f, "{}", opt),
            Operation::Select(opt) => write!(f, "{}", opt),
            Operation::Block(opt) => write!(f, "{}", opt),
            Operation::Call(opt) => write!(f, "{}", opt),
            Operation::Loop(opt) => write!(f, "{}", opt),
            Operation::If(opt) => write!(f, "{}", opt),
            Operation::Break(opt) => write!(f, "{}", opt),
            Operation::BreakIf(opt) => write!(f, "{}", opt),
            Operation::BreakTable(opt) => write!(f, "{}", opt),
            Operation::Return(opt) => write!(f, "{}", opt),
            Operation::Test(opt) => write!(f, "{}", opt),
            Operation::Unary(opt) => write!(f, "{}", opt),
            Operation::Binary(opt) => write!(f, "{}", opt),
            Operation::RefNull(opt) => write!(f, "{}", opt),
            Operation::RefFunc(opt) => write!(f, "{}", opt),
            Operation::RefIsNull(opt) => write!(f, "{}", opt),
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Operation {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        let op = match tokens.next().expect_keyword()? {
            Keyword::FuncRef => todo!(),
            Keyword::ExternRef => todo!(),
            Keyword::VecShape(_) => todo!(),
            Keyword::Extern => todo!(),
            Keyword::Mut => todo!(),
            Keyword::Nop => Operation::Nop(NopOperation {}),
            Keyword::Unreachable => Operation::Unreachable(UnreachableOperation {}),
            Keyword::Drop => Operation::Drop(DropOperation {}),
            Keyword::Block => Operation::Block(BlockOperation::parse(tokens)?),
            Keyword::Loop => Operation::Loop(LoopOperation::parse(tokens)?),
            Keyword::End => todo!(),
            Keyword::Br => Operation::Break(BrOperation::parse(tokens)?),
            Keyword::BrIf => Operation::BreakIf(BrIfOperation::parse(tokens)?),
            Keyword::BrTable => Operation::BreakTable(BrTableOperation::parse(tokens)?),
            Keyword::Return => Operation::Return(ReturnOperation {}),
            Keyword::If => Operation::If(IfOperation::parse(tokens)?),
            Keyword::Select => Operation::Select(SelectOperation::parse(tokens)?),
            Keyword::Call => Operation::Call(CallOperation::parse(tokens)?),
            Keyword::CallIndirect => todo!(),
            Keyword::LocalGet => Operation::LocalGet(LocalGetOperation::parse(tokens)?),
            Keyword::LocalSet => todo!(),
            Keyword::LocalTee => todo!(),
            Keyword::GlobalGet => Operation::GlobalGet(GlobalGetOperation::parse(tokens)?),
            Keyword::GlobalSet => todo!(),
            Keyword::TableGet => todo!(),
            Keyword::TableSet => todo!(),
            Keyword::TableSize => todo!(),
            Keyword::TableGrow => todo!(),
            Keyword::TableFill => todo!(),
            Keyword::TableCopy => todo!(),
            Keyword::TableInit => todo!(),
            Keyword::ElemDrop => todo!(),
            Keyword::MemorySize => todo!(),
            Keyword::MemoryGrow => todo!(),
            Keyword::MemoryFill => todo!(),
            Keyword::MemoryCopy => todo!(),
            Keyword::MemoryInit => todo!(),
            Keyword::DataDrop => todo!(),
            Keyword::Load(_) => todo!(),
            Keyword::Store(_) => todo!(),
            Keyword::I32Load8(_) => todo!(),
            Keyword::I32Load16(_) => todo!(),
            Keyword::I64Load8(_) => todo!(),
            Keyword::I64Load16(_) => todo!(),
            Keyword::I64Load32(_) => todo!(),
            Keyword::I32Store8 => todo!(),
            Keyword::I32Store16 => todo!(),
            Keyword::I64Store8 => todo!(),
            Keyword::I64Store16 => todo!(),
            Keyword::I64Store32 => todo!(),
            Keyword::MemArgsAlign(_) => todo!(),
            Keyword::MemArgsOffset(_) => todo!(),
            // Keyword::Declare => todo!(),
            // Keyword::Offset => todo!(),
            // Keyword::Item => todo!(),
            Keyword::V128Load => todo!(),
            Keyword::V128Store => todo!(),
            Keyword::VecLoad8x8(_) => todo!(),
            Keyword::VecLoad16x4(_) => todo!(),
            Keyword::VecLoad32x2(_) => todo!(),
            Keyword::VecLoadSplat(_) => todo!(),
            Keyword::VecLoadZero(_) => todo!(),
            Keyword::VecLoadLane(_) => todo!(),
            Keyword::VecStoreLane(_) => todo!(),
            Keyword::Const(expected) => {
                let value = read_number(expected, tokens.next().expect_number()?)?;
                Operation::Const(Const::new(expected, value))
            }
            Keyword::ConstV128 => todo!(),
            Keyword::RefNull => Operation::RefNull(RefNull::parse(tokens)?),
            Keyword::RefFunc => Operation::RefFunc(RefFunc::parse(tokens)?),
            Keyword::RefIsNull => Operation::RefIsNull(RefIsNull),
            Keyword::RefExtern => todo!(),
            Keyword::IntClz(_) => todo!(),
            Keyword::IntCtz(_) => todo!(),
            Keyword::IntPopCnt(_) => todo!(),
            Keyword::IntExtend8Signed(_) => todo!(),
            Keyword::IntExtend16Signed(_) => todo!(),
            Keyword::I64Extend32Signed => todo!(),

            Keyword::NegativeFloat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Neg))
            }
            Keyword::AbsoluteFloat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Abs))
            }
            Keyword::SquareRootFloat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Sqrt))
            }
            Keyword::CeilFloat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Ceil))
            }
            Keyword::FloorFLoat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Floor))
            }
            Keyword::NearestFloat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Nearest))
            }
            Keyword::TruncateFloat(expected) => {
                Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Trunc))
            }
            Keyword::AddInt(expected) => {
                Operation::Binary(BinaryOperation::new(BinaryFunction::Add(expected)))
            }
            Keyword::SubInt(_) => todo!(),
            Keyword::MultiplyInt(_) => todo!(),
            Keyword::AndInt(_) => todo!(),
            Keyword::OrInt(_) => todo!(),
            Keyword::XORInt(_) => todo!(),
            Keyword::ShiftLeftInt(_) => todo!(),
            Keyword::DivideInt { shape, sign } => todo!(),
            Keyword::RemainderInt { shape, sign } => todo!(),
            Keyword::ShiftRightInt { shape, sign } => todo!(),
            Keyword::RotateInt { shape, direction } => todo!(),
            Keyword::AddFloat(_) => todo!(),
            Keyword::SubFloat(_) => todo!(),
            Keyword::MultiplyFloat(_) => todo!(),
            Keyword::DivideFloat(_) => todo!(),
            Keyword::MinFloat(_) => todo!(),
            Keyword::MaxFloat(_) => todo!(),
            Keyword::CopySign(_) => todo!(),

            // Testing
            Keyword::I32EqualTest => Operation::Test(TestOperation::new(IntType::I32)),
            Keyword::I64EqualTest => Operation::Test(TestOperation::new(IntType::I64)),

            Keyword::CompareIntEqual(_) => todo!(),
            Keyword::CompareIntNotEqual(_) => todo!(),
            Keyword::CompareIntLessThen { shape, sign } => todo!(),
            Keyword::CompareIntLessOrEqual { shape, sign } => todo!(),
            Keyword::CompareIntGreaterThen { shape, sign } => todo!(),
            Keyword::CompareIntGreaterOrEqual { shape, sign } => todo!(),
            Keyword::CompareFloatEqual(_) => todo!(),
            Keyword::CompareFloatNotEqual(_) => todo!(),
            Keyword::CompareFloatLessThen(_) => todo!(),
            Keyword::CompareFloatLessOrEqual(_) => todo!(),
            Keyword::CompareFloatGreaterThen(_) => todo!(),
            Keyword::CompareFloatGreaterOrEqual(_) => todo!(),
            Keyword::I32WrapI64 => todo!(),
            Keyword::I64ExtendI32(_) => todo!(),
            Keyword::F32DemoteF64 => todo!(),
            Keyword::F64PromoteF32 => todo!(),
            Keyword::I32TruncateF32(_) => todo!(),
            Keyword::I64TruncateF32(_) => todo!(),
            Keyword::I32TruncateF64(_) => todo!(),
            Keyword::I64TruncateF64(_) => todo!(),
            Keyword::I32TruncateSatF32(_) => todo!(),
            Keyword::I64TruncateSatF32(_) => todo!(),
            Keyword::I32TruncateSatF64(_) => todo!(),
            Keyword::I64TruncateSatF64(_) => todo!(),
            Keyword::F32ConvertI32(_) => todo!(),
            Keyword::F32ConvertI64(_) => todo!(),
            Keyword::F64ConvertI32(_) => todo!(),
            Keyword::F64ConvertI64(_) => todo!(),
            Keyword::F32ReinterpretI32 => todo!(),
            Keyword::F64ReinterpretI64 => todo!(),
            Keyword::I32ReinterpretI32 => todo!(),
            Keyword::I64ReinterpretI64 => todo!(),
            Keyword::V128Not => todo!(),
            Keyword::V128And => todo!(),
            Keyword::V128AndNot => todo!(),
            Keyword::V128Or => todo!(),
            Keyword::V128XOr => todo!(),
            Keyword::V128BitSelect => todo!(),
            Keyword::V128AnyTrue => todo!(),
            Keyword::VecIntNegative(_) => todo!(),
            Keyword::VecIntAbsolute(_) => todo!(),
            Keyword::VecI8x16PopCnt => todo!(),
            Keyword::VecI8x16AverageUnsigned => todo!(),
            Keyword::VecI16x8AverageUnsigned => todo!(),
            Keyword::VecFloatNegative(_) => todo!(),
            Keyword::VecFloatAbsolute(_) => todo!(),
            Keyword::VecFloatSquareRoot(_) => todo!(),
            Keyword::VecFloatCeil(_) => todo!(),
            Keyword::VecFloatFloor(_) => todo!(),
            Keyword::VecFloatTruncate(_) => todo!(),
            Keyword::VecFloatNearest(_) => todo!(),
            Keyword::I32x4TruncSatF32x4(_) => todo!(),
            Keyword::I32x4TruncSatF64x2Zero(_) => todo!(),
            Keyword::F64x2PromoteLowF32x4 => todo!(),
            Keyword::F32x4PemoteF64x2Zero => todo!(),
            Keyword::F32x4ConvertI32x4(_) => todo!(),
            Keyword::F64x2ConvertLowI32x4(_) => todo!(),
            Keyword::I16x8ExtendAddPairwiseI8x16(_) => todo!(),
            Keyword::I32x4ExtaddPairwiseI16x8(_) => todo!(),
            Keyword::VecIntEqual(_) => todo!(),
            Keyword::VecIntNotEqual(_) => todo!(),
            Keyword::VecIntLessThen { shape, sign } => todo!(),
            Keyword::VecIntLessOrEqual { shape, sign } => todo!(),
            Keyword::VecIntGreaterThen { shape, sign } => todo!(),
            Keyword::VecIntGreaterOrEqual { shape, sign } => todo!(),
            Keyword::VecEqualFloat(_) => todo!(),
            Keyword::VecNotEqualFloat(_) => todo!(),
            Keyword::VecLessThenFloat(_) => todo!(),
            Keyword::VecLessOrEqualFloat(_) => todo!(),
            Keyword::VecGreaterThenFloat(_) => todo!(),
            Keyword::VecGreaterOrEqualFloat(_) => todo!(),
            Keyword::VecSwizzleFloatI8x16 => todo!(),
            Keyword::VecIntAdd(_) => todo!(),
            Keyword::VecIntSub(_) => todo!(),
            Keyword::VecIntMultiplyI16x8 => todo!(),
            Keyword::VecIntMultiplyI32x4 => todo!(),
            Keyword::VecIntMultiplyI64x2 => todo!(),
            Keyword::VecAddSatI16x8(_) => todo!(),
            Keyword::VecSubtractSatI16x8(_) => todo!(),
            Keyword::VecI32x4DotProductOfI16x8Signed => todo!(),
            Keyword::VecMinInt { shape, sign } => todo!(),
            Keyword::VecMaxInt { shape, sign } => todo!(),
            Keyword::VecSubFloat(_) => todo!(),
            Keyword::VecAddSatI8x16(_) => todo!(),
            Keyword::VecSubtractSatI8x16(_) => todo!(),
            Keyword::VecAddFloat(_) => todo!(),
            Keyword::VecDivFloat(_) => todo!(),
            Keyword::VecMulFloat(_) => todo!(),
            Keyword::VecMinFloat(_) => todo!(),
            Keyword::VecMaxFloat(_) => todo!(),
            Keyword::VecPMin(_) => todo!(),
            Keyword::VecPMax(_) => todo!(),
            Keyword::I16x8Q15mulrSatS => todo!(),
            Keyword::I8x16NarrowI16x8(_) => todo!(),
            Keyword::I16x8NarrowI32x4(_) => todo!(),
            Keyword::I16x8ExtendI8x16 { half, sign } => todo!(),
            Keyword::I32x4ExtendI16x8 { half, sign } => todo!(),
            Keyword::I64x2ExtendI32x4 { half, sign } => todo!(),
            Keyword::I16x8ExtendMultiplyI8x16 { half, sign } => todo!(),
            Keyword::I32x4ExtendMultiplyI16x8 { half, sign } => todo!(),
            Keyword::I64x2ExtendMultiplyI32x4 { half, sign } => todo!(),
            Keyword::VecTest(_) => todo!(),
            Keyword::VecBitmask(_) => todo!(),
            Keyword::VecShiftLeft(_) => todo!(),
            Keyword::VecShiftRight { shape, sign } => todo!(),
            Keyword::VecShuffle => todo!(),
            Keyword::VecSplat(_) => todo!(),
            Keyword::VecExtract { shape, sign } => todo!(),
            Keyword::VecReplate(_) => todo!(),
            Keyword::Module => todo!(),
            Keyword::Bin => todo!(),
            Keyword::Quote => todo!(),
            Keyword::Script => todo!(),
            Keyword::Register => todo!(),
            Keyword::Invoke => todo!(),
            Keyword::Get => todo!(),
            Keyword::AssertMalformed => todo!(),
            Keyword::AssertInvalid => todo!(),
            Keyword::AssertUnlinkable => todo!(),
            Keyword::AssertReturn => todo!(),
            Keyword::AssertTrap => todo!(),
            Keyword::AssertExhaustion => todo!(),
            Keyword::NaNCanonical => todo!(),
            Keyword::NaNArithmetic(_) => todo!(),
            Keyword::Infinit => todo!(),
            Keyword::NaN => todo!(),
            Keyword::Input => todo!(),
            Keyword::Output => todo!(),
            keyword => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    format!("{:?} is not an instruction. It can't be used.", keyword),
                ))
            }
        };
        Ok(op)
    }
}

pub trait Execute {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap>;
}

/**
 * Number Instructions
 */

// Validate Const operations
#[derive(Clone, Debug)]
pub struct Const {
    ty: NumType,
    value: Number,
}
impl Default for Const {
    fn default() -> Self {
        Self {
            ty: NumType::I32,
            value: Number::I32(0),
        }
    }
}
impl std::fmt::Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}.const {})", self.ty, self.value)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Const {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        // TODO(Alec): In the future select may allow for more data to be selected
        match tokens.next().expect_keyword()? {
            Keyword::Const(expected) => Ok(Const::new(
                expected,
                read_number(expected, tokens.next().expect_number()?)?,
            )),
            keyword => Err(Error::new(
                tokens.next().cloned(),
                format!("Expected const.xx but got keyword {:?}", keyword),
            )),
        }
    }
}
impl Const {
    pub fn new(ty: NumType, value: Number) -> Self {
        Self { ty, value }
    }
}
impl ValidateInstruction for Const {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::Num(self.ty)])
    }
}
impl Execute for Const {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
        if self.ty != self.value.ty() {
            Err(Trap::new())
        } else {
            stack.push(self.value);
            Ok(())
        }
    }
}

#[derive(Clone, Debug)]
pub enum UnaryFunction {
    // interger
    Clz,
    Ctz,
    PopCnt,
    // Float
    Abs,
    Neg,
    Sqrt,
    Ceil,
    Floor,
    Trunc,
    Nearest,
}
impl std::fmt::Display for UnaryFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryFunction::Clz => write!(f, "clz"),
            UnaryFunction::Ctz => write!(f, "ctz"),
            UnaryFunction::PopCnt => write!(f, "popcnt"),
            UnaryFunction::Abs => write!(f, "abs"),
            UnaryFunction::Neg => write!(f, "neg"),
            UnaryFunction::Sqrt => write!(f, "sqrt"),
            UnaryFunction::Ceil => write!(f, "ceil"),
            UnaryFunction::Floor => write!(f, "floor"),
            UnaryFunction::Trunc => write!(f, "trunc"),
            UnaryFunction::Nearest => write!(f, "nearest"),
        }
    }
}

// Validate Unary Operations. Only available for numbers.
#[derive(Clone, Debug)]
pub struct UnaryOperation {
    ty: NumType,
    op: UnaryFunction,
}
impl UnaryOperation {
    pub fn new(ty: impl Into<NumType>, op: UnaryFunction) -> Self {
        Self { ty: ty.into(), op }
    }
}
impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}.{})", self.ty, self.op)
    }
}
impl ValidateInstruction for UnaryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, input: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let value = input.pop()?;
        if value.try_into_num()? == self.ty {
            Ok(vec![value])
        } else {
            Err(ValidationError::new())
        }
    }
}
impl Execute for UnaryOperation {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
        let _number = stack.pop_and_assert_num(self.ty)?;
        todo!("Implment Unary Function");
        Ok(())
    }
}

// Validate Binary Operation. Only avaliable for numbers.
#[derive(Clone, Debug)]
pub enum BinaryFunction {
    Add(IntType),
}
impl std::fmt::Display for BinaryFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryFunction::Add(ty) => write!(f, "{}.add", ty),
        }
    }
}
impl Default for BinaryFunction {
    fn default() -> Self {
        Self::Add(IntType::default())
    }
}
#[derive(Clone, Default, Debug)]
pub struct BinaryOperation {
    func: BinaryFunction,
}
impl BinaryOperation {
    pub fn new(func: BinaryFunction) -> Self {
        Self { func }
    }
}
impl std::fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.func)
    }
}
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
#[derive(Clone, Debug)]
pub struct TestOperation {
    ty: IntType,
}
impl TestOperation {
    pub fn new(ty: IntType) -> Self {
        Self { ty }
    }
}
impl std::fmt::Display for TestOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}.eqz)", self.ty)
    }
}
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
    function_index: FunctionIndex,
}
impl ValidateInstruction for FunctionReferenceOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        todo!("Validation is another day :(")
        // let _ = ctx.get_function(&Index::Index(self.function_index))?;
        // if ctx.contains_reference(self.function_index) {
        //     // Reference: https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-ref-mathsf-ref-func-x
        //     // TODO(Alec): We are supposed to be returning a function ref
        //     // A function ref is basically a pointer to a function
        //     // At runtime we validate if the function call is given the correct
        //     // arguments. So i think just returning the index to the function
        //     // should be ok because it's technically a pointer.
        //     return Ok(vec![ValueType::RefType(RefType::FuncRef(
        //         self.function_index as FunctionReference,
        //     ))]);
        // } else {
        //     Err(ValidationError::new())
        // }
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
pub struct VectorShuffleOperation {
    // Index lane can't be larger then the number 32
    lane_index: LaneIndex,
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
pub struct VectorSplatOperation {
    shape: VectorShape,
}
impl ValidateInstruction for VectorSplatOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, _: &mut Context, _inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
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
        let _shape = inputs.pop()?.try_into_num()?; // Get the value type from the first operand
        inputs.pop()?.try_into_vec_type()?; // Get the vector
        if self.lane_index.validate_against_shape(self.shape) {
            // } && shape != self.shape {
            Err(ValidationError::new())
        } else {
            Ok(vec![ValueType::VecType(VecType)])
        }
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-vunop-mathit-vunop
pub struct VectorShapeUnaryOperation {
    _shape: VectorShape,
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
    _shape: VectorShape,
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
    _shape: VectorShape,
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
    _shape: IntegerVectorShape,
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
    _shape: VectorShape,
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
    _half: HalfType,
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
    _desired_sign: SignType,
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
    _shape: IntegerVectorShape,
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
    _desired_sign: SignType,
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
    _desired_sign: SignType,
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
#[derive(Clone, Debug)]
pub struct DropOperation {}
impl std::fmt::Display for DropOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(drop)")
    }
}
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

#[derive(Clone, Debug)]
pub struct SelectOperation {
    t: Option<ValueType>,

    op1: Box<Operation>,
    op2: Box<Operation>,
    op3: Box<Operation>,
}
impl std::fmt::Display for SelectOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(select {} {} {})", self.op1, self.op2, self.op3)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for SelectOperation {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        // TODO(Alec): In the future select may allow for more data to be selected
        let t = if Some(Keyword::Result) == get_next_keyword(tokens) {
            let mut result = FuncResult::parse(tokens)?.0;
            let ty = result.pop().map(|ty| *ty.ty());
            if !result.is_empty() {
                return Err(Error::new(
                    tokens.next().cloned(),
                    "Select result block has too many arguments".to_string(),
                ));
            }
            ty
        } else {
            None
        };

        let op1 = Box::new(Operation::parse(tokens)?);
        let op2 = Box::new(Operation::parse(tokens)?);
        let op3 = Box::new(Operation::parse(tokens)?);

        Ok(Self { t, op1, op2, op3 })
    }
}
impl ValidateInstruction for SelectOperation {
    // type Output = [ValueType; 1];

    fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // First we always pop an I32
        inputs.pop()?.try_into_num()?.try_into_i32()?;
        let ty1 = inputs.pop()?;
        let ty2 = inputs.pop()?;
        match self.t {
            Some(ty) => {
                if ty1 == ty2 && ty == ty1 && ty == ty2 {
                    Ok(vec![ty1])
                } else {
                    Err(ValidationError::new())
                }
            }
            None => {
                if ty1 == ty2 && (ty1.is_num() || ty1.is_vec_type()) {
                    Ok(vec![ty1])
                } else {
                    Err(ValidationError::new())
                }
            }
        }
    }
}

/**
 * Variable Instructions
 */

#[derive(Clone, Debug)]
pub struct LocalGetOperation {
    index: Index,
}
impl std::fmt::Display for LocalGetOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(local.get {})", self.index)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LocalGetOperation {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        Ok(Self {
            index: Index::parse(tokens)?,
        })
    }
}
impl ValidateInstruction for LocalGetOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let a = *ctx.get_local(&self.index)?;
        Ok(vec![a])
    }
}

pub struct LocalSetOperation {
    index: LocalIndex,
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
    index: LocalIndex,
}
impl ValidateInstruction for LocalTeeOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = inputs.pop()?;
        ctx.set_local(self.index, ty.clone())?;
        Ok(vec![ty])
    }
}

#[derive(Clone, Debug)]
pub struct GlobalGetOperation {
    index: Index,
}
impl std::fmt::Display for GlobalGetOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(global.get {})", self.index)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for GlobalGetOperation {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        let index = Index::Index(read_u32(tokens.next().expect_number()?)?);
        Ok(Self { index })
    }
}
impl ValidateInstruction for GlobalGetOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let global = ctx.get_global(&self.index)?;
        match global {
            // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-global-get-x
            // Why can't we get a const?
            GlobalType::Const(_) => Err(ValidationError::new()),
            GlobalType::Var(ty) => Ok(vec![ty.clone()]),
        }
    }
}

pub struct GlobalSetOperation {
    index: LocalIndex,
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
    index: TableIndex,
}
impl ValidateInstruction for TableGetOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(&Index::Index(self.index))?;
        inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![ValueType::RefType(*table.ref_type())])
    }
}

pub struct TableSetOperation {
    index: TableIndex,
}
impl ValidateInstruction for TableSetOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(&Index::Index(self.index))?;
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
    index: TableIndex,
}
impl ValidateInstruction for TableSizeOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _table = ctx.get_table(&Index::Index(self.index))?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

pub struct TableGrowOperation {
    index: TableIndex,
}
impl ValidateInstruction for TableGrowOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(&Index::Index(self.index))?;

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
    index: TableIndex,
}
impl ValidateInstruction for TableFillOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(&Index::Index(self.index))?;

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
    table_y_index: TableIndex,
}
impl ValidateInstruction for TableCopyOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table_x = ctx.get_table(&Index::Index(self.table_x_index))?;
        let table_y = ctx.get_table(&Index::Index(self.table_y_index))?;
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
    element: ElementIndex,
}
impl ValidateInstruction for TableInitOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let table = ctx.get_table(&Index::Index(self.table))?;
        let element = ctx.get_element(&Index::Index(self.element))?;
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
    element: ElementIndex,
}
impl ValidateInstruction for ElementDropOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, _inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        ctx.get_element(&Index::Index(self.element))?;
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
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
    args: MemoryArgument,
}
impl ValidateInstruction for VectorLoadMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
    args: MemoryArgument,
}
impl ValidateInstruction for VectorLoadNSplatMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
    args: MemoryArgument,
}
impl ValidateInstruction for VectorLoadNZeroMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
    args: MemoryArgument,
}
impl ValidateInstruction for VectorLoadNLaneMemoryOperation {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if !self.index.validate_against_memory_width(&self.width_n) {
            return Err(ValidationError::new());
        }

        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
    args: MemoryArgument,
}
impl ValidateInstruction for VectorStoreNLaneMemoryOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        if !self.index.validate_against_memory_width(&self.width_n) {
            return Err(ValidationError::new());
        }

        let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
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
    fn validate(&self, ctx: &mut Context, _inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0.map(Index::Index).as_ref())?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

pub struct MemoryGrow(Option<MemoryIndex>);
impl ValidateInstruction for MemoryGrow {
    // type Output = [ValueType; 1];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0.map(Index::Index).as_ref())?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}

pub struct MemoryFill(Option<MemoryIndex>);
impl ValidateInstruction for MemoryFill {
    // type Output = [ValueType; 0];
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let _ = ctx.get_memory(self.0.map(Index::Index).as_ref())?;
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
        let _ = ctx.get_memory(self.0.map(Index::Index).as_ref())?;
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
        let _ = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
        let _ = ctx.get_data(&Index::Index(self.data))?;
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
        let _ = ctx.get_data(&Index::Index(self.data))?;
        Ok(vec![])
    }
}

/**
 * Control Instructions
 */
#[derive(Clone, Debug)]
pub struct NopOperation;
impl std::fmt::Display for NopOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(nop)")
    }
}
impl Execute for NopOperation {
    fn exec(&self, _: &mut Stack) -> Result<(), Trap> {
        Ok(())
    }
}
impl ValidateInstruction for NopOperation {
    // type Output = [ValueType; 0];
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![])
    }
}

#[derive(Clone, Debug)]
pub struct UnreachableOperation;
impl std::fmt::Display for UnreachableOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(unreachable)")
    }
}
impl ValidateInstruction for UnreachableOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, _: &mut Context, _input: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-unreachable
        Ok(vec![])
    }
}

// block _blocktype_ _instr_* end
#[derive(Clone, Default, Debug)]
pub struct BlockOperation {
    label: Option<String>,
    ty: Option<BlockType>,
    ops: Instruction,
}
impl std::fmt::Display for BlockOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(block")?;
        if let Some(label) = self.label.as_ref() {
            write!(f, " ${}", label)?;
        }
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " {}", ty)?;
        }
        if self.ops.is_empty() {
            write!(f, ")")
        } else {
            f.pad(&" ".repeat(2))?;
            write!(f, "\n{}", self.ops)?;
            f.pad(&" ".repeat(4))?;
            write!(f, ")")
        }
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BlockOperation {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        if tokens.peek().copied().expect_right_paren().is_ok() {
            return Ok(BlockOperation::default());
        }
        // Copied from FunctionDefinition
        let label = get_id(tokens);

        if tokens.peek().copied().expect_right_paren().is_ok() {
            return Ok(BlockOperation {
                label,
                ..Default::default()
            });
        }

        let ty = match get_next_keyword(tokens) {
            Some(Keyword::Type) => Some(BlockType::parse(tokens)?),
            Some(Keyword::Result) => Some(BlockType::parse(tokens)?),
            _ => None,
        };
        let ops = Instruction::parse(tokens)?;

        Ok(Self { label, ty, ops })
    }
}
impl ValidateInstruction for BlockOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // Block type must be some function type
        todo!("validate meeee");
        // let ty = self.ty.get_function_type(ctx)?;
        // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
        // ctx.prepend_label(ty.output().clone());
        // TODO(Alec): Validate how to execute instructions
        todo!("implement instructions for a given opcode for a block");
        // let output = self.instructions.validate(ctx, inputs)?;
        // remove the label
        // let label = ctx.remove_prepend_label()?;
        // // validate the output matches the function output
        // if *ty.output() == label && *ty.output().values() == output {
        //     // This should never trigger
        //     for input_ty in ty.input().values() {
        //         let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
        //     }
        //     return Ok(ty.output().clone().values().to_vec());
        // }
        // Err(ValidationError::new())
    }
}

// loop _blocktype_ _instr_* end
#[derive(Clone, Default, Debug)]
pub struct LoopOperation {
    label: Option<String>,
    ty: Option<BlockType>,
    ops: Instruction,
}
impl std::fmt::Display for LoopOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(loop")?;
        if let Some(label) = self.label.as_ref() {
            write!(f, " ${}", label)?;
        }
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " {}", ty)?;
        }
        if self.ops.is_empty() {
            write!(f, ")")
        } else {
            writeln!(f, "{}", self.ops)?;
            write!(f, ")")
        }
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LoopOperation {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        if tokens.peek().copied().expect_right_paren().is_ok() {
            return Ok(LoopOperation::default());
        }
        let label = get_id(tokens);
        let ty = match get_next_keyword(tokens) {
            Some(Keyword::Type) => Some(BlockType::parse(tokens)?),
            Some(Keyword::Result) => Some(BlockType::parse(tokens)?),
            _ => None,
        };
        let instructions = Instruction::parse(tokens)?;
        Ok(Self {
            label,
            ty,
            ops: instructions,
        })
    }
}
impl ValidateInstruction for LoopOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        todo!("Validate loop operations")
        // // Block type must be some function type
        // let ty = self.ty.get_function_type(ctx)?;
        // // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
        // ctx.prepend_label(ty.output().clone());
        // // TODO(Alec): Validate how to execute instructions
        // let output = self.instructions.validate(ctx, inputs)?;
        // // remove the label
        // let label = ctx.remove_prepend_label()?;
        // if *ty.output() == label && *ty.output().values() == output {
        //     // This should never trigger
        //     for input_ty in ty.input().values() {
        //         let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
        //     }
        //     return Ok(ty.output().clone().values().to_vec());
        // }
        // Err(ValidationError::new())
    }
}

// if _blocktype_ _instr_* else _instr_* end
#[derive(Clone, Default, Debug)]
pub struct IfOperation {
    label: Option<String>,
    ty: Option<BlockType>,
    condition: Option<Instruction>,
    thens: Instruction,
    elses: Option<Instruction>,
}
impl std::fmt::Display for IfOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(if")?;
        if let Some(label) = self.label.as_ref() {
            write!(f, " ${}", label)?;
        }
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " {}", ty)?;
        }
        if let Some(condition) = self.condition.as_ref() {
            write!(f, "{}", condition)?;
        }
        if self.thens.is_empty() {
            writeln!(f, "(then)")?;
        } else {
            writeln!(f, "(then {})", self.thens)?;
        }
        if let Some(elses) = self.elses.as_ref() {
            if elses.is_empty() {
                writeln!(f, "(else)")?;
            } else {
                writeln!(f, "(else {})", elses)?;
            }
        }

        Ok(())
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for IfOperation {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        if tokens.peek().copied().expect_right_paren().is_ok() {
            return Ok(IfOperation::default());
        }

        let label = get_id(tokens);
        let ty = match get_next_keyword(tokens) {
            Some(Keyword::Type) => Some(BlockType::parse(tokens)?),
            Some(Keyword::Result) => Some(BlockType::parse(tokens)?),
            _ => None,
        };

        let condition = match get_next_keyword(tokens) {
            Some(Keyword::Then) | Some(Keyword::Else) | None => None,
            Some(_) => Some(Instruction::parse(tokens)?),
        };

        let mut thens = Instruction::new();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Then)?;
        if !tokens.peek().copied().expect_right_paren().is_ok() {
            thens = Instruction::parse(tokens)?;
        }
        tokens.next().expect_right_paren()?;

        let mut elses = None;
        if Some(Keyword::Else) == get_next_keyword(tokens) {
            tokens.next().expect_left_paren()?;
            tokens.next().expect_keyword_token(Keyword::Else)?;
            if !tokens.peek().copied().expect_right_paren().is_ok() {
                elses = Some(Instruction::parse(tokens)?);
            }
            tokens.next().expect_right_paren()?;
        };

        Ok(Self {
            label,
            ty,
            condition,
            thens,
            elses,
        })
    }
}
impl ValidateInstruction for IfOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        todo!("Validation for if")
        // // Block type must be some function type
        // let ty = self.ty.get_function_type(ctx)?;
        // // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
        // ctx.prepend_label(ty.output().clone());
        // // TODO(Alec): Validate how to execute instructions
        // let o1 = self.t1.validate(ctx, inputs)?;
        // let o2 = self.t2.validate(ctx, inputs)?;
        // // remove the label
        // let label = ctx.remove_prepend_label()?;
        // // validate the output matches the functions output
        // if label.values() == o1
        //     && label.values() == o2
        //     && *ty.output().values() == o1
        //     && *ty.output().values() == o2
        // {
        //     // validate that the input stack has an i32 and all of of the function types arguments
        //     let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        //     for input_ty in ty.input().values() {
        //         let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
        //     }
        //     return Ok(ty.output().clone().values().to_vec());
        // }
        // Err(ValidationError::new())
    }
}
#[derive(Clone, Default, Debug)]
pub struct BrOperation {
    label: Index,
}
impl std::fmt::Display for BrOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(br {})", self.label)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BrOperation {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let label = if let Some(label) = get_id(tokens) {
            Index::Id(label)
        } else {
            Index::Index(read_u32(tokens.next().expect_number()?)?)
        };

        Ok(Self { label })
    }
}
impl ValidateInstruction for BrOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = ctx.get_label(&self.label)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        for input_ty in ty.values() {
            let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
        }
        Ok(ty.values().to_vec())
    }
}

#[derive(Clone, Default, Debug)]
pub struct BrIfOperation {
    label: Index,
}
impl std::fmt::Display for BrIfOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(br_if {})", self.label)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BrIfOperation {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let label = if let Some(label) = get_id(tokens) {
            Index::Id(label)
        } else {
            Index::Index(read_u32(tokens.next().expect_number()?)?)
        };

        Ok(Self { label })
    }
}
impl ValidateInstruction for BrIfOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = ctx.get_label(&self.label)?;
        let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        for input_ty in ty.values() {
            let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
        }
        Ok(ty.values().to_vec())
    }
}

////////////////////////////////////////////////////////////////////////////////
/// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-table-l-ast-l-n
#[derive(Clone, Default, Debug)]
pub struct BrTableOperation {
    labels: Vec<Index>,
}
impl std::fmt::Display for BrTableOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let labels = self
            .labels
            .iter()
            .map(|l| l.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, "(br_table {})", labels)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BrTableOperation {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut labels = vec![];
        loop {
            let label = if let Some(label) = get_id(tokens) {
                Index::Id(label)
            } else if tokens.peek().copied().expect_number().is_ok() {
                Index::Index(read_u32(tokens.next().expect_number()?)?)
            } else {
                return if labels.is_empty() {
                    Err(Error::new(
                        tokens.next().cloned(),
                        "Expected there to be more labels for br_table operation",
                    ))
                } else {
                    Ok(Self { labels })
                };
            };
            labels.push(label)
        }
    }
}
impl ValidateInstruction for BrTableOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // TODO(Alec): WHAT DOES IT ALL MEAN? WHAT IS THE MEANING OF LIFE?
        // performs an indirect branch through an operand indexing into the label
        // vector that is an immediate to the instruction, or to a default target
        // if the operand is out of bounds
        todo!("re-evaluate br_table rules");
        // let ty_n = ctx.get_label(&self.label)?;
        // // validate the rest of the labels exist
        // for label in self.labels.iter() {
        //     let ty = ctx.get_label(label)?;
        //     if ty.values().len() != ty_n.values().len() {
        //         return Err(ValidationError::new());
        //     }
        //     for index in ty.values().len()..0 {
        //         let value = inputs
        //             .0
        //             .get(index - inputs.0.len())
        //             .ok_or_else(ValidationError::new)?;
        //         value.try_into_value_type(
        //             ty.values().get(index).ok_or_else(ValidationError::new)?,
        //         )?;
        //     }
        // }
        // // There must be enough input as expected in `ty_n`
        // if ty_n.values().len() > inputs.0.len() {
        //     return Err(ValidationError::new());
        // }
        // let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        // for index in ty_n.values().len()..0 {
        //     let _ = inputs
        //         .pop()?
        //         .try_into_value_type(ty_n.values().get(index).unwrap())?;
        // }

        // Ok(ty_n.values().to_vec())
    }
}
////////////////////////////////////////////////////////////////////////////////

// Come back and review
// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-return
#[derive(Clone, Default, Debug)]
pub struct ReturnOperation;
impl std::fmt::Display for ReturnOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(return)")
    }
}
impl ValidateInstruction for ReturnOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty_set = ctx.returning().ok_or_else(ValidationError::new)?;
        for ty in ty_set.values() {
            let _ = inputs.pop()?.try_into_value_type(&ty)?;
        }
        Ok(ty_set.values().to_vec())
    }
}

#[derive(Clone, Debug)]
pub struct CallOperation {
    index: Index,
}
impl std::fmt::Display for CallOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(call {})", self.index)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for CallOperation {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        let index = if let Some(id) = get_id(tokens) {
            Index::Id(id)
        } else {
            Index::Index(read_u32(tokens.next().expect_number()?)?)
        };
        Ok(Self { index })
    }
}
impl ValidateInstruction for CallOperation {
    // type Output = Vec<ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let func = ctx.get_function(&self.index)?;
        // validate function input arguments
        for ty in func.input().values() {
            let _ = inputs.pop()?.try_into_value_type(&ty)?;
        }
        // if all valid, return output arguments
        Ok(func.output().values().to_vec())
    }
}

pub struct CallIndirectOperation {
    table: TableIndex,
    ty: TypeIndex,
}
impl ValidateInstruction for CallIndirectOperation {
    // type Output = Vec<ValueType>;

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        todo!("Alec fix call indirect operation index call")
        // let table = ctx.get_table(self.table)?;
        // // validate reference type is func ref
        // table.ref_type().try_into_func_ref()?;
        // // type ty must be defined
        // let ty = ctx.get_type(self.ty)?;
        // // validate the function
        // let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
        // for input_ty in ty.input().values() {
        //     let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
        // }
        // Ok(ty.output().values().to_vec())
    }
}

#[derive(Clone, Debug)]
pub struct RefNull {
    ty: HeapType,
}
impl ValidateInstruction for RefNull {
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        Ok(vec![ValueType::RefType(self.ty.into())])
    }
}
impl std::fmt::Display for RefNull {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(ref.null {})", self.ty)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for RefNull {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        Ok(Self {
            ty: HeapType::parse(tokens)?,
        })
    }
}

#[derive(Clone, Debug)]
pub struct RefIsNull;
impl ValidateInstruction for RefIsNull {
    fn validate(&self, _: &mut Context, input: &mut Input) -> ValidateResult<Vec<ValueType>> {
        input.pop()?.try_into_ref_type()?;
        Ok(vec![ValueType::Num(NumType::I32)])
    }
}
impl std::fmt::Display for RefIsNull {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(ref.is_null)")
    }
}
#[derive(Clone, Debug)]
pub struct RefFunc {
    index: Index,
}

impl RefFunc {
    pub fn new(index: Index) -> Self {
        Self { index }
    }
}
impl ValidateInstruction for RefFunc {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        ctx.get_function(&self.index)?;
        todo!("implement get_ref");
        // ctx.get_ref(&self.index)
        // Ok(vec![ValueType::RefType(RefType::FuncRef)])
    }
}
impl std::fmt::Display for RefFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(ref.func {})", self.index)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for RefFunc {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        Ok(Self {
            index: Index::parse(tokens)?,
        })
    }
}
