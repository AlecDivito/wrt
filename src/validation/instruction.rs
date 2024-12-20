// use std::{convert::TryFrom, iter::Peekable};

// use crate::{
//     ast::tree::Node,
//     execution::{Number, Stack, Trap},
//     lex::{
//         ast::{read_number, read_u128, read_u32, read_u8, write_optional, Error, Expect, Parse},
//         Keyword, Token,
//     },
//     structure::{
//         module::{get_id, get_next_keyword},
//         types::{
//             AllVectorShape,
//             BlockType,
//             Direction::{Left, Right},
//             FloatType,
//             FloatVectorShape,
//             FuncResult,
//             FunctionIndex,
//             HalfType,
//             InBracketInstruction,
//             Index,
//             Instruction,
//             IntType,
//             IntegerVectorShape,
//             MemoryArgument,
//             MemoryIndex,
//             MemoryLoadNumber,
//             MemoryWidth,
//             MemoryZeroWidth,
//             NumType, // RefType, HeapType, GlobalType,
//             SignType,
//             TypeUse,
//             ValueType,
//             VecType,
//             VectorMemoryOp,
//             VectorShape,
//         },
//     },
// };

// use super::{Context, Input, ValidateInstruction, ValidateResult, ValidationError};

// #[derive(Clone, Debug)]
// pub enum Operation {
//     // Const
//     Const(Const),

//     // Unary
//     Test(TestOperation),
//     Unary(UnaryOperation),

//     // Binary
//     Binary(BinaryOperation),

//     // Compare
//     Compare(CompareOperation),

//     // Convert
//     Convert(ConvertOperation),

//     Nop(NopOperation),
//     GlobalGet(GlobalGetOperation),
//     LocalSet(LocalSetOperation),
//     LocalTee(LocalTeeOperation),
//     GlobalSet(GlobalSetOperation),
//     LocalGet(LocalGetOperation),
//     Unreachable(UnreachableOperation),

//     // Parametric
//     Drop(DropOperation),
//     Select(SelectOperation),

//     // Control
//     Block(BlockOperation),
//     Call(CallOperation),
//     CallIndirect(CallIndirectOperation),
//     Loop(LoopOperation),
//     If(IfOperation),
//     Break(BrOperation),
//     BreakIf(BrIfOperation),
//     BreakTable(BrTableOperation),
//     Return(ReturnOperation),

//     // Reference
//     RefNull(RefNull),
//     RefIsNull(RefIsNull),
//     RefFunc(RefFunc),

//     // Table
//     TableGet(TableGetOperation),
//     TableSet(TableSetOperation),
//     TableSize(TableSizeOperation),
//     TableGrow(TableGrowOperation),
//     TableFill(TableFillOperation),
//     TableCopy(TableCopyOperation),
//     TableInit(TableInitOperation),
//     ElemDrop(ElementDropOperation),

//     // Memory
//     MemoryLoad(LoadMemoryOperation),
//     MemoryLoadN(LoadNMemoryOperation),
//     MemoryStore(StoreMemoryOperation),
//     MemoryStoreN(StoreNMemoryOperation),
//     MemorySize(MemorySize),
//     MemoryGrow(MemoryGrow),
//     MemoryFill(MemoryFill),
//     MemoryCopy(MemoryCopy),
//     MemoryInit(MemoryInit),
//     DataDrop(DataDrop),
//     MemoryLoadVector(LoadVectorMemoryOperation),
//     MemoryStoreVector(StoreVectorMemoryOperation),

//     // Vector
//     VectorConst(ConstV128),
//     VectorUnary(VectorUnaryOperation),
//     VectorBinary(VectorBinaryOperation),
//     VectorTernary(VectorTernaryOperation),
//     VectorTest(VectorTestOperation),
//     VectorCompare(VectorCompareOperation),
//     VectorShift(VectorShapeShiftOperation),
//     VectorSplat(VectorSplatOperation),
//     VectorExtractLane(VectorExtractLaneOperation),
//     VectorReplaceLane(VectorReplaceLaneOperation),
//     VectorShuffle(VectorShuffleOperation),
//     VectorSwizzle(VectorSwizzleOperation),
//     VectorBitMask(VectorShapeBitMaskOperation),
//     VectorNarrow(VectorShapeNarrowOperation),
//     VectorTop(VectorShapeTopOperation),
// }

// impl Default for Operation {
//     fn default() -> Self {
//         Operation::Nop(NopOperation {})
//     }
// }

// impl ValidateInstruction for Operation {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         match self {
//             Operation::Const(v) => v.validate(ctx, inputs),
//             Operation::Unary(v) => v.validate(ctx, inputs),
//             Operation::Binary(v) => v.validate(ctx, inputs),
//             Operation::Test(v) => v.validate(ctx, inputs),
//             Operation::Compare(v) => v.validate(ctx, inputs),
//             Operation::Convert(v) => v.validate(ctx, inputs),
//             Operation::Nop(v) => v.validate(ctx, inputs),
//             Operation::GlobalGet(v) => v.validate(ctx, inputs),
//             Operation::LocalGet(v) => v.validate(ctx, inputs),
//             Operation::LocalSet(v) => v.validate(ctx, inputs),
//             Operation::LocalTee(v) => v.validate(ctx, inputs),
//             Operation::GlobalSet(v) => v.validate(ctx, inputs),
//             Operation::Unreachable(v) => v.validate(ctx, inputs),
//             Operation::Drop(v) => v.validate(ctx, inputs),
//             Operation::Select(v) => v.validate(ctx, inputs),
//             Operation::Block(v) => v.validate(ctx, inputs),
//             Operation::Call(v) => v.validate(ctx, inputs),
//             Operation::CallIndirect(v) => v.validate(ctx, inputs),
//             Operation::Loop(v) => v.validate(ctx, inputs),
//             Operation::If(v) => v.validate(ctx, inputs),
//             Operation::Break(v) => v.validate(ctx, inputs),
//             Operation::BreakIf(v) => v.validate(ctx, inputs),
//             Operation::BreakTable(v) => v.validate(ctx, inputs),
//             Operation::Return(v) => v.validate(ctx, inputs),
//             Operation::RefNull(v) => v.validate(ctx, inputs),
//             Operation::RefFunc(v) => v.validate(ctx, inputs),
//             Operation::RefIsNull(v) => v.validate(ctx, inputs),
//             // Table
//             Operation::TableGet(v) => v.validate(ctx, inputs),
//             Operation::TableSet(v) => v.validate(ctx, inputs),
//             Operation::TableSize(v) => v.validate(ctx, inputs),
//             Operation::TableGrow(v) => v.validate(ctx, inputs),
//             Operation::TableFill(v) => v.validate(ctx, inputs),
//             Operation::TableCopy(v) => v.validate(ctx, inputs),
//             Operation::TableInit(v) => v.validate(ctx, inputs),
//             Operation::ElemDrop(v) => v.validate(ctx, inputs),
//             // Memory
//             Operation::MemoryLoad(v) => v.validate(ctx, inputs),
//             Operation::MemoryLoadN(v) => v.validate(ctx, inputs),
//             Operation::MemoryStore(v) => v.validate(ctx, inputs),
//             Operation::MemoryStoreN(v) => v.validate(ctx, inputs),
//             Operation::MemorySize(v) => v.validate(ctx, inputs),
//             Operation::MemoryGrow(v) => v.validate(ctx, inputs),
//             Operation::MemoryFill(v) => v.validate(ctx, inputs),
//             Operation::MemoryCopy(v) => v.validate(ctx, inputs),
//             Operation::MemoryInit(v) => v.validate(ctx, inputs),
//             Operation::DataDrop(v) => v.validate(ctx, inputs),
//             Operation::MemoryLoadVector(v) => v.validate(ctx, inputs),
//             Operation::MemoryStoreVector(v) => v.validate(ctx, inputs),
//             // Vector
//             Operation::VectorConst(v) => v.validate(ctx, inputs),
//             Operation::VectorUnary(v) => v.validate(ctx, inputs),
//             Operation::VectorBinary(v) => v.validate(ctx, inputs),
//             Operation::VectorTernary(v) => v.validate(ctx, inputs),
//             Operation::VectorTest(v) => v.validate(ctx, inputs),
//             Operation::VectorShift(v) => v.validate(ctx, inputs),
//             Operation::VectorSplat(v) => v.validate(ctx, inputs),
//             Operation::VectorExtractLane(v) => v.validate(ctx, inputs),
//             Operation::VectorReplaceLane(v) => v.validate(ctx, inputs),
//             Operation::VectorShuffle(v) => v.validate(ctx, inputs),

//             Operation::VectorCompare(v) => v.validate(ctx, inputs),
//             Operation::VectorSwizzle(v) => v.validate(ctx, inputs),
//             Operation::VectorBitMask(v) => v.validate(ctx, inputs),
//             Operation::VectorNarrow(v) => v.validate(ctx, inputs),
//             Operation::VectorTop(v) => v.validate(ctx, inputs),
//         }
//     }
// }

// impl std::fmt::Display for Operation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Operation::Const(cst) => write!(f, "{}", cst),
//             Operation::Nop(opt) => write!(f, "{}", opt),
//             Operation::GlobalGet(opt) => write!(f, "{}", opt),
//             Operation::LocalGet(opt) => write!(f, "{}", opt),
//             Operation::GlobalSet(opt) => write!(f, "{}", opt),
//             Operation::LocalSet(opt) => write!(f, "{}", opt),
//             Operation::LocalTee(opt) => write!(f, "{}", opt),
//             Operation::Unreachable(opt) => write!(f, "{}", opt),
//             Operation::Drop(opt) => write!(f, "{}", opt),
//             Operation::Select(opt) => write!(f, "{}", opt),
//             Operation::Block(opt) => write!(f, "{}", opt),
//             Operation::Call(opt) => write!(f, "{}", opt),
//             Operation::CallIndirect(opt) => write!(f, "{}", opt),
//             Operation::Loop(opt) => write!(f, "{}", opt),
//             Operation::If(opt) => write!(f, "{}", opt),
//             Operation::Break(opt) => write!(f, "{}", opt),
//             Operation::BreakIf(opt) => write!(f, "{}", opt),
//             Operation::BreakTable(opt) => write!(f, "{}", opt),
//             Operation::Return(opt) => write!(f, "{}", opt),
//             Operation::Test(opt) => write!(f, "{}", opt),
//             Operation::Unary(opt) => write!(f, "{}", opt),
//             Operation::Binary(opt) => write!(f, "{}", opt),
//             Operation::Compare(opt) => write!(f, "{}", opt),
//             Operation::Convert(opt) => write!(f, "{}", opt),
//             Operation::RefNull(opt) => write!(f, "{}", opt),
//             Operation::RefFunc(opt) => write!(f, "{}", opt),
//             Operation::RefIsNull(opt) => write!(f, "{}", opt),
//             // Table
//             Operation::TableGet(opt) => write!(f, "{}", opt),
//             Operation::TableSet(opt) => write!(f, "{}", opt),
//             Operation::TableSize(opt) => write!(f, "{}", opt),
//             Operation::TableGrow(opt) => write!(f, "{}", opt),
//             Operation::TableFill(opt) => write!(f, "{}", opt),
//             Operation::TableCopy(opt) => write!(f, "{}", opt),
//             Operation::TableInit(opt) => write!(f, "{}", opt),
//             Operation::ElemDrop(opt) => write!(f, "{}", opt),
//             // Memory
//             Operation::MemoryLoad(opt) => write!(f, "{}", opt),
//             Operation::MemoryLoadN(opt) => write!(f, "{}", opt),
//             Operation::MemoryStore(opt) => write!(f, "{}", opt),
//             Operation::MemoryStoreN(opt) => write!(f, "{}", opt),
//             Operation::MemorySize(opt) => write!(f, "{}", opt),
//             Operation::MemoryGrow(opt) => write!(f, "{}", opt),
//             Operation::MemoryFill(opt) => write!(f, "{}", opt),
//             Operation::MemoryCopy(opt) => write!(f, "{}", opt),
//             Operation::MemoryInit(opt) => write!(f, "{}", opt),
//             Operation::DataDrop(opt) => write!(f, "{}", opt),
//             Operation::MemoryLoadVector(opt) => write!(f, "{}", opt),
//             Operation::MemoryStoreVector(opt) => write!(f, "{}", opt),
//             // Vector
//             Operation::VectorConst(opt) => write!(f, "{}", opt),
//             Operation::VectorUnary(opt) => write!(f, "{}", opt),
//             Operation::VectorBinary(opt) => write!(f, "{}", opt),
//             Operation::VectorTernary(opt) => write!(f, "{}", opt),
//             Operation::VectorTest(opt) => write!(f, "{}", opt),
//             Operation::VectorShift(opt) => write!(f, "{}", opt),
//             Operation::VectorSplat(opt) => write!(f, "{}", opt),
//             Operation::VectorExtractLane(opt) => write!(f, "{}", opt),
//             Operation::VectorReplaceLane(opt) => write!(f, "{}", opt),
//             Operation::VectorShuffle(opt) => write!(f, "{}", opt),
//             Operation::VectorCompare(opt) => write!(f, "{}", opt),
//             Operation::VectorSwizzle(opt) => write!(f, "{}", opt),
//             Operation::VectorBitMask(opt) => write!(f, "{}", opt),
//             Operation::VectorNarrow(opt) => write!(f, "{}", opt),
//             Operation::VectorTop(opt) => write!(f, "{}", opt),
//         }
//     }
// }

// // impl TryFrom<&mut Node> for Operation {
// //     type Error = Error;

// //     fn try_from(value: &mut Node) -> Result<Self, Self::Error> {
// //         match
// //     }
// // }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Operation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         let op = match tokens.next().expect_keyword()? {
//             // Keyword::FuncRef => todo!(),
//             // Keyword::ExternRef => todo!(),
//             Keyword::VecShape(_) => todo!(),
//             // Keyword::Extern => todo!(),
//             // Keyword::Mut => todo!(),
//             Keyword::Nop => Operation::Nop(NopOperation {}),
//             Keyword::Unreachable => Operation::Unreachable(UnreachableOperation {}),
//             Keyword::Drop => Operation::Drop(DropOperation {}),
//             Keyword::Block => Operation::Block(BlockOperation::parse(tokens)?),
//             Keyword::Loop => Operation::Loop(LoopOperation::parse(tokens)?),
//             // Keyword::End => todo!(),
//             Keyword::Br => Operation::Break(BrOperation::parse(tokens)?),
//             Keyword::BrIf => Operation::BreakIf(BrIfOperation::parse(tokens)?),
//             Keyword::BrTable => Operation::BreakTable(BrTableOperation::parse(tokens)?),
//             Keyword::Return => Operation::Return(ReturnOperation {}),
//             Keyword::If => Operation::If(IfOperation::parse(tokens)?),
//             Keyword::Select => Operation::Select(SelectOperation::parse(tokens)?),
//             Keyword::Call => Operation::Call(CallOperation::parse(tokens)?),
//             Keyword::CallIndirect => Operation::CallIndirect(CallIndirectOperation::parse(tokens)?),
//             // Keyword::LocalGet => Operation::LocalGet(LocalGetOperation::parse(tokens)?),
//             // Keyword::LocalSet => Operation::LocalSet(LocalSetOperation::parse(tokens)?),
//             // Keyword::LocalTee => Operation::LocalTee(LocalTeeOperation::parse(tokens)?),
//             // Keyword::GlobalGet => Operation::GlobalGet(GlobalGetOperation::parse(tokens)?),
//             // Keyword::GlobalSet => Operation::GlobalSet(GlobalSetOperation::parse(tokens)?),
//             // Table
//             // Keyword::TableGet => Operation::TableGet(TableGetOperation::parse(tokens)?),
//             // Keyword::TableSet => Operation::TableSet(TableSetOperation::parse(tokens)?),
//             // Keyword::TableSize => Operation::TableSize(TableSizeOperation::parse(tokens)?),
//             // Keyword::TableGrow => Operation::TableGrow(TableGrowOperation::parse(tokens)?),
//             // Keyword::TableFill => Operation::TableFill(TableFillOperation::parse(tokens)?),
//             // Keyword::TableCopy => Operation::TableCopy(TableCopyOperation::parse(tokens)?),
//             // Keyword::TableInit => Operation::TableInit(TableInitOperation::parse(tokens)?),
//             // Keyword::ElemDrop => Operation::ElemDrop(ElementDropOperation::parse(tokens)?),
//             // Memory
//             // Keyword::MemorySize => Operation::MemorySize(MemorySize::new()),
//             // Keyword::MemoryGrow => Operation::MemoryGrow(MemoryGrow::new()),
//             // Keyword::MemoryFill => Operation::MemoryFill(MemoryFill::new()),
//             // Keyword::MemoryCopy => Operation::MemoryCopy(MemoryCopy::new()),
//             // Keyword::MemoryInit => Operation::MemoryInit(MemoryInit::parse(tokens)?),
//             // Keyword::DataDrop => Operation::DataDrop(DataDrop::parse(tokens)?),
//             // Keyword::Load(ty) => {
//             //     Operation::MemoryLoad(LoadMemoryOperation::new(ty, MemoryArgument::parse(tokens)?))
//             // }
//             // Keyword::Store(ty) => Operation::MemoryStore(StoreMemoryOperation::new(
//             //     ty,
//             //     MemoryArgument::parse(tokens)?,
//             // )),
//             // Keyword::LoadN(ty, sign, num) => Operation::MemoryLoadN(LoadNMemoryOperation::new(
//             //     ty,
//             //     num,
//             //     sign,
//             //     MemoryArgument::parse(tokens)?,
//             // )),
//             // Keyword::MemArgsAlign(_) => todo!(),
//             // Keyword::MemArgsOffset(_) => todo!(),
//             // Keyword::Declare => todo!(),
//             // Keyword::Offset => todo!(),
//             // Keyword::Item => todo!(),

//             // number stuff
//             Keyword::Const(expected) => {
//                 let value = read_number(expected, tokens.next().expect_number()?)?;
//                 Operation::Const(Const::new(expected, value))
//             }
//             Keyword::RefNull => Operation::RefNull(RefNull::parse(tokens)?),
//             Keyword::RefFunc => Operation::RefFunc(RefFunc::parse(tokens)?),
//             Keyword::RefIsNull => Operation::RefIsNull(RefIsNull),
//             // Keyword::RefExtern => todo!(),
//             Keyword::IntClz(ty) => Operation::Unary(UnaryOperation::new(ty, UnaryFunction::Clz)),
//             Keyword::IntCtz(ty) => Operation::Unary(UnaryOperation::new(ty, UnaryFunction::Ctz)),
//             Keyword::IntPopCnt(ty) => {
//                 Operation::Unary(UnaryOperation::new(ty, UnaryFunction::PopCnt))
//             }
//             Keyword::IntExtend8Signed(ty) => Operation::Convert(ConvertOperation::new(
//                 ty,
//                 ConvertFunction::Extend {
//                     by: MemoryWidth::I8,
//                     sign: SignType::Signed,
//                 },
//             )),
//             Keyword::IntExtend16Signed(ty) => Operation::Convert(ConvertOperation::new(
//                 ty,
//                 ConvertFunction::Extend {
//                     by: MemoryWidth::I16,
//                     sign: SignType::Signed,
//                 },
//             )),
//             Keyword::I64Extend32Signed => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::Extend {
//                     by: MemoryWidth::I32,
//                     sign: SignType::Signed,
//                 },
//             )),

//             Keyword::NegativeFloat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Neg))
//             }
//             Keyword::AbsoluteFloat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Abs))
//             }
//             Keyword::SquareRootFloat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Sqrt))
//             }
//             Keyword::CeilFloat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Ceil))
//             }
//             Keyword::FloorFLoat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Floor))
//             }
//             Keyword::NearestFloat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Nearest))
//             }
//             Keyword::TruncateFloat(expected) => {
//                 Operation::Unary(UnaryOperation::new(expected, UnaryFunction::Trunc))
//             }
//             Keyword::AddInt(ty) => Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Add)),
//             Keyword::SubInt(ty) => Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Sub)),
//             Keyword::MultiplyInt(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Mul))
//             }
//             Keyword::AndInt(ty) => Operation::Binary(BinaryOperation::new(ty, BinaryFunction::And)),
//             Keyword::OrInt(ty) => Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Or)),
//             Keyword::XORInt(ty) => Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Xor)),
//             Keyword::ShiftLeftInt(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Shl))
//             }
//             Keyword::DivideInt { shape, sign } => {
//                 Operation::Binary(BinaryOperation::new(shape, BinaryFunction::Div(sign)))
//             }
//             Keyword::RemainderInt { shape, sign } => {
//                 Operation::Binary(BinaryOperation::new(shape, BinaryFunction::Rem(sign)))
//             }
//             Keyword::ShiftRightInt { shape, sign } => {
//                 Operation::Binary(BinaryOperation::new(shape, BinaryFunction::Shr(sign)))
//             }
//             Keyword::RotateInt { shape, direction } => match direction {
//                 Left => Operation::Binary(BinaryOperation::new(shape, BinaryFunction::Rotl)),
//                 Right => Operation::Binary(BinaryOperation::new(shape, BinaryFunction::Rotr)),
//             },
//             Keyword::AddFloat(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Add))
//             }
//             Keyword::SubFloat(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Sub))
//             }
//             Keyword::MultiplyFloat(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Mul))
//             }
//             Keyword::DivideFloat(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::DivideFloat))
//             }
//             Keyword::MinFloat(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Min))
//             }
//             Keyword::MaxFloat(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::Max))
//             }
//             Keyword::CopySign(ty) => {
//                 Operation::Binary(BinaryOperation::new(ty, BinaryFunction::CopySign))
//             }

//             // Testing
//             Keyword::I32EqualTest => Operation::Test(TestOperation::new(IntType::I32)),
//             Keyword::I64EqualTest => Operation::Test(TestOperation::new(IntType::I64)),

//             // Compare
//             Keyword::CompareIntEqual(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::Equal))
//             }
//             Keyword::CompareIntNotEqual(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::NotEqual))
//             }
//             Keyword::CompareIntLessThen { shape, sign } => Operation::Compare(
//                 CompareOperation::new(shape, CompareFunction::LessThen(sign)),
//             ),
//             Keyword::CompareIntLessOrEqual { shape, sign } => Operation::Compare(
//                 CompareOperation::new(shape, CompareFunction::MoreThen(sign)),
//             ),
//             Keyword::CompareIntGreaterThen { shape, sign } => Operation::Compare(
//                 CompareOperation::new(shape, CompareFunction::LessOrEqual(sign)),
//             ),
//             Keyword::CompareIntGreaterOrEqual { shape, sign } => Operation::Compare(
//                 CompareOperation::new(shape, CompareFunction::MoreOrEqual(sign)),
//             ),
//             Keyword::CompareFloatEqual(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::Equal))
//             }
//             Keyword::CompareFloatNotEqual(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::NotEqual))
//             }
//             Keyword::CompareFloatLessThen(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::Less))
//             }
//             Keyword::CompareFloatLessOrEqual(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::Greater))
//             }
//             Keyword::CompareFloatGreaterThen(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::LessEqual))
//             }
//             Keyword::CompareFloatGreaterOrEqual(shape) => {
//                 Operation::Compare(CompareOperation::new(shape, CompareFunction::GreaterEqual))
//             }
//             Keyword::I32WrapI64 => Operation::Convert(ConvertOperation::new(
//                 NumType::I32,
//                 ConvertFunction::Wrap(NumType::I64),
//             )),
//             Keyword::I64ExtendI32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::ExtendI32 { sign },
//             )),
//             Keyword::F32DemoteF64 => Operation::Convert(ConvertOperation::new(
//                 FloatType::F32,
//                 ConvertFunction::Demote(FloatType::F64),
//             )),
//             Keyword::F64PromoteF32 => Operation::Convert(ConvertOperation::new(
//                 FloatType::F64,
//                 ConvertFunction::Promote(FloatType::F32),
//             )),
//             Keyword::I32TruncateF32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I32,
//                 ConvertFunction::Truncate {
//                     by: FloatType::F32,
//                     sign,
//                 },
//             )),
//             Keyword::I64TruncateF32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::Truncate {
//                     by: FloatType::F32,
//                     sign,
//                 },
//             )),
//             Keyword::I32TruncateF64(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I32,
//                 ConvertFunction::Truncate {
//                     by: FloatType::F64,
//                     sign,
//                 },
//             )),
//             Keyword::I64TruncateF64(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::Truncate {
//                     by: FloatType::F64,
//                     sign,
//                 },
//             )),
//             Keyword::I32TruncateSatF32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I32,
//                 ConvertFunction::TruncateSat {
//                     by: FloatType::F32,
//                     sign,
//                 },
//             )),
//             Keyword::I64TruncateSatF32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::TruncateSat {
//                     by: FloatType::F32,
//                     sign,
//                 },
//             )),
//             Keyword::I32TruncateSatF64(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I32,
//                 ConvertFunction::TruncateSat {
//                     by: FloatType::F64,
//                     sign,
//                 },
//             )),
//             Keyword::I64TruncateSatF64(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::TruncateSat {
//                     by: FloatType::F64,
//                     sign,
//                 },
//             )),
//             Keyword::F32ConvertI32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::F32,
//                 ConvertFunction::Convert {
//                     by: IntType::I32,
//                     sign,
//                 },
//             )),
//             Keyword::F32ConvertI64(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::F32,
//                 ConvertFunction::Convert {
//                     by: IntType::I64,
//                     sign,
//                 },
//             )),
//             Keyword::F64ConvertI32(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::F64,
//                 ConvertFunction::Convert {
//                     by: IntType::I32,
//                     sign,
//                 },
//             )),
//             Keyword::F64ConvertI64(sign) => Operation::Convert(ConvertOperation::new(
//                 NumType::F64,
//                 ConvertFunction::Convert {
//                     by: IntType::I64,
//                     sign,
//                 },
//             )),
//             Keyword::F32ReinterpretI32 => Operation::Convert(ConvertOperation::new(
//                 NumType::F32,
//                 ConvertFunction::Reinterpret(NumType::I32),
//             )),
//             Keyword::F64ReinterpretI64 => Operation::Convert(ConvertOperation::new(
//                 NumType::F64,
//                 ConvertFunction::Reinterpret(NumType::I64),
//             )),
//             Keyword::I32ReinterpretF32 => Operation::Convert(ConvertOperation::new(
//                 NumType::I32,
//                 ConvertFunction::Reinterpret(NumType::F32),
//             )),
//             Keyword::I64ReinterpretF64 => Operation::Convert(ConvertOperation::new(
//                 NumType::I64,
//                 ConvertFunction::Reinterpret(NumType::F64),
//             )),
//             Keyword::ConstV128 => Self::VectorConst(ConstV128::parse(tokens)?),
//             Keyword::V128Load => Self::MemoryLoadVector(LoadVectorMemoryOperation::load(
//                 MemoryArgument::parse(tokens)?,
//             )),
//             Keyword::V128Store => Self::MemoryStoreVector(StoreVectorMemoryOperation::load(
//                 MemoryArgument::parse(tokens)?,
//             )),

//             Keyword::VecLoad(shape) => Self::MemoryLoadVector(LoadVectorMemoryOperation::new(
//                 shape,
//                 MemoryArgument::parse(tokens)?,
//             )),
//             Keyword::VecLoadSplat(shape) => Self::MemoryLoadVector(LoadVectorMemoryOperation::new(
//                 VectorMemoryOp::Splat(shape),
//                 MemoryArgument::parse(tokens)?,
//             )),
//             Keyword::VecLoadZero(shape) => Self::MemoryLoadVector(LoadVectorMemoryOperation::new(
//                 VectorMemoryOp::Zero(shape),
//                 MemoryArgument::parse(tokens)?,
//             )),
//             Keyword::VecLoadLane(shape) => Self::MemoryLoadVector(LoadVectorMemoryOperation::lane(
//                 VectorMemoryOp::Lane(shape),
//                 MemoryArgument::parse(tokens)?,
//                 LaneIndex::parse(tokens)?,
//             )),
//             Keyword::VecStoreLane(shape) => {
//                 Self::MemoryStoreVector(StoreVectorMemoryOperation::lane(
//                     VectorMemoryOp::Lane(shape),
//                     MemoryArgument::parse(tokens)?,
//                     LaneIndex::parse(tokens)?,
//                 ))
//             }

//             Keyword::V128Not => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::V128,
//                 VectorUnaryFunction::Not,
//             )),
//             Keyword::V128And => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::V128,
//                 VectorBinaryFunction::And,
//             )),
//             Keyword::V128AndNot => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::V128,
//                 VectorBinaryFunction::AndNot,
//             )),
//             Keyword::V128Or => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::V128,
//                 VectorBinaryFunction::Or,
//             )),
//             Keyword::V128XOr => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::V128,
//                 VectorBinaryFunction::Xor,
//             )),
//             Keyword::V128BitSelect => Self::VectorTernary(VectorTernaryOperation::new(
//                 AllVectorShape::V128,
//                 VectorTernaryFunction::BitSelect,
//             )),
//             Keyword::V128AnyTrue => Self::VectorTest(VectorTestOperation::new(
//                 AllVectorShape::V128,
//                 VectorTestFunction::AnyTrue,
//             )),
//             Keyword::VecIntNegative(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorUnaryFunction::Neg,
//             )),
//             Keyword::VecIntAbsolute(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorUnaryFunction::Abs,
//             )),
//             Keyword::VecI8x16PopCnt => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Int(IntegerVectorShape::I8x16),
//                 VectorUnaryFunction::Popcnt,
//             )),
//             Keyword::VecI8x16AverageUnsigned => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(IntegerVectorShape::I8x16),
//                 VectorBinaryFunction::Avg(SignType::Unsigned),
//             )),
//             Keyword::VecI16x8AverageUnsigned => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(IntegerVectorShape::I16x8),
//                 VectorBinaryFunction::Avg(SignType::Unsigned),
//             )),
//             Keyword::VecFloatNegative(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Neg,
//             )),
//             Keyword::VecFloatAbsolute(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Abs,
//             )),
//             Keyword::VecFloatSquareRoot(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Sqrt,
//             )),
//             Keyword::VecFloatCeil(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Ceil,
//             )),
//             Keyword::VecFloatFloor(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Floor,
//             )),
//             Keyword::VecFloatTruncate(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Trunc,
//             )),
//             Keyword::VecFloatNearest(shape) => Self::VectorUnary(VectorUnaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorUnaryFunction::Nearest,
//             )),
//             Keyword::I32x4TruncSatF32x4(sign) => Self::VectorTop(VectorShapeTopOperation::new(
//                 IntegerVectorShape::I32x4,
//                 HalfType::High,
//                 FloatVectorShape::F32x4,
//                 sign,
//                 VectorTopFunction::TruncSat,
//                 false,
//             )),
//             Keyword::I32x4TruncSatF64x2Zero(sign) => Self::VectorTop(VectorShapeTopOperation::new(
//                 IntegerVectorShape::I32x4,
//                 HalfType::High,
//                 FloatVectorShape::F64x2,
//                 sign,
//                 VectorTopFunction::TruncSat,
//                 true,
//             )),
//             Keyword::F64x2PromoteLowF32x4 => Self::VectorTop(VectorShapeTopOperation::new(
//                 FloatVectorShape::F64x2,
//                 HalfType::Low,
//                 FloatVectorShape::F32x4,
//                 SignType::Signed,
//                 VectorTopFunction::Promote,
//                 false,
//             )),
//             Keyword::F32x4PromoteF64x2Zero => Self::VectorTop(VectorShapeTopOperation::new(
//                 FloatVectorShape::F32x4,
//                 HalfType::Low,
//                 FloatVectorShape::F64x2,
//                 SignType::Signed,
//                 VectorTopFunction::Promote,
//                 true,
//             )),
//             Keyword::F32x4ConvertI32x4(sign) => Self::VectorTop(VectorShapeTopOperation::new(
//                 FloatVectorShape::F32x4,
//                 HalfType::Low,
//                 IntegerVectorShape::I32x4,
//                 sign,
//                 VectorTopFunction::Convert,
//                 false,
//             )),
//             Keyword::F64x2ConvertLowI32x4(sign) => Self::VectorTop(VectorShapeTopOperation::new(
//                 FloatVectorShape::F64x2,
//                 HalfType::Low,
//                 IntegerVectorShape::I32x4,
//                 sign,
//                 VectorTopFunction::Convert,
//                 false,
//             )),
//             Keyword::I16x8ExtendAddPairwiseI8x16(_) => todo!(),
//             Keyword::I32x4ExtaddPairwiseI16x8(_) => todo!(),
//             Keyword::VecIntEqual(shape) => Self::VectorCompare(VectorCompareOperation::new(
//                 VectorCompareFunction::Equal,
//                 VectorShape::Int(shape),
//             )),
//             Keyword::VecIntNotEqual(shape) => Self::VectorCompare(VectorCompareOperation::new(
//                 VectorCompareFunction::NotEqual,
//                 VectorShape::Int(shape),
//             )),
//             Keyword::VecIntLessThen { shape, sign } => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::LessThen(sign),
//                     VectorShape::Int(shape),
//                 ))
//             }
//             Keyword::VecIntLessOrEqual { shape, sign } => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::LessOrEqual(sign),
//                     VectorShape::Int(shape),
//                 ))
//             }
//             Keyword::VecIntGreaterThen { shape, sign } => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::GreaterThen(sign),
//                     VectorShape::Int(shape),
//                 ))
//             }
//             Keyword::VecIntGreaterOrEqual { shape, sign } => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::GreaterOrEqual(sign),
//                     VectorShape::Int(shape),
//                 ))
//             }
//             Keyword::VecEqualFloat(shape) => Self::VectorCompare(VectorCompareOperation::new(
//                 VectorCompareFunction::FEqual,
//                 VectorShape::Float(shape),
//             )),
//             Keyword::VecNotEqualFloat(shape) => Self::VectorCompare(VectorCompareOperation::new(
//                 VectorCompareFunction::FNotEqual,
//                 VectorShape::Float(shape),
//             )),
//             Keyword::VecLessThenFloat(shape) => Self::VectorCompare(VectorCompareOperation::new(
//                 VectorCompareFunction::FLessThen,
//                 VectorShape::Float(shape),
//             )),
//             Keyword::VecLessOrEqualFloat(shape) => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::FLessOrEqual,
//                     VectorShape::Float(shape),
//                 ))
//             }
//             Keyword::VecGreaterThenFloat(shape) => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::FGreaterThen,
//                     VectorShape::Float(shape),
//                 ))
//             }
//             Keyword::VecGreaterOrEqualFloat(shape) => {
//                 Self::VectorCompare(VectorCompareOperation::new(
//                     VectorCompareFunction::FGreaterOrEqual,
//                     VectorShape::Float(shape),
//                 ))
//             }
//             Keyword::VecSwizzleFloatI8x16 => Self::VectorSwizzle(VectorSwizzleOperation {}),
//             Keyword::VecIntAdd(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorBinaryFunction::Add,
//             )),
//             Keyword::VecIntSub(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorBinaryFunction::Sub,
//             )),
//             Keyword::VecIntMultiply(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorBinaryFunction::Mul,
//             )),
//             Keyword::VecAddSatI16x8(_) => todo!(),
//             Keyword::VecSubtractSatI16x8(_) => todo!(),
//             Keyword::VecI32x4DotProductOfI16x8Signed => todo!(),
//             Keyword::VecMinInt { shape, sign } => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorBinaryFunction::OpMax(sign),
//             )),
//             Keyword::VecMaxInt { shape, sign } => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(shape),
//                 VectorBinaryFunction::OpMin(sign),
//             )),
//             Keyword::VecSubFloat(_) => todo!(),
//             Keyword::VecAddSatI8x16(_) => todo!(),
//             Keyword::VecSubtractSatI8x16(_) => todo!(),
//             Keyword::VecAddFloat(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::Add,
//             )),
//             Keyword::VecDivFloat(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::Div,
//             )),
//             Keyword::VecMulFloat(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::Mul,
//             )),
//             Keyword::VecMinFloat(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::Min,
//             )),
//             Keyword::VecMaxFloat(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::Max,
//             )),
//             Keyword::VecPMin(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::PMin,
//             )),
//             Keyword::VecPMax(shape) => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Float(shape),
//                 VectorBinaryFunction::PMax,
//             )),
//             Keyword::I16x8Q15mulrSatS => Self::VectorBinary(VectorBinaryOperation::new(
//                 AllVectorShape::Int(IntegerVectorShape::I16x8),
//                 VectorBinaryFunction::Q15mulr_Sat_S,
//             )),
//             Keyword::I8x16NarrowI16x8(sign) => Self::VectorNarrow(VectorShapeNarrowOperation::new(
//                 IntegerVectorShape::I8x16,
//                 IntegerVectorShape::I16x8,
//                 sign,
//             )),
//             Keyword::I16x8NarrowI32x4(sign) => Self::VectorNarrow(VectorShapeNarrowOperation::new(
//                 IntegerVectorShape::I16x8,
//                 IntegerVectorShape::I32x4,
//                 sign,
//             )),
//             Keyword::Extend {
//                 input,
//                 output,
//                 half,
//                 sign,
//             } => Self::VectorTop(VectorShapeTopOperation::new(
//                 input,
//                 half,
//                 output,
//                 sign,
//                 VectorTopFunction::Extend,
//                 false,
//             )),
//             Keyword::VecTest(test) => todo!(),
//             Keyword::VecBitmask(shape) => {
//                 Self::VectorBitMask(VectorShapeBitMaskOperation::new(shape))
//             }
//             Keyword::VecShiftLeft(shape) => {
//                 Self::VectorShift(VectorShapeShiftOperation::new(shape))
//             }
//             Keyword::VecShiftRight { shape, sign } => todo!(),
//             Keyword::VecShuffle => Self::VectorShuffle(VectorShuffleOperation::parse(tokens)?),
//             Keyword::VecSplat(shape) => Self::VectorSplat(VectorSplatOperation::new(shape)),
//             Keyword::VecExtract { shape, sign } => Self::VectorExtractLane(
//                 VectorExtractLaneOperation::new(shape, sign, LaneIndex::parse(tokens)?),
//             ),
//             Keyword::VecReplace(shape) => Self::VectorReplaceLane(VectorReplaceLaneOperation::new(
//                 shape,
//                 LaneIndex::parse(tokens)?,
//             )),
//             // Keyword::Module => todo!(),
//             // Keyword::Bin => todo!(),
//             // Keyword::Quote => todo!(),
//             // Keyword::Script => todo!(),
//             // Keyword::Register => todo!(),
//             // Keyword::Invoke => todo!(),
//             // Keyword::Get => todo!(),
//             // Keyword::AssertMalformed => todo!(),
//             // Keyword::AssertInvalid => todo!(),
//             // Keyword::AssertUnlinkable => todo!(),
//             // Keyword::AssertReturn => todo!(),
//             // Keyword::AssertTrap => todo!(),
//             // Keyword::AssertExhaustion => todo!(),
//             Keyword::NaNCanonical => todo!(),
//             Keyword::NaNArithmetic(_) => todo!(),
//             // Keyword::Infinit => todo!(),
//             // Keyword::NaN => todo!(),
//             // Keyword::Input => todo!(),
//             // Keyword::Output => todo!(),
//             keyword => {
//                 return Err(Error::new(
//                     tokens.next().cloned(),
//                     format!("{:?} is not an instruction. It can't be used.", keyword),
//                 ))
//             }
//         };
//         Ok(op)
//     }
// }

// pub trait Execute {
//     fn exec(&self, stack: &mut Stack) -> Result<(), Trap>;
// }

// /**
//  * Number Instructions
//  */
// // Validate Const operations
// #[derive(Clone, Debug)]
// pub struct Const {
//     ty: NumType,
//     value: Number,
// }
// impl Default for Const {
//     fn default() -> Self {
//         Self {
//             ty: NumType::I32,
//             value: Number::I32(0),
//         }
//     }
// }
// impl std::fmt::Display for Const {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({}.const {})", self.ty, self.value)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Const {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         // TODO(Alec): In the future select may allow for more data to be selected
//         match tokens.next().expect_keyword()? {
//             Keyword::Const(expected) => Ok(Const::new(
//                 expected,
//                 read_number(expected, tokens.next().expect_number()?)?,
//             )),
//             keyword => Err(Error::new(
//                 tokens.next().cloned(),
//                 format!("Expected const.xx but got keyword {:?}", keyword),
//             )),
//         }
//     }
// }
// impl Const {
//     pub fn new(ty: NumType, value: Number) -> Self {
//         Self { ty, value }
//     }
// }
// impl ValidateInstruction for Const {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![ValueType::Num(self.ty)])
//     }
// }
// impl Execute for Const {
//     fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
//         if self.ty != self.value.ty() {
//             Err(Trap::new())
//         } else {
//             stack.push(self.value);
//             Ok(())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub enum UnaryFunction {
//     // interger
//     Clz,
//     Ctz,
//     PopCnt,
//     // Float
//     Abs,
//     Neg,
//     Sqrt,
//     Ceil,
//     Floor,
//     Trunc,
//     Nearest,
// }
// impl std::fmt::Display for UnaryFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             UnaryFunction::Clz => write!(f, "clz"),
//             UnaryFunction::Ctz => write!(f, "ctz"),
//             UnaryFunction::PopCnt => write!(f, "popcnt"),
//             UnaryFunction::Abs => write!(f, "abs"),
//             UnaryFunction::Neg => write!(f, "neg"),
//             UnaryFunction::Sqrt => write!(f, "sqrt"),
//             UnaryFunction::Ceil => write!(f, "ceil"),
//             UnaryFunction::Floor => write!(f, "floor"),
//             UnaryFunction::Trunc => write!(f, "trunc"),
//             UnaryFunction::Nearest => write!(f, "nearest"),
//         }
//     }
// }

// // Validate Unary Operations. Only available for numbers.
// #[derive(Clone, Debug)]
// pub struct UnaryOperation {
//     ty: NumType,
//     op: UnaryFunction,
// }
// impl UnaryOperation {
//     pub fn new(ty: impl Into<NumType>, op: UnaryFunction) -> Self {
//         Self { ty: ty.into(), op }
//     }
// }
// impl std::fmt::Display for UnaryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({}.{})", self.ty, self.op)
//     }
// }
// impl ValidateInstruction for UnaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, input: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let value = input.pop()?;
//         if value.try_into_num()? == self.ty {
//             Ok(vec![value])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }
// impl Execute for UnaryOperation {
//     fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
//         let _number = stack.pop_and_assert_num(self.ty)?;
//         todo!("Implment Unary Function");
//         Ok(())
//     }
// }

// // Validate Binary Operation. Only avaliable for numbers.
// #[derive(Clone, Debug)]
// pub enum BinaryFunction {
//     // Both
//     Add,
//     Sub,
//     Mul,
//     // Integer
//     Div(SignType),
//     Rem(SignType),
//     And,
//     Or,
//     Xor,
//     Shl,
//     Shr(SignType),
//     Rotl,
//     Rotr,
//     // Float
//     DivideFloat,
//     Min,
//     Max,
//     CopySign,
// }
// impl std::fmt::Display for BinaryFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             BinaryFunction::Add => write!(f, "add"),
//             BinaryFunction::Sub => write!(f, "sub"),
//             BinaryFunction::Mul => write!(f, "mul"),
//             BinaryFunction::Div(sign) => write!(f, "div_{}", sign),
//             BinaryFunction::Rem(sign) => write!(f, "rem_{}", sign),
//             BinaryFunction::And => write!(f, "and"),
//             BinaryFunction::Or => write!(f, "or"),
//             BinaryFunction::Xor => write!(f, "xor"),
//             BinaryFunction::Shl => write!(f, "shl"),
//             BinaryFunction::Shr(sign) => write!(f, "shr_{}", sign),
//             BinaryFunction::Rotl => write!(f, "rotl"),
//             BinaryFunction::Rotr => write!(f, "rotr"),
//             // Float
//             BinaryFunction::DivideFloat => write!(f, "div"),
//             BinaryFunction::Min => write!(f, "min"),
//             BinaryFunction::Max => write!(f, "max"),
//             BinaryFunction::CopySign => write!(f, "copysign"),
//         }
//     }
// }
// impl Default for BinaryFunction {
//     fn default() -> Self {
//         Self::Add
//     }
// }
// #[derive(Clone, Default, Debug)]
// pub struct BinaryOperation {
//     ty: NumType,
//     func: BinaryFunction,
// }
// impl BinaryOperation {
//     pub fn new(ty: impl Into<NumType>, func: BinaryFunction) -> Self {
//         Self {
//             ty: ty.into(),
//             func,
//         }
//     }
// }
// impl std::fmt::Display for BinaryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({})", self.func)
//     }
// }
// impl ValidateInstruction for BinaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let op1 = inputs.pop()?;
//         let op2 = inputs.pop()?;
//         if op1.is_num() && op2.is_num() && op1 == op2 {
//             Ok(vec![op1])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // Validate test operations. Only avalible for numbers
// #[derive(Clone, Debug)]
// pub struct TestOperation {
//     ty: IntType,
// }
// impl TestOperation {
//     pub fn new(ty: IntType) -> Self {
//         Self { ty }
//     }
// }
// impl std::fmt::Display for TestOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({}.eqz)", self.ty)
//     }
// }
// impl ValidateInstruction for TestOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         if inputs.pop()?.is_num() {
//             Ok(vec![ValueType::Num(NumType::I32)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // Validate Binary Operation. Only avaliable for numbers.
// #[derive(Clone, Debug, Default)]
// pub enum CompareFunction {
//     // Both
//     #[default]
//     Equal,
//     NotEqual,
//     // Integer
//     LessThen(SignType),
//     MoreThen(SignType),
//     LessOrEqual(SignType),
//     MoreOrEqual(SignType),
//     // Float
//     Less,
//     Greater,
//     LessEqual,
//     GreaterEqual,
// }
// impl std::fmt::Display for CompareFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             CompareFunction::Equal => write!(f, "eq"),
//             CompareFunction::NotEqual => write!(f, "ne"),
//             CompareFunction::LessThen(sign) => write!(f, "lt_{}", sign),
//             CompareFunction::MoreThen(sign) => write!(f, "gt_{}", sign),
//             CompareFunction::LessOrEqual(sign) => write!(f, "le_{}", sign),
//             CompareFunction::MoreOrEqual(sign) => write!(f, "ge_{}", sign),
//             CompareFunction::Less => write!(f, "lt"),
//             CompareFunction::Greater => write!(f, "gt"),
//             CompareFunction::LessEqual => write!(f, "le"),
//             CompareFunction::GreaterEqual => write!(f, "ge"),
//         }
//     }
// }
// // Validate relop opertion. Only avalible to numbers
// #[derive(Clone, Debug, Default)]
// pub struct CompareOperation {
//     ty: NumType,
//     func: CompareFunction,
// }

// impl CompareOperation {
//     pub fn new(ty: impl Into<NumType>, func: CompareFunction) -> Self {
//         Self {
//             ty: ty.into(),
//             func,
//         }
//     }
// }
// impl std::fmt::Display for CompareOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.ty, self.func)
//     }
// }
// impl ValidateInstruction for CompareOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let op1 = inputs.pop()?;
//         let op2 = inputs.pop()?;
//         if op1.is_num() && op2.is_num() && op1 == op2 {
//             Ok(vec![ValueType::Num(NumType::I32)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub enum ConvertFunction {
//     Extend { by: MemoryWidth, sign: SignType },
//     ExtendI32 { sign: SignType },
//     Truncate { by: FloatType, sign: SignType },
//     TruncateSat { by: FloatType, sign: SignType },
//     Convert { by: IntType, sign: SignType },
//     Reinterpret(NumType),
//     Demote(FloatType),
//     Promote(FloatType),
//     Wrap(NumType),
// }
// impl std::fmt::Display for ConvertFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ConvertFunction::Extend { by, sign } => write!(f, "extend{}_{}", by, sign),
//             ConvertFunction::ExtendI32 { sign } => write!(f, "extend_i32_{}", sign),
//             ConvertFunction::Truncate { by, sign } => write!(f, "trunc_{}_{}", by, sign),
//             ConvertFunction::TruncateSat { by, sign } => write!(f, "trunc_sat_{}_{}", by, sign),
//             ConvertFunction::Convert { by, sign } => write!(f, "convert_{}_{}", by, sign),
//             ConvertFunction::Reinterpret(ty) => write!(f, "reinterpret_{}", ty),
//             ConvertFunction::Demote(ty) => write!(f, "demote_{}", ty),
//             ConvertFunction::Promote(ty) => write!(f, "promote_{}", ty),
//             ConvertFunction::Wrap(ty) => write!(f, "wrap_{}", ty),
//         }
//     }
// }
// // Validate cvt operation. Only avaliable on numbers.
// #[derive(Clone, Debug)]
// pub struct ConvertOperation {
//     // This is the number types that is calling this function. Ex. i32 => i32.warp_i64_s
//     // i32 is the input
//     // i64 is the output
//     // s is for signed
//     ty: NumType,
//     func: ConvertFunction,
// }
// impl std::fmt::Display for ConvertOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.ty, self.func)
//     }
// }
// impl ConvertOperation {
//     pub fn new(ty: impl Into<NumType>, func: ConvertFunction) -> Self {
//         Self {
//             ty: ty.into(),
//             func,
//         }
//     }
// }
// impl ValidateInstruction for ConvertOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("validation for convert instructions")
//         // let op1 = inputs.pop()?;
//         // if ValueType::Num(self.input) == op1 {
//         //     Ok(vec![ValueType::Num(self.output)])
//         // } else {
//         //     Err(ValidationError::new())
//         // }
//     }
// }

// /**
//  * Reference Instructions
//  */
// pub struct NullReferenceOperation(RefType);
// impl ValidateInstruction for NullReferenceOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![ValueType::RefType(self.0.clone())])
//     }
// }

// pub struct IsNullReferenceOperation;
// impl ValidateInstruction for IsNullReferenceOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let op = inputs.pop()?;
//         if op.is_ref_type() {
//             Ok(vec![ValueType::Num(NumType::I32)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// pub struct FunctionReferenceOperation {
//     function_index: FunctionIndex,
// }
// impl ValidateInstruction for FunctionReferenceOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("Validation is another day :(")
//         // let _ = ctx.get_function(&Index::Index(self.function_index))?;
//         // if ctx.contains_reference(self.function_index) {
//         //     // Reference: https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-ref-mathsf-ref-func-x
//         //     // TODO(Alec): We are supposed to be returning a function ref
//         //     // A function ref is basically a pointer to a function
//         //     // At runtime we validate if the function call is given the correct
//         //     // arguments. So i think just returning the index to the function
//         //     // should be ok because it's technically a pointer.
//         //     return Ok(vec![ValueType::RefType(RefType::FuncRef(
//         //         self.function_index as FunctionReference,
//         //     ))]);
//         // } else {
//         //     Err(ValidationError::new())
//         // }
//     }
// }

// /**
//  * Vector Instructions
//  * https://webassembly.github.io/spec/core/valid/instructions.html#vector-instructions
//  */
// #[derive(Clone, Debug)]
// pub struct ConstV128(u128);
// impl std::fmt::Display for ConstV128 {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "v128.const {}", self.0)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ConstV128 {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         Ok(ConstV128(read_u128(tokens.next().expect_number()?)?))
//     }
// }
// impl ValidateInstruction for ConstV128 {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// #[derive(Clone, Debug)]
// enum VectorUnaryFunction {
//     // both (vv)
//     Not,
//     Popcnt, // only i8x16
//     // int (vi) and (vf)
//     Abs,
//     Neg,
//     // vf
//     Sqrt,
//     Ceil,
//     Floor,
//     Trunc,
//     Nearest,
// }
// impl std::fmt::Display for VectorUnaryFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Not => write!(f, "not"),
//             Self::Abs => write!(f, "abs"),
//             Self::Neg => write!(f, "beg"),
//             Self::Sqrt => write!(f, "sqrt"),
//             Self::Ceil => write!(f, "ceil"),
//             Self::Floor => write!(f, "floor"),
//             Self::Trunc => write!(f, "trunc"),
//             Self::Nearest => write!(f, "nearest"),
//             Self::Popcnt => write!(f, "popcnt"),
//         }
//     }
// }
// #[derive(Clone, Debug)]
// pub struct VectorUnaryOperation {
//     shape: AllVectorShape,
//     func: VectorUnaryFunction,
// }

// impl VectorUnaryOperation {
//     pub fn new(shape: AllVectorShape, func: VectorUnaryFunction) -> Self {
//         Self { shape, func }
//     }
// }
// impl std::fmt::Display for VectorUnaryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.shape, self.func,)
//     }
// }
// impl ValidateInstruction for VectorUnaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![ValueType::VecType(inputs.pop()?.try_into_vec_type()?)])
//     }
// }

// #[derive(Clone, Debug)]
// pub enum VectorBinaryFunction {
//     // (vv) int
//     And,
//     AndNot,
//     Or,
//     Xor,
//     // (vi) (and float)
//     Add,
//     Sub,
//     // FLoat
//     Mul,
//     Div,
//     Min,
//     Max,
//     PMin,
//     PMax,
//     // other
//     Q15mulr_Sat_S, // i16x8
//     OpMax(SignType),
//     OpMin(SignType),
//     Avg(SignType),
// }
// impl std::fmt::Display for VectorBinaryFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::And => write!(f, "and"),
//             Self::AndNot => write!(f, "andnot"),
//             Self::Or => write!(f, "or"),
//             Self::Xor => write!(f, "xor"),
//             Self::Add => write!(f, "add"),
//             Self::Sub => write!(f, "sub"),
//             Self::Mul => write!(f, "mul"),
//             Self::Div => write!(f, "div"),
//             Self::Min => write!(f, "min"),
//             Self::Max => write!(f, "max"),
//             Self::PMin => write!(f, "pmin"),
//             Self::PMax => write!(f, "pmax"),
//             Self::Q15mulr_Sat_S => write!(f, "q15mulr_sat_s"),
//             Self::OpMax(sign) => write!(f, "max_{}", sign),
//             Self::OpMin(sign) => write!(f, "min_{}", sign),
//             Self::Avg(sign) => write!(f, "avgr_{}", sign),
//         }
//     }
// }
// #[derive(Clone, Debug)]
// pub struct VectorBinaryOperation {
//     shape: AllVectorShape,
//     func: VectorBinaryFunction,
// }
// impl VectorBinaryOperation {
//     pub fn new(shape: AllVectorShape, func: VectorBinaryFunction) -> Self {
//         Self { shape, func }
//     }
// }
// impl std::fmt::Display for VectorBinaryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.shape, self.func)
//     }
// }
// impl ValidateInstruction for VectorBinaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// #[derive(Clone, Debug)]
// pub enum VectorTernaryFunction {
//     // (vv)
//     BitSelect, // (vi) (and float)
// }
// impl std::fmt::Display for VectorTernaryFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::BitSelect => write!(f, "bitselect"),
//         }
//     }
// }
// #[derive(Clone, Debug)]
// pub struct VectorTernaryOperation {
//     shape: AllVectorShape,
//     func: VectorTernaryFunction,
// }

// impl VectorTernaryOperation {
//     pub fn new(shape: AllVectorShape, func: VectorTernaryFunction) -> Self {
//         Self { shape, func }
//     }
// }
// impl std::fmt::Display for VectorTernaryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.shape, self.func)
//     }
// }
// impl ValidateInstruction for VectorTernaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// #[derive(Clone, Debug)]
// pub enum VectorTestFunction {
//     // (vv)
//     AnyTrue, // (vi) (and float)
// }
// impl std::fmt::Display for VectorTestFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::AnyTrue => write!(f, "any_true"),
//         }
//     }
// }
// #[derive(Clone, Debug)]
// pub struct VectorTestOperation {
//     shape: AllVectorShape,
//     func: VectorTestFunction,
// }

// impl VectorTestOperation {
//     pub fn new(shape: AllVectorShape, func: VectorTestFunction) -> Self {
//         Self { shape, func }
//     }
// }

// impl std::fmt::Display for VectorTestOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.shape, self.func)
//     }
// }
// impl ValidateInstruction for VectorTestOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }

// // https://webassembly.github.io/spec/core/valid/instructions.html#mathsf-i8x16-xref-syntax-instructions-syntax-instr-vec-mathsf-swizzle
// // i8x16 swizzle
// #[derive(Clone, Debug)]
// pub struct VectorSwizzleOperation;
// impl std::fmt::Display for VectorSwizzleOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "i8x16.swizzle")
//     }
// }
// impl ValidateInstruction for VectorSwizzleOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct LaneIndex(u8);
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LaneIndex {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(LaneIndex(read_u8(tokens.next().expect_number()?)?))
//     }
// }
// impl std::fmt::Display for LaneIndex {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }
// impl LaneIndex {
//     pub fn validate(&self) -> bool {
//         self.0 < 32
//     }

//     pub fn validate_against_shape(&self, shape: VectorShape) -> bool {
//         match shape {
//             VectorShape::Int(IntegerVectorShape::I8x16) => self.0 < 16,
//             VectorShape::Int(IntegerVectorShape::I16x8) => self.0 < 8,
//             VectorShape::Int(IntegerVectorShape::I32x4) => self.0 < 4,
//             VectorShape::Int(IntegerVectorShape::I64x2) => self.0 < 2,
//             VectorShape::Float(FloatVectorShape::F32x4) => self.0 < 4,
//             VectorShape::Float(FloatVectorShape::F64x2) => self.0 < 2,
//         }
//     }

//     pub fn validate_against_memory_width(&self, width: &MemoryWidth) -> bool {
//         // https://webassembly.github.io/spec/core/valid/instructions.html#mathsf-v128-xref-syntax-instructions-syntax-instr-memory-mathsf-load-n-mathsf-lane-xref-syntax-instructions-syntax-memarg-mathit-memarg-xref-syntax-instructions-syntax-laneidx-mathit-laneidx
//         match width {
//             MemoryWidth::I8 => self.0 < 128 / 8,
//             MemoryWidth::I16 => self.0 < 128 / 16,
//             MemoryWidth::I32 => self.0 < 128 / 32,
//             MemoryWidth::I64 => self.0 < 128 / 64,
//         }
//     }
// }

// // https://webassembly.github.io/spec/core/valid/instructions.html#mathsf-i8x16-xref-syntax-instructions-syntax-instr-vec-mathsf-shuffle-xref-syntax-instructions-syntax-laneidx-mathit-laneidx-16
// // i8x16
// #[derive(Clone, Debug)]
// pub struct VectorShuffleOperation {
//     // Index lane can't be larger then the number 32
//     lane_index: LaneIndex,
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for VectorShuffleOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             lane_index: LaneIndex::parse(tokens)?,
//         })
//     }
// }
// impl std::fmt::Display for VectorShuffleOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "i8x16.shuffle")
//     }
// }
// impl ValidateInstruction for VectorShuffleOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         if !self.lane_index.validate() {
//             Err(ValidationError::new())
//         } else {
//             inputs.pop()?.try_into_vec_type()?;
//             inputs.pop()?.try_into_vec_type()?;
//             Ok(vec![ValueType::VecType(VecType)])
//         }
//     }
// }

// // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-instr-vec-mathsf-splat
// // shape.splat
// // shape => i8x16, i16x8, 132x4, i64x2, f32x4, f64x2
// // TODO(Alec): Come back to this
// #[derive(Clone, Debug)]
// pub struct VectorSplatOperation {
//     shape: VectorShape,
// }
// impl VectorSplatOperation {
//     pub fn new(shape: VectorShape) -> Self {
//         Self { shape }
//     }
// }
// impl std::fmt::Display for VectorSplatOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.splat", self.shape)
//     }
// }
// impl ValidateInstruction for VectorSplatOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // unpacked(VectorShape), takes an V128 from the top and "unpacks" it?
//         // Does unpack mean take one value, or all of them? For example, if I have
//         // I32x4, Does that mean i pop 4 inputs off the top and pack them into an
//         // V128? And is that the types i return?
//         todo!("What does 'unpacked' mean?")
//         // inputs.pop()?.try_into_vec_type()?.unpack
//         // Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-instr-vec-mathsf-extract-lane-mathsf-xref-syntax-instructions-syntax-sx-mathit-sx-xref-syntax-instructions-syntax-laneidx-mathit-laneidx
// // shape.extract_lane_sx laneindex
// #[derive(Clone, Debug)]
// pub struct VectorExtractLaneOperation {
//     shape: VectorShape,
//     sign: Option<SignType>,
//     lane_index: LaneIndex,
// }

// impl VectorExtractLaneOperation {
//     pub fn new(shape: VectorShape, sign: Option<SignType>, lane_index: LaneIndex) -> Self {
//         Self {
//             shape,
//             sign,
//             lane_index,
//         }
//     }
// }
// impl std::fmt::Display for VectorExtractLaneOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.sign {
//             Some(sign) => write!(
//                 f,
//                 "{}.extract_lane_{} {}",
//                 self.shape, sign, self.lane_index
//             ),
//             None => write!(f, "{}.extract_lane {}", self.shape, self.lane_index),
//         }
//     }
// }
// impl ValidateInstruction for VectorExtractLaneOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         if self.lane_index.validate_against_shape(self.shape) {
//             Err(ValidationError::new())
//         } else {
//             inputs.pop()?.try_into_vec_type()?;
//             // TODO(Alec): I don't believe we can validate the actual operation,
//             // we'd need to do that at runtime.
//             let ty = match self.shape {
//                 VectorShape::Int(IntegerVectorShape::I8x16) => NumType::I32,
//                 VectorShape::Int(IntegerVectorShape::I16x8) => NumType::I32,
//                 VectorShape::Int(IntegerVectorShape::I32x4) => NumType::I32,
//                 VectorShape::Int(IntegerVectorShape::I64x2) => NumType::I64,
//                 VectorShape::Float(FloatVectorShape::F32x4) => NumType::F32,
//                 VectorShape::Float(FloatVectorShape::F64x2) => NumType::F64,
//             };
//             Ok(vec![ValueType::Num(ty)])
//         }
//     }
// }

// // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-instr-vec-mathsf-replace-lane-xref-syntax-instructions-syntax-laneidx-mathit-laneidx
// // shape.replace_lane lane index
// #[derive(Clone, Debug)]
// pub struct VectorReplaceLaneOperation {
//     shape: VectorShape,
//     lane_index: LaneIndex,
// }
// impl VectorReplaceLaneOperation {
//     pub fn new(shape: VectorShape, lane_index: LaneIndex) -> Self {
//         Self { shape, lane_index }
//     }
// }
// impl std::fmt::Display for VectorReplaceLaneOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.replace_lane {}", self.shape, self.lane_index)
//     }
// }
// impl ValidateInstruction for VectorReplaceLaneOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _shape = inputs.pop()?.try_into_num()?; // Get the value type from the first operand
//         inputs.pop()?.try_into_vec_type()?; // Get the vector
//         if self.lane_index.validate_against_shape(self.shape) {
//             // } && shape != self.shape {
//             Err(ValidationError::new())
//         } else {
//             Ok(vec![ValueType::VecType(VecType)])
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub enum VectorCompareFunction {
//     // int
//     Equal,
//     NotEqual,
//     LessThen(SignType),
//     GreaterThen(SignType),
//     LessOrEqual(SignType),
//     GreaterOrEqual(SignType),
//     // float
//     FEqual,
//     FNotEqual,
//     FLessThen,
//     FGreaterThen,
//     FLessOrEqual,
//     FGreaterOrEqual,
// }
// impl std::fmt::Display for VectorCompareFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Equal => write!(f, "eq"),
//             Self::NotEqual => write!(f, "ne"),
//             Self::LessThen(sign_type) => write!(f, "lt_{}", sign_type),
//             Self::GreaterThen(sign_type) => write!(f, "gt_{}", sign_type),
//             Self::LessOrEqual(sign_type) => write!(f, "le_{}", sign_type),
//             Self::GreaterOrEqual(sign_type) => write!(f, "ge_{}", sign_type),
//             // float
//             Self::FEqual => write!(f, "eq"),
//             Self::FNotEqual => write!(f, "ne"),
//             Self::FLessThen => write!(f, "lt",),
//             Self::FGreaterThen => write!(f, "gt",),
//             Self::FLessOrEqual => write!(f, "le",),
//             Self::FGreaterOrEqual => write!(f, "ge",),
//         }
//     }
// }
// #[derive(Clone, Debug)]
// pub struct VectorCompareOperation {
//     func: VectorCompareFunction,
//     shape: VectorShape,
// }

// impl VectorCompareOperation {
//     pub fn new(func: VectorCompareFunction, shape: VectorShape) -> Self {
//         Self { func, shape }
//     }
// }
// impl std::fmt::Display for VectorCompareOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.{}", self.shape, self.func)
//     }
// }
// impl ValidateInstruction for VectorCompareOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("Implement validation for vector compare")
//     }
// }

// // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-vunop-mathit-vunop
// #[derive(Clone, Debug)]
// pub struct VectorShapeUnaryOperation {
//     _shape: VectorShape,
// }
// impl ValidateInstruction for VectorShapeUnaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // TODO(Alec): DO i need to parse the actual value to validate that it
//         // matches with the shape we have?
//         Ok(vec![ValueType::VecType(inputs.pop()?.try_into_vec_type()?)])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct VectorShapeBinaryOperation {
//     _shape: VectorShape,
// }
// impl ValidateInstruction for VectorShapeBinaryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // TODO(Alec): DO i need to parse the actual value to validate that it
//         // matches with the shape we have?
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // shape.vrelop
// #[derive(Clone, Debug)]
// pub struct VectorShapeRelationOperation {
//     _shape: VectorShape,
// }
// impl ValidateInstruction for VectorShapeRelationOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // TODO(Alec): DO i need to parse the actual value to validate that it
//         // matches with the shape we have?
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // ishape.vishiftop
// #[derive(Clone, Debug)]
// pub struct VectorShapeShiftOperation {
//     shape: IntegerVectorShape,
// }
// impl VectorShapeShiftOperation {
//     pub fn new(shape: IntegerVectorShape) -> Self {
//         Self { shape }
//     }
// }
// impl std::fmt::Display for VectorShapeShiftOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.vishiftop", self.shape)
//     }
// }
// impl ValidateInstruction for VectorShapeShiftOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // TODO(Alec): DO i need to parse the actual value to validate that it
//         // matches with the shape we have?

//         // Pop the first argument as a I32
//         inputs.pop()?.try_into_num()?.try_into_i32()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // shape.vtestop
// // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-shape-mathit-shape-mathsf-xref-syntax-instructions-syntax-vtestop-mathit-vtestop
// #[derive(Clone, Debug)]
// pub struct VectorShapeTestOperation {
//     _shape: VectorShape,
// }
// impl ValidateInstruction for VectorShapeTestOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // TODO(Alec): DO i need to parse the actual value to validate that it
//         // matches with the shape we have?
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }

// #[derive(Clone, Debug)]
// pub enum VectorTopFunction {
//     Extend,
//     TruncSat,
//     Convert,
//     Demote,
//     Promote,
// }
// impl std::fmt::Display for VectorTopFunction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             VectorTopFunction::Extend => write!(f, "extend"),
//             VectorTopFunction::TruncSat => write!(f, "trunc_sat"),
//             VectorTopFunction::Convert => write!(f, "convert"),
//             VectorTopFunction::Demote => write!(f, "demote"),
//             VectorTopFunction::Promote => write!(f, "promote"),
//         }
//     }
// }
// // (input).vcvtop_(half)_(output)_(signed)_(is_zero)
// #[derive(Clone, Debug)]
// pub struct VectorShapeTopOperation {
//     input: VectorShape,
//     half: HalfType,
//     output: VectorShape,
//     sign: SignType,
//     func: VectorTopFunction,
//     is_zero: bool,
// }
// impl VectorShapeTopOperation {
//     pub fn new(
//         input: impl Into<VectorShape>,
//         half: HalfType,
//         output: impl Into<VectorShape>,
//         sign: SignType,
//         func: VectorTopFunction,
//         is_zero: bool,
//     ) -> Self {
//         Self {
//             input: input.into(),
//             half,
//             output: output.into(),
//             sign,
//             func,
//             is_zero,
//         }
//     }
// }
// impl std::fmt::Display for VectorShapeTopOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.is_zero {
//             true => write!(
//                 f,
//                 "{}.{}_{}_{}_{}_zero",
//                 self.input, self.func, self.half, self.output, self.sign
//             ),
//             false => write!(
//                 f,
//                 "{}.{}_{}_{}_{}",
//                 self.input, self.func, self.half, self.output, self.sign
//             ),
//         }
//     }
// }
// impl ValidateInstruction for VectorShapeTopOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![ValueType::VecType(inputs.pop()?.try_into_vec_type()?)])
//     }
// }

// // (input).narrow_(output)_(sx)
// // First ishape is _op_shape
// #[derive(Clone, Debug)]
// pub struct VectorShapeNarrowOperation {
//     input: IntegerVectorShape,
//     output: IntegerVectorShape,
//     sign: SignType,
// }

// impl VectorShapeNarrowOperation {
//     pub fn new(input: IntegerVectorShape, output: IntegerVectorShape, sign: SignType) -> Self {
//         Self {
//             input,
//             output,
//             sign,
//         }
//     }
// }
// impl std::fmt::Display for VectorShapeNarrowOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.narrow_{}_{}", self.input, self.output, self.sign)
//     }
// }
// impl ValidateInstruction for VectorShapeNarrowOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // (ishape).bitmask
// #[derive(Clone, Debug)]
// pub struct VectorShapeBitMaskOperation {
//     shape: IntegerVectorShape,
// }

// impl VectorShapeBitMaskOperation {
//     pub fn new(shape: IntegerVectorShape) -> Self {
//         Self { shape }
//     }
// }
// impl std::fmt::Display for VectorShapeBitMaskOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}.bitmask", self.shape)
//     }
// }
// impl ValidateInstruction for VectorShapeBitMaskOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }

// // (ishape).dot_(ishape)_s
// #[derive(Clone, Debug)]
// pub struct VectorShapeDotProductOperation {
//     _op_shape: IntegerVectorShape,
//     _desired_shape: IntegerVectorShape,
// }
// impl ValidateInstruction for VectorShapeDotProductOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // (ishape).extmul_(half)_(ishape)_(sx)
// #[derive(Clone, Debug)]
// pub struct VectorShapeExtendMultiplyOperation {
//     _op_shape: IntegerVectorShape,
//     _desired_shape: IntegerVectorShape,
//     _desired_half: HalfType,
//     _desired_sign: SignType,
// }
// impl ValidateInstruction for VectorShapeExtendMultiplyOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// // (ishape).extadd_pairwise_(ishape)_(sx)
// #[derive(Clone, Debug)]
// pub struct VectorShapeExtendAddPairWiseOperation {
//     _op_shape: IntegerVectorShape,
//     _desired_shape: IntegerVectorShape,
//     _desired_sign: SignType,
// }
// impl ValidateInstruction for VectorShapeExtendAddPairWiseOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         inputs.pop()?.try_into_vec_type()?;
//         inputs.pop()?.try_into_vec_type()?;
//         Ok(vec![ValueType::VecType(VecType)])
//     }
// }

// /**
//  * Parametric Instructions
//  */
// #[derive(Clone, Debug)]
// pub struct DropOperation {}
// impl std::fmt::Display for DropOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(drop)")
//     }
// }
// impl ValidateInstruction for DropOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // Drop is a `value-polymorphic` instruction.
//         // NOTE: I believe that means that **ANY NUMBER** can be popped off the stack
//         // It doesn't not mean you can drop a vector...but that may not be correct.
//         // It could be that inputs.pop()?, is all that is needed here.
//         inputs.pop()?.try_into_num()?;
//         Ok(vec![])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct SelectOperation {
//     t: Option<ValueType>,

//     // Doesn't necessary take arguments, could take operands
//     op1: Option<Box<Operation>>,
//     op2: Option<Box<Operation>>,
//     op3: Option<Box<Operation>>,
// }
// impl std::fmt::Display for SelectOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(select")?;
//         write_optional(f, " ", self.op1.as_ref())?;
//         write_optional(f, " ", self.op2.as_ref())?;
//         write_optional(f, " ", self.op3.as_ref())?;
//         write!(f, ")")
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for SelectOperation {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         // TODO(Alec): In the future select may allow for more data to be selected
//         let t = if Some(Keyword::Result) == get_next_keyword(tokens) {
//             let mut result = FuncResult::parse(tokens)?.0;
//             let ty = result.pop().map(|ty| *ty.ty());
//             if !result.is_empty() {
//                 return Err(Error::new(
//                     tokens.next().cloned(),
//                     "Select result block has too many arguments".to_string(),
//                 ));
//             }
//             ty
//         } else {
//             None
//         };

//         let mut op1 = None;
//         let mut op2 = None;
//         let mut op3 = None;
//         if Operation::parse(&mut tokens.clone()).is_ok() {
//             op1 = Some(Box::new(Operation::parse(tokens)?));
//         }
//         if Operation::parse(&mut tokens.clone()).is_ok() {
//             op2 = Some(Box::new(Operation::parse(tokens)?));
//         }
//         if Operation::parse(&mut tokens.clone()).is_ok() {
//             op3 = Some(Box::new(Operation::parse(tokens)?));
//         }

//         Ok(Self { t, op1, op2, op3 })
//     }
// }
// impl ValidateInstruction for SelectOperation {
//     // type Output = [ValueType; 1];

//     fn validate(&self, _: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // First we always pop an I32
//         inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let ty1 = inputs.pop()?;
//         let ty2 = inputs.pop()?;
//         match self.t {
//             Some(ty) => {
//                 if ty1 == ty2 && ty == ty1 && ty == ty2 {
//                     Ok(vec![ty1])
//                 } else {
//                     Err(ValidationError::new())
//                 }
//             }
//             None => {
//                 if ty1 == ty2 && (ty1.is_num() || ty1.is_vec_type()) {
//                     Ok(vec![ty1])
//                 } else {
//                     Err(ValidationError::new())
//                 }
//             }
//         }
//     }
// }

// /**
//  * Variable Instructions
//  */
// #[derive(Clone, Debug)]
// pub struct LocalGetOperation {
//     index: Index,
// }
// impl std::fmt::Display for LocalGetOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(local.get {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LocalGetOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for LocalGetOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let a = *ctx.get_local(&self.index)?;
//         Ok(vec![a])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct LocalSetOperation {
//     index: Index,
// }
// impl std::fmt::Display for LocalSetOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(local.set {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LocalSetOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for LocalSetOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty = inputs.pop()?;
//         ctx.set_local(self.index.clone(), ty)?;
//         Ok(vec![])
//     }
// }

// #[derive(Debug, Clone)]
// pub struct LocalTeeOperation {
//     index: Index,
// }
// impl std::fmt::Display for LocalTeeOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(local.tee {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LocalTeeOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for LocalTeeOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty = inputs.pop()?;
//         ctx.set_local(self.index.clone(), ty.clone())?;
//         Ok(vec![ty])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct GlobalGetOperation {
//     index: Index,
// }
// impl std::fmt::Display for GlobalGetOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(global.get {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for GlobalGetOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for GlobalGetOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let global = ctx.get_global(&self.index)?;
//         match global {
//             // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-variable-mathsf-global-get-x
//             // Why can't we get a const?
//             GlobalType::Const(_) => Err(ValidationError::new()),
//             GlobalType::Var(ty) => Ok(vec![ty.clone()]),
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub struct GlobalSetOperation {
//     index: Index,
// }
// impl std::fmt::Display for GlobalSetOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(global.set {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for GlobalSetOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for GlobalSetOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty = inputs.pop()?;
//         ctx.set_global(self.index.clone(), ty)?;
//         Ok(vec![])
//     }
// }

// /**
//  * Table Instructions
//  */
// #[derive(Clone, Debug)]
// pub struct TableGetOperation {
//     index: Index,
// }
// impl std::fmt::Display for TableGetOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(table.get {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableGetOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableGetOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let table = ctx.get_table(&self.index)?;
//         inputs.pop()?.try_into_num()?.try_into_i32()?;
//         Ok(vec![ValueType::RefType(*table.ref_type())])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct TableSetOperation {
//     index: Index,
// }
// impl std::fmt::Display for TableSetOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(table.set {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableSetOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableSetOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let table = ctx.get_table(&self.index)?;
//         let _tb_set_value = inputs.pop()?.try_into_ref_type()?;
//         let _tbl_index = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         if _tb_set_value == *table.ref_type() {
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct TableSizeOperation {
//     index: Index,
// }
// impl std::fmt::Display for TableSizeOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(table.size {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableSizeOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableSizeOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _table = ctx.get_table(&self.index)?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct TableGrowOperation {
//     index: Index,
// }
// impl std::fmt::Display for TableGrowOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(table.grow {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableGrowOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableGrowOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let table = ctx.get_table(&self.index)?;

//         let _tbl_index = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _tb_set_value = inputs.pop()?.try_into_ref_type()?;

//         if _tb_set_value == *table.ref_type() {
//             Ok(vec![ValueType::Num(NumType::I32)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct TableFillOperation {
//     index: Index,
// }
// impl std::fmt::Display for TableFillOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(table.fill {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableFillOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableFillOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let table = ctx.get_table(&self.index)?;

//         let _tbl_from = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _tb_set_value = inputs.pop()?.try_into_ref_type()?;
//         let _tbl_to = inputs.pop()?.try_into_num()?.try_into_i32()?;

//         if _tb_set_value == *table.ref_type() {
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct TableCopyOperation {
//     table_x_index: Index,
//     table_y_index: Index,
// }
// impl std::fmt::Display for TableCopyOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "(table.copy {} {})",
//             self.table_x_index, self.table_y_index
//         )
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableCopyOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             table_x_index: Index::parse(tokens)?,
//             table_y_index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableCopyOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let table_x = ctx.get_table(&self.table_x_index)?;
//         let table_y = ctx.get_table(&self.table_y_index)?;
//         if table_x.ref_type() == table_y.ref_type() {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct TableInitOperation {
//     table: Index,
//     element: Index,
// }
// impl std::fmt::Display for TableInitOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(table.init {} {})", self.table, self.element)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableInitOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             table: Index::parse(tokens)?,
//             element: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for TableInitOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let table = ctx.get_table(&self.table)?;
//         let element = ctx.get_element(&self.element)?;
//         if *table.ref_type() == *element {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct ElementDropOperation {
//     element: Index,
// }
// impl std::fmt::Display for ElementDropOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(elem.drop {})", self.element)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ElementDropOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             element: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for ElementDropOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, _inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         ctx.get_element(&self.element)?;
//         Ok(vec![])
//     }
// }

// /**
//  * Memory Instructions
//  */
// // (t).load memarg
// #[derive(Debug, Clone)]
// pub struct LoadMemoryOperation {
//     ty: NumType,
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     args: MemoryArgument,
// }
// impl LoadMemoryOperation {
//     pub fn new(ty: NumType, args: MemoryArgument) -> Self {
//         Self {
//             ty,
//             args,
//             memory: None,
//         }
//     }
// }
// impl std::fmt::Display for LoadMemoryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({}.load {})", self.ty, self.args)
//     }
// }
// impl ValidateInstruction for LoadMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.ty.bit_width();
//         let alignment = 2_u32.pow(self.args.align());
//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![ValueType::Num(self.ty.clone())])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // (t).load(N)_(Sign) memarg
// // N => 8, 16, 32
// #[derive(Clone, Debug)]
// pub struct LoadNMemoryOperation {
//     ty: IntType,
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     load_n: MemoryLoadNumber,
//     sign: SignType,
//     args: MemoryArgument,
// }
// impl LoadNMemoryOperation {
//     pub fn new(
//         ty: IntType,
//         load_n: MemoryLoadNumber,
//         sign: SignType,
//         args: MemoryArgument,
//     ) -> Self {
//         Self {
//             ty,
//             load_n,
//             sign,
//             args,
//             memory: None,
//         }
//     }
// }
// impl std::fmt::Display for LoadNMemoryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "({}.load{}_{} {})",
//             self.ty, self.load_n, self.sign, self.args
//         )
//     }
// }
// impl ValidateInstruction for LoadNMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.load_n.bit_width();
//         let alignment = 2_u32.pow(self.args.align());
//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![ValueType::from(self.ty)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct LoadVectorMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     _memory: Option<MemoryIndex>,
//     vector: Option<VectorMemoryOp>,
//     args: MemoryArgument,
//     lane: Option<LaneIndex>,
// }
// impl LoadVectorMemoryOperation {
//     pub fn new(vector: VectorMemoryOp, args: MemoryArgument) -> Self {
//         Self {
//             vector: Some(vector),
//             args,
//             _memory: None,
//             lane: None,
//         }
//     }

//     pub fn lane(vector: VectorMemoryOp, args: MemoryArgument, lane: LaneIndex) -> Self {
//         Self {
//             _memory: None,
//             lane: Some(lane),
//             vector: Some(vector),
//             args,
//         }
//     }

//     pub fn load(args: MemoryArgument) -> Self {
//         Self {
//             _memory: None,
//             vector: None,
//             args,
//             lane: None,
//         }
//     }
// }
// impl std::fmt::Display for LoadVectorMemoryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.vector {
//             Some(vector) => write!(f, "(v128.load{} {})", vector, self.args),
//             None => write!(f, "(v128.load {})", self.args),
//         }?;
//         if let Some(lane) = self.lane.as_ref() {
//             write!(f, " {}", lane)?;
//         }
//         Ok(())
//     }
// }
// impl ValidateInstruction for LoadVectorMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("fix vector memory load")
//         // let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         // let bit_width = self.vector.bit_width();
//         // let alignment = 2_u32.pow(self.args.align());
//         // if alignment <= (bit_width / 8) {
//         //     inputs.pop()?.try_into_num()?.try_into_i32()?;
//         //     Ok(vec![ValueType::from(self.ty)])
//         // } else {
//         //     Err(ValidationError::new())
//         // }
//     }
// }

// #[derive(Clone, Debug)]
// pub struct StoreVectorMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     args: MemoryArgument,
//     _memory: Option<MemoryIndex>,
//     vector: Option<VectorMemoryOp>,
//     lane: Option<LaneIndex>,
// }
// impl StoreVectorMemoryOperation {
//     pub fn lane(vector: VectorMemoryOp, args: MemoryArgument, lane: LaneIndex) -> Self {
//         Self {
//             _memory: None,
//             lane: Some(lane),
//             vector: Some(vector),
//             args,
//         }
//     }

//     pub fn load(args: MemoryArgument) -> Self {
//         Self {
//             _memory: None,
//             vector: None,
//             args,
//             lane: None,
//         }
//     }
// }
// impl std::fmt::Display for StoreVectorMemoryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.vector {
//             Some(vector) => write!(
//                 f,
//                 "(v128.store{}_lane {} {})",
//                 vector,
//                 self.args,
//                 self.lane.as_ref().unwrap()
//             ),
//             None => write!(f, "(v128.store {})", self.args),
//         }
//     }
// }
// impl ValidateInstruction for StoreVectorMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("fix vector memory store")
//         // let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         // let bit_width = self.vector.bit_width();
//         // let alignment = 2_u32.pow(self.args.align());
//         // if alignment <= (bit_width / 8) {
//         //     inputs.pop()?.try_into_num()?.try_into_i32()?;
//         //     Ok(vec![ValueType::from(self.ty)])
//         // } else {
//         //     Err(ValidationError::new())
//         // }
//     }
// }

// // (t).store memarg
// #[derive(Clone, Debug)]
// pub struct StoreMemoryOperation {
//     ty: NumType,
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     args: MemoryArgument,
// }
// impl StoreMemoryOperation {
//     pub fn new(ty: NumType, args: MemoryArgument) -> Self {
//         Self {
//             ty,
//             args,
//             memory: None,
//         }
//     }
// }
// impl std::fmt::Display for StoreMemoryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({}.store {})", self.ty, self.args)
//     }
// }
// impl ValidateInstruction for StoreMemoryOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.ty.bit_width();
//         let alignment = 2_u32.pow(self.args.align());
//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_num()?.try_into_ty(self.ty)?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // (t).store(N) memarg
// // N => 8, 16, 32
// #[derive(Clone, Debug)]
// pub struct StoreNMemoryOperation {
//     ty: NumType,
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     load_n: MemoryLoadNumber,
//     args: MemoryArgument,
// }
// impl StoreNMemoryOperation {
//     pub fn new(ty: NumType, load_n: MemoryLoadNumber, args: MemoryArgument) -> Self {
//         Self {
//             ty,
//             args,
//             load_n,
//             memory: None,
//         }
//     }
// }
// impl std::fmt::Display for StoreNMemoryOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "({}.store{} {})", self.ty, self.load_n, self.args)
//     }
// }
// impl ValidateInstruction for StoreNMemoryOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.load_n.bit_width();
//         let alignment = 2_u32.pow(self.args.align());
//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_num()?.try_into_ty(self.ty)?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // v128.load(NxM)_(sx) memarg
// // NxM => 8x8, 16x4, 32x2
// #[derive(Clone, Debug)]
// pub struct VectorLoadMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     op: VectorMemoryOp,
//     _sign: SignType,
//     args: MemoryArgument,
// }
// impl ValidateInstruction for VectorLoadMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.op.bit_width();
//         let alignment = 2_u32.pow(self.args.align());
//         if alignment <= bit_width {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![ValueType::VecType(VecType)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // v128.load(N)_splat memarg
// // N => 8, 16, 32, 64
// #[derive(Clone, Debug)]
// pub struct VectorLoadNSplatMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     width_n: MemoryWidth,
//     args: MemoryArgument,
// }
// impl ValidateInstruction for VectorLoadNSplatMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.width_n.bit_width();
//         let alignment = 2_u32.pow(self.args.align());

//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![ValueType::VecType(VecType)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // v128.load(N)_zero memarg
// // N => 32, 64
// #[derive(Clone, Debug)]
// pub struct VectorLoadNZeroMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<MemoryIndex>,
//     width_n: MemoryZeroWidth,
//     args: MemoryArgument,
// }
// impl ValidateInstruction for VectorLoadNZeroMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _memory = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let bit_width = self.width_n.bit_width();
//         let alignment = 2_u32.pow(self.args.align());

//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![ValueType::VecType(VecType)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // v128.load(N)_lane memarg laneidx
// // N => 8, 16, 32, 64
// #[derive(Clone, Debug)]
// pub struct VectorLoadNLaneMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<Index>,
//     width_n: MemoryWidth,
//     index: Index,
//     args: MemoryArgument,
// }
// impl ValidateInstruction for VectorLoadNLaneMemoryOperation {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // if !self.index.validate_against_memory_width(&self.width_n) {
//         //     return Err(ValidationError::new());
//         // }

//         let _memory = ctx.get_memory(self.memory.as_ref())?;
//         let bit_width = self.width_n.bit_width();
//         let alignment = 2_u32.pow(self.args.align());

//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_vec_type()?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![ValueType::VecType(VecType)])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// // v128.store(N)_lane memarg laneidx
// // N => 8, 16, 32, 64
// #[derive(Clone, Debug)]
// pub struct VectorStoreNLaneMemoryOperation {
//     // This is more for me, 99% of the time this is supposed to be None, as described
//     // in the spec.
//     memory: Option<Index>,
//     width_n: MemoryWidth,
//     index: Index,
//     args: MemoryArgument,
// }
// impl ValidateInstruction for VectorStoreNLaneMemoryOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // if !self.index.validate_against_memory_width(&self.width_n) {
//         //     return Err(ValidationError::new());
//         // }

//         let _memory = ctx.get_memory(self.memory.as_ref())?;
//         let bit_width = self.width_n.bit_width();
//         let alignment = 2_u32.pow(self.args.align());

//         if alignment <= (bit_width / 8) {
//             inputs.pop()?.try_into_vec_type()?;
//             inputs.pop()?.try_into_num()?.try_into_i32()?;
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub struct MemorySize(Option<Index>);
// impl MemorySize {
//     pub fn new() -> Self {
//         Self(None)
//     }
// }
// impl std::fmt::Display for MemorySize {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(memory.size)")
//     }
// }
// impl ValidateInstruction for MemorySize {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, _inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _ = ctx.get_memory(self.0.as_ref())?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct MemoryGrow(Option<Index>);
// impl MemoryGrow {
//     pub fn new() -> Self {
//         Self(None)
//     }
// }
// impl std::fmt::Display for MemoryGrow {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(memory.grow)")
//     }
// }
// impl ValidateInstruction for MemoryGrow {
//     // type Output = [ValueType; 1];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _ = ctx.get_memory(self.0.as_ref())?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct MemoryFill(Option<Index>);
// impl MemoryFill {
//     pub fn new() -> Self {
//         Self(None)
//     }
// }
// impl std::fmt::Display for MemoryFill {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(memory.fill)")
//     }
// }
// impl ValidateInstruction for MemoryFill {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _ = ctx.get_memory(self.0.as_ref())?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         Ok(vec![])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct MemoryCopy(Option<MemoryIndex>);
// impl MemoryCopy {
//     pub fn new() -> Self {
//         Self(None)
//     }
// }
// impl std::fmt::Display for MemoryCopy {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(memory.copy)")
//     }
// }
// impl ValidateInstruction for MemoryCopy {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _ = ctx.get_memory(self.0.map(Index::Index).as_ref())?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         Ok(vec![])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct MemoryInit {
//     memory: Option<MemoryIndex>,
//     data: Index,
// }
// impl MemoryInit {
//     pub fn new(index: Index) -> Self {
//         Self {
//             data: index,
//             memory: None,
//         }
//     }
// }
// impl std::fmt::Display for MemoryInit {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(memory.init {})", self.data)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for MemoryInit {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             memory: None,
//             data: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for MemoryInit {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _ = ctx.get_memory(self.memory.map(Index::Index).as_ref())?;
//         let _ = ctx.get_data(&self.data)?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         Ok(vec![])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct DataDrop {
//     data: Index,
// }
// impl std::fmt::Display for DataDrop {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(data.drop {})", self.data)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for DataDrop {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             data: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for DataDrop {
//     // type Output = [ValueType; 0];
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let _ = ctx.get_data(&self.data)?;
//         Ok(vec![])
//     }
// }

// /**
//  * Control Instructions
//  */
// #[derive(Clone, Debug)]
// pub struct NopOperation;
// impl std::fmt::Display for NopOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(nop)")
//     }
// }
// impl Execute for NopOperation {
//     fn exec(&self, _: &mut Stack) -> Result<(), Trap> {
//         Ok(())
//     }
// }
// impl ValidateInstruction for NopOperation {
//     // type Output = [ValueType; 0];
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![])
//     }
// }

// #[derive(Clone, Debug)]
// pub struct UnreachableOperation;
// impl std::fmt::Display for UnreachableOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(unreachable)")
//     }
// }
// impl ValidateInstruction for UnreachableOperation {
//     // type Output = Vec<ValueType>;
//     fn validate(&self, _: &mut Context, _input: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-unreachable
//         Ok(vec![])
//     }
// }

// // block _blocktype_ _instr_* end
// #[derive(Clone, Default, Debug)]
// pub struct BlockOperation {
//     label: Option<String>,
//     ty: Option<BlockType>,
//     ops: Instruction,
// }
// impl std::fmt::Display for BlockOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(block")?;
//         if let Some(label) = self.label.as_ref() {
//             write!(f, " ${}", label)?;
//         }
//         if let Some(ty) = self.ty.as_ref() {
//             write!(f, " {}", ty)?;
//         }
//         if self.ops.is_empty() {
//             write!(f, ")")
//         } else {
//             f.pad(&" ".repeat(2))?;
//             write!(f, "\n{}", self.ops)?;
//             f.pad(&" ".repeat(4))?;
//             write!(f, ")")
//         }
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BlockOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         if tokens.peek().copied().expect_right_paren().is_ok() {
//             return Ok(BlockOperation::default());
//         }
//         // Copied from FunctionDefinition
//         let label = get_id(tokens);

//         if tokens.peek().copied().expect_right_paren().is_ok() {
//             return Ok(BlockOperation {
//                 label,
//                 ..Default::default()
//             });
//         }

//         let ty = match get_next_keyword(tokens) {
//             Some(Keyword::Type) => Some(BlockType::parse(tokens)?),
//             Some(Keyword::Result) => Some(BlockType::parse(tokens)?),
//             _ => None,
//         };
//         let ops = Instruction::parse(tokens)?;

//         Ok(Self { label, ty, ops })
//     }
// }
// impl ValidateInstruction for BlockOperation {
//     // type Output = Vec<ValueType>;

//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // Block type must be some function type
//         todo!("validate meeee");
//         // let ty = self.ty.get_function_type(ctx)?;
//         // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
//         // ctx.prepend_label(ty.output().clone());
//         // TODO(Alec): Validate how to execute instructions
//         todo!("implement instructions for a given opcode for a block");
//         // let output = self.instructions.validate(ctx, inputs)?;
//         // remove the label
//         // let label = ctx.remove_prepend_label()?;
//         // // validate the output matches the function output
//         // if *ty.output() == label && *ty.output().values() == output {
//         //     // This should never trigger
//         //     for input_ty in ty.input().values() {
//         //         let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
//         //     }
//         //     return Ok(ty.output().clone().values().to_vec());
//         // }
//         // Err(ValidationError::new())
//     }
// }

// // loop _blocktype_ _instr_* end
// #[derive(Clone, Default, Debug)]
// pub struct LoopOperation {
//     label: Option<String>,
//     ty: Option<BlockType>,
//     ops: Instruction,
// }
// impl std::fmt::Display for LoopOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(loop")?;
//         if let Some(label) = self.label.as_ref() {
//             write!(f, " ${}", label)?;
//         }
//         if let Some(ty) = self.ty.as_ref() {
//             write!(f, " {}", ty)?;
//         }
//         if self.ops.is_empty() {
//             write!(f, ")")
//         } else {
//             writeln!(f, "{}", self.ops)?;
//             write!(f, ")")
//         }
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for LoopOperation {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         if tokens.peek().copied().expect_right_paren().is_ok() {
//             return Ok(LoopOperation::default());
//         }
//         let label = get_id(tokens);
//         let ty = match get_next_keyword(tokens) {
//             Some(Keyword::Type) => Some(BlockType::parse(tokens)?),
//             Some(Keyword::Result) => Some(BlockType::parse(tokens)?),
//             _ => None,
//         };
//         let instructions = Instruction::parse(tokens)?;
//         Ok(Self {
//             label,
//             ty,
//             ops: instructions,
//         })
//     }
// }
// impl ValidateInstruction for LoopOperation {
//     // type Output = Vec<ValueType>;

//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("Validate loop operations")
//         // // Block type must be some function type
//         // let ty = self.ty.get_function_type(ctx)?;
//         // // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
//         // ctx.prepend_label(ty.output().clone());
//         // // TODO(Alec): Validate how to execute instructions
//         // let output = self.instructions.validate(ctx, inputs)?;
//         // // remove the label
//         // let label = ctx.remove_prepend_label()?;
//         // if *ty.output() == label && *ty.output().values() == output {
//         //     // This should never trigger
//         //     for input_ty in ty.input().values() {
//         //         let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
//         //     }
//         //     return Ok(ty.output().clone().values().to_vec());
//         // }
//         // Err(ValidationError::new())
//     }
// }

// // if _blocktype_ _instr_* else _instr_* end
// #[derive(Clone, Default, Debug)]
// pub struct IfOperation {
//     label: Option<String>,
//     ty: Option<BlockType>,
//     condition: Option<Instruction>,
//     thens: Instruction,
//     elses: Option<Instruction>,
// }
// impl std::fmt::Display for IfOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(if")?;
//         if let Some(label) = self.label.as_ref() {
//             write!(f, " ${}", label)?;
//         }
//         if let Some(ty) = self.ty.as_ref() {
//             write!(f, " {}", ty)?;
//         }
//         if let Some(condition) = self.condition.as_ref() {
//             write!(f, "{}", condition)?;
//         }
//         if self.thens.is_empty() {
//             writeln!(f, "(then)")?;
//         } else {
//             writeln!(f, "(then {})", self.thens)?;
//         }
//         if let Some(elses) = self.elses.as_ref() {
//             if elses.is_empty() {
//                 writeln!(f, "(else)")?;
//             } else {
//                 writeln!(f, "(else {})", elses)?;
//             }
//         }

//         Ok(())
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for IfOperation {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         if tokens.peek().copied().expect_right_paren().is_ok() {
//             return Ok(IfOperation::default());
//         }

//         let label = get_id(tokens);
//         let ty = match get_next_keyword(tokens) {
//             Some(Keyword::Type) => Some(BlockType::parse(tokens)?),
//             Some(Keyword::Result) => Some(BlockType::parse(tokens)?),
//             _ => None,
//         };

//         // println!("{:?}", ty);

//         let condition = match get_next_keyword(tokens) {
//             Some(Keyword::Then) | Some(Keyword::Else) | None => None,
//             Some(_) => Some(InBracketInstruction::parse(tokens)?.into()),
//         };

//         // println!("---{:?}", condition);

//         let mut thens = Instruction::new();
//         tokens.next().expect_left_paren()?;
//         tokens.next().expect_keyword_token(Keyword::Then)?;
//         if !tokens.peek().copied().expect_right_paren().is_ok() {
//             thens = InBracketInstruction::parse(tokens)?.into();
//         }
//         tokens.next().expect_right_paren()?;

//         let mut elses = None;
//         if Some(Keyword::Else) == get_next_keyword(tokens) {
//             tokens.next().expect_left_paren()?;
//             tokens.next().expect_keyword_token(Keyword::Else)?;
//             if !tokens.peek().copied().expect_right_paren().is_ok() {
//                 elses = Some(InBracketInstruction::parse(tokens)?.into());
//             }
//             tokens.next().expect_right_paren()?;
//         };

//         Ok(Self {
//             label,
//             ty,
//             condition,
//             thens,
//             elses,
//         })
//     }
// }
// impl ValidateInstruction for IfOperation {
//     // type Output = Vec<ValueType>;

//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("Validation for if")
//         // // Block type must be some function type
//         // let ty = self.ty.get_function_type(ctx)?;
//         // // Prepend return value to this to ctx.labels (aka, this points at ctx.get_labels(0))
//         // ctx.prepend_label(ty.output().clone());
//         // // TODO(Alec): Validate how to execute instructions
//         // let o1 = self.t1.validate(ctx, inputs)?;
//         // let o2 = self.t2.validate(ctx, inputs)?;
//         // // remove the label
//         // let label = ctx.remove_prepend_label()?;
//         // // validate the output matches the functions output
//         // if label.values() == o1
//         //     && label.values() == o2
//         //     && *ty.output().values() == o1
//         //     && *ty.output().values() == o2
//         // {
//         //     // validate that the input stack has an i32 and all of of the function types arguments
//         //     let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         //     for input_ty in ty.input().values() {
//         //         let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
//         //     }
//         //     return Ok(ty.output().clone().values().to_vec());
//         // }
//         // Err(ValidationError::new())
//     }
// }
// #[derive(Clone, Default, Debug)]
// pub struct BrOperation {
//     label: Index,
// }
// impl std::fmt::Display for BrOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(br {})", self.label)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BrOperation {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         let label = if let Some(label) = get_id(tokens) {
//             Index::Id(label)
//         } else {
//             Index::Index(read_u32(tokens.next().expect_number()?)?)
//         };

//         Ok(Self { label })
//     }
// }
// impl ValidateInstruction for BrOperation {
//     // type Output = Vec<ValueType>;
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty = ctx.get_label(&self.label)?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         for input_ty in ty.values() {
//             let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
//         }
//         Ok(ty.values().to_vec())
//     }
// }

// #[derive(Clone, Default, Debug)]
// pub struct BrIfOperation {
//     label: Index,
// }
// impl std::fmt::Display for BrIfOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(br_if {})", self.label)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BrIfOperation {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         let label = if let Some(label) = get_id(tokens) {
//             Index::Id(label)
//         } else {
//             Index::Index(read_u32(tokens.next().expect_number()?)?)
//         };

//         Ok(Self { label })
//     }
// }
// impl ValidateInstruction for BrIfOperation {
//     // type Output = Vec<ValueType>;
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty = ctx.get_label(&self.label)?;
//         let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         for input_ty in ty.values() {
//             let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
//         }
//         Ok(ty.values().to_vec())
//     }
// }

// ////////////////////////////////////////////////////////////////////////////////
// /// https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-table-l-ast-l-n
// #[derive(Clone, Default, Debug)]
// pub struct BrTableOperation {
//     labels: Vec<Index>,
// }
// impl std::fmt::Display for BrTableOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let labels = self
//             .labels
//             .iter()
//             .map(|l| l.to_string())
//             .collect::<Vec<_>>()
//             .join(" ");
//         write!(f, "(br_table {})", labels)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BrTableOperation {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         let mut labels = vec![];
//         loop {
//             let label = if let Some(label) = get_id(tokens) {
//                 Index::Id(label)
//             } else if tokens.peek().copied().expect_number().is_ok() {
//                 Index::Index(read_u32(tokens.next().expect_number()?)?)
//             } else {
//                 return if labels.is_empty() {
//                     Err(Error::new(
//                         tokens.next().cloned(),
//                         "Expected there to be more labels for br_table operation",
//                     ))
//                 } else {
//                     Ok(Self { labels })
//                 };
//             };
//             labels.push(label)
//         }
//     }
// }
// impl ValidateInstruction for BrTableOperation {
//     // type Output = Vec<ValueType>;
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // TODO(Alec): WHAT DOES IT ALL MEAN? WHAT IS THE MEANING OF LIFE?
//         // performs an indirect branch through an operand indexing into the label
//         // vector that is an immediate to the instruction, or to a default target
//         // if the operand is out of bounds
//         todo!("re-evaluate br_table rules");
//         // let ty_n = ctx.get_label(&self.label)?;
//         // // validate the rest of the labels exist
//         // for label in self.labels.iter() {
//         //     let ty = ctx.get_label(label)?;
//         //     if ty.values().len() != ty_n.values().len() {
//         //         return Err(ValidationError::new());
//         //     }
//         //     for index in ty.values().len()..0 {
//         //         let value = inputs
//         //             .0
//         //             .get(index - inputs.0.len())
//         //             .ok_or_else(ValidationError::new)?;
//         //         value.try_into_value_type(
//         //             ty.values().get(index).ok_or_else(ValidationError::new)?,
//         //         )?;
//         //     }
//         // }
//         // // There must be enough input as expected in `ty_n`
//         // if ty_n.values().len() > inputs.0.len() {
//         //     return Err(ValidationError::new());
//         // }
//         // let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         // for index in ty_n.values().len()..0 {
//         //     let _ = inputs
//         //         .pop()?
//         //         .try_into_value_type(ty_n.values().get(index).unwrap())?;
//         // }

//         // Ok(ty_n.values().to_vec())
//     }
// }
// ////////////////////////////////////////////////////////////////////////////////

// // Come back and review
// // https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-return
// #[derive(Clone, Default, Debug)]
// pub struct ReturnOperation;
// impl std::fmt::Display for ReturnOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(return)")
//     }
// }
// impl ValidateInstruction for ReturnOperation {
//     // type Output = Vec<ValueType>;

//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty_set = ctx.returning().ok_or_else(ValidationError::new)?;
//         for ty in ty_set.values() {
//             let _ = inputs.pop()?.try_into_value_type(&ty)?;
//         }
//         Ok(ty_set.values().to_vec())
//     }
// }

// #[derive(Clone, Debug)]
// pub struct CallOperation {
//     index: Index,
// }
// impl std::fmt::Display for CallOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(call {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for CallOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
// impl ValidateInstruction for CallOperation {
//     // type Output = Vec<ValueType>;
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let func = ctx.get_function(&self.index)?;
//         // validate function input arguments
//         for ty in func.input().values() {
//             let _ = inputs.pop()?.try_into_value_type(&ty)?;
//         }
//         // if all valid, return output arguments
//         Ok(func.output().values().to_vec())
//     }
// }

// #[derive(Debug, Clone)]
// pub struct CallIndirectOperation {
//     table: Index,
//     ty: TypeUse,
// }

// impl std::fmt::Display for CallIndirectOperation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(call_indirect {} {})", self.table, self.ty)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for CallIndirectOperation {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         let table = match Index::parse(&mut tokens.clone()) {
//             Ok(_) => Index::parse(tokens).unwrap(),
//             Err(_) => Index::default(), // 0 index
//         };
//         let ty = TypeUse::parse(tokens)?;
//         Ok(Self { table, ty })
//     }
// }
// impl ValidateInstruction for CallIndirectOperation {
//     // type Output = Vec<ValueType>;

//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("Alec fix call indirect operation index call")
//         // let table = ctx.get_table(self.table)?;
//         // // validate reference type is func ref
//         // table.ref_type().try_into_func_ref()?;
//         // // type ty must be defined
//         // let ty = ctx.get_type(self.ty)?;
//         // // validate the function
//         // let _ = inputs.pop()?.try_into_num()?.try_into_i32()?;
//         // for input_ty in ty.input().values() {
//         //     let _ = inputs.pop()?.try_into_value_type(&input_ty)?;
//         // }
//         // Ok(ty.output().values().to_vec())
//     }
// }

// #[derive(Clone, Debug)]
// pub struct RefNull {
//     ty: HeapType,
// }
// impl ValidateInstruction for RefNull {
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         Ok(vec![ValueType::RefType(self.ty.into())])
//     }
// }
// impl std::fmt::Display for RefNull {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(ref.null {})", self.ty)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for RefNull {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             ty: HeapType::parse(tokens)?,
//         })
//     }
// }

// #[derive(Clone, Debug)]
// pub struct RefIsNull;
// impl ValidateInstruction for RefIsNull {
//     fn validate(&self, _: &mut Context, input: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         input.pop()?.try_into_ref_type()?;
//         Ok(vec![ValueType::Num(NumType::I32)])
//     }
// }
// impl std::fmt::Display for RefIsNull {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(ref.is_null)")
//     }
// }
// #[derive(Clone, Debug)]
// pub struct RefFunc {
//     index: Index,
// }

// impl RefFunc {
//     pub fn new(index: Index) -> Self {
//         Self { index }
//     }
// }
// impl ValidateInstruction for RefFunc {
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         ctx.get_function(&self.index)?;
//         todo!("implement get_ref");
//         // ctx.get_ref(&self.index)
//         // Ok(vec![ValueType::RefType(RefType::FuncRef)])
//     }
// }
// impl std::fmt::Display for RefFunc {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(ref.func {})", self.index)
//     }
// }
// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for RefFunc {
//     fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
//         Ok(Self {
//             index: Index::parse(tokens)?,
//         })
//     }
// }
