//! Check that WebAssembly module is well-formed. Only valid modules will be
//! instantiated.
//!
//! The type system defines the validity using an abstract syntax of a module and
//! the contents. Each abstract syntax has a typing rule that specifies the constraints
//! that apply to it. Rules are given 2 forms:
//!
//! 1. _prose_: describe the meaning in an intuitive form
//! 2. _formal notation_: describe the rule in a mathematical form
//!
//! ## Validity relative to a context
//!
//! Collect information about the surrounding module and the definitions in scope.
//! - _types_: list of types in module
//! - _functions_: list of functions in module
//! - _tables_: list of tables in module
//! - _memories_: list of memories in module
//! - _globals_: list of globals in module
//! - _element segment_: list of elements in module
//! - _data segments_: list of data in module. Represented as ok entry
//! - _locals_: list of locals declared in the current function (includes parameters). Represented by their value.
//! - _labels_: stack of labels accessible from the current position. Represented by return type.
//! - _returning_: Return type of current function. An optional return type. None if function returns nothing
//! - _references_: list of function indices that occur in the module outside functions and can hence be used to form references inside them.
//!
//! Keep a sequence of suitable types for each index space. Locals, labels and return
//! types are used for validating instructions in function bodies, but left empty
//! elsewhere. THe label stack is only part of the context that changes as validation
//! of the instruction sequence proceeds.
//!
//! ### Prose Notation
//!
//! > Phrase _A_ is valid with type _T_ if all constraints expressed by relative rules
//! > are met. There is an assumption that a context exists.
//!
//! ### Formal Notation
//!
//! > phrase A : has respective type T, holds under the assumptions encoded in C.
//! > If all premises hold, then the conclusion holds. If there is no premise, they
//! > are _axioms_ whose conclusion holds unconditionally.

pub mod instruction;

use std::convert::TryFrom;

use crate::structure::types::{
    FunctionDefinition, FunctionIndex, FunctionType, GlobalType, Index, MemoryType, RefType,
    ResultType, TableType, TypeDefinition, ValueType, Variable,
};

/// Representation of the validation context of a [Data] segment inside of a
/// Web Assembly [Module].
#[derive(Clone, Default)]
pub struct Ok {}

impl Ok {
    pub fn is_ok(&self) -> bool {
        true
    }
}

#[derive(Clone, Default)]
pub struct CtxList<T>(Vec<(Option<String>, T)>);
impl<T> CtxList<T> {
    pub fn push(&mut self, id: Option<String>, def: T) {
        self.0.push((id, def))
    }

    pub fn find(&self, index: &Index) -> Result<&T, ValidationError> {
        let item = match index {
            Index::Id(id) => self
                .0
                .iter()
                .find(|i| i.0.as_ref() == Some(id))
                .ok_or_else(ValidationError::new),
            Index::Index(i) => {
                let index = usize::try_from(*i).map_err(|_| ValidationError::new())?;
                self.0.get(index).ok_or_else(ValidationError::new)
            }
        }?;
        let item = &item.1;
        Ok(item)
    }
}

#[derive(Clone, Default)]
pub struct Context {
    types: CtxList<FunctionType>,
    functions: CtxList<FunctionType>,
    tables: CtxList<TableType>,
    memories: CtxList<MemoryType>,
    elements: CtxList<RefType>,
    globals: CtxList<GlobalType>,
    datas: CtxList<Ok>,

    // Used for local functions
    locals: CtxList<ValueType>,
    labels: CtxList<ResultType>,
    returning: Option<ResultType>,

    references: Vec<FunctionIndex>,
}

impl Context {
    pub fn push_type(&mut self, def: &TypeDefinition) {
        self.types.push(def.id().cloned(), def.ty());
    }

    pub fn get_type(&self, index: &Index) -> Result<&FunctionType, ValidationError> {
        self.types.find(index)
    }

    pub fn push_function(&mut self, _: &FunctionDefinition) -> Result<(), ValidationError> {
        todo!("Holy man");
        // let ty = match def.ty() {
        //     TypeUse::Index(index) => self.get_type(index)?.clone(),
        //     TypeUse::Inline { params, results } => {
        //         FunctionType::new(params.clone(), results.clone())
        //     }
        // };
        // self.functions.push(def.id().cloned(), ty);
        // Ok(())
    }

    pub fn get_function(&self, index: &Index) -> Result<&FunctionType, ValidationError> {
        self.types.find(index)
    }

    pub fn get_element(&self, index: &Index) -> Result<&RefType, ValidationError> {
        self.elements.find(index)
    }

    pub fn get_data(&self, index: &Index) -> Result<&Ok, ValidationError> {
        self.datas.find(index)
    }

    pub fn get_memory(&self, index: Option<&Index>) -> Result<&MemoryType, ValidationError> {
        let i = match index {
            Some(id) => id.clone(),
            None => Index::Index(0),
        };
        self.memories.find(&i)
    }

    pub fn push_local(&mut self, var: &Variable) {
        self.locals.push(var.id().cloned(), *var.ty())
    }

    pub fn get_local(&self, index: &Index) -> Result<&ValueType, ValidationError> {
        self.locals.find(index)
    }

    pub fn get_label(&self, index: &Index) -> Result<&ResultType, ValidationError> {
        // Note: We are supposed to push labels to the index 0 when we add them,
        // but thats wasteful so here we go, pre-mature optimization! :)
        //
        // We will push and pop labels instead basically meaning the list is in
        // reverse. Meaning indexing will also be in reverse
        self.labels.find(index)
        // let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        // if self.labels.is_empty() || self.labels.len() <= index {
        //     return Err(ValidationError::new());
        // } else {
        //     let max_size = self.labels.len() - 1;
        //     self.labels
        //         .get(max_size - index)
        //         .ok_or_else(ValidationError::new)
        // }
    }

    pub fn prepend_label(&mut self, _: ResultType) {
        todo!("aaahhh")
        // self.labels.push(ty)
    }

    pub fn remove_prepend_label(&mut self) -> Result<ResultType, ValidationError> {
        todo!("remove_prepend_label");
        // self.labels.pop().ok_or_else(ValidationError::new)
    }

    // pub fn get_local(&self, index: u32) -> Result<&ValueType, ValidationError> {
    //     let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
    //     self.locals.get(index).ok_or_else(ValidationError::new)
    // }

    pub fn set_local(&mut self, _: Index, _: ValueType) -> Result<(), ValidationError> {
        todo!("set_local");
        // let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        // if self.locals.get(index).is_some() {
        //     self.locals[index] = ty;
        //     Ok(())
        // } else {
        //     Err(ValidationError::new())
        // }
    }

    pub fn get_global(&self, index: &Index) -> Result<&GlobalType, ValidationError> {
        self.globals.find(index)
    }

    pub fn set_global(&mut self, _: Index, _: ValueType) -> Result<(), ValidationError> {
        todo!("set_global")
        // let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        // match self.globals.get(index) {
        //     Some(GlobalType::Var(_)) => {
        //         self.globals[index] = GlobalType::Var(ty);
        //         Ok(())
        //     }
        //     Some(GlobalType::Const(_)) => Err(ValidationError::new()),
        //     None => Err(ValidationError::new()),
        // }
    }

    pub fn get_table(&self, index: &Index) -> Result<&TableType, ValidationError> {
        self.tables.find(index)
    }

    pub fn get_ref(&self, index: u32) -> Option<&FunctionIndex> {
        let index = usize::try_from(index).expect("TO be able to convert u32 to usize");
        self.references.get(index)
    }

    // fn contains_reference(&self, arg: u32) -> bool {
    //     self.references.contains(&arg)
    // }

    pub fn set_returning(&mut self, opt: Option<ResultType>) {
        self.returning = opt;
    }

    pub fn returning(&self) -> Option<&ResultType> {
        self.returning.as_ref()
    }

    // pub fn prepare_function_execution(&mut self, locals: Vec<ValueType>, ty: &FunctionType) {
    //     self.locals = locals;
    //     self.labels = vec![ty.output().clone()];
    //     self.returning = Some(ty.output().clone());
    // }

    // pub fn functions(&self) -> &Vec<FunctionType> {
    //     &self.functions
    // }

    // pub fn locals(&self) -> &[ValueType] {
    //     &self.locals
    // }

    // pub fn labels(&self) -> &[ResultType] {
    //     &self.labels
    // }

    // pub fn datas(&self) -> &[Ok] {
    //     &self.datas
    // }

    // pub fn memories(&self) -> &[MemoryType] {
    //     &self.memories
    // }
}

pub trait Validation<Extra> {
    /// Validate if the structure is valid.
    fn validate(&self, ctx: &Context, args: Extra) -> Result<(), ValidationError>;
}

#[derive(Debug)]
pub enum ValidationErrorTy {
    MultipleMemories,
}

impl std::fmt::Display for ValidationErrorTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MultipleMemories => write!(f, "multiple memories"),
        }
    }
}

#[derive(Debug)]
pub struct ValidationError {
    error: Option<String>,
    ty: Option<ValidationErrorTy>,
}

impl std::error::Error for ValidationError {}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TODO: make better {:?}", self)
    }
}

impl ValidationError {
    pub fn new() -> Self {
        Self {
            error: None,
            ty: None,
        }
    }

    pub fn multiple_memory(e: impl ToString) -> Self {
        Self {
            error: Some(e.to_string()),
            ty: Some(ValidationErrorTy::MultipleMemories),
        }
    }

    pub fn error(&self) -> String {
        self.error.clone().unwrap_or_default()
    }

    pub fn error_ty(&self) -> String {
        self.ty
            .as_ref()
            .map(ValidationErrorTy::to_string)
            .unwrap_or_default()
    }
}

impl Default for ValidationError {
    fn default() -> Self {
        Self::new()
    }
}

pub type ValidateResult<T> = Result<T, ValidationError>;

pub struct Input(pub Vec<ValueType>);

impl Input {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn pop(&mut self) -> ValidateResult<ValueType> {
        self.0.pop().ok_or_else(ValidationError::new)
    }
}

impl Default for Input {
    fn default() -> Self {
        Self::new()
    }
}

pub trait ValidateInstruction {
    // type Output: IntoIterator<Item = ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>>;
}

// pub struct InstructionSequence {
//     instructions: Vec<Box<dyn ValidateInstruction>>,
// }

// impl ValidateInstruction for InstructionSequence {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         for index in 0..self.instructions.len() {
//             // TODO(Alec): I believe this bit of code validates all instructions.
//             // https://webassembly.github.io/spec/core/valid/instructions.html#non-empty-instruction-sequence-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-mathit-instr-n
//             let output = self.instructions[index].validate(ctx, inputs)?;
//             if index == self.instructions.len() - 1 {
//                 return Ok(output);
//             } else {
//                 inputs.0.extend(output);
//             }
//         }
//         Err(ValidationError::new())
//     }
// }

// Used for function bodies, global initialization values, elements and offsets of
// element segments and offsets of data segments are given as expressions which
// are sequences of instructions terminated by an end marker.
// pub struct Expression {
//     instructions: Vec<Box<dyn ValidateInstruction>>,
// }

// impl Expression {
//     pub fn new(instructions: Vec<Box<dyn ValidateInstruction>>) -> Self {
//         Self { instructions }
//     }
// }

// impl ValidateInstruction for Expression {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         for index in 0..self.instructions.len() {
//             // TODO(Alec): I believe this bit of code validates all instructions.
//             // https://webassembly.github.io/spec/core/valid/instructions.html#non-empty-instruction-sequence-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-mathit-instr-n
//             let output = self.instructions[index].validate(ctx, inputs)?;
//             if index == self.instructions.len() - 1 {
//                 return Ok(output);
//             } else {
//                 inputs.0.extend(output);
//             }
//         }
//         Err(ValidationError::new())
//     }
// }

// TODO(Alec): I'm not sure we need these expressions just yet. Maybe when we
// get into parsing.
// pub struct ConstantExpression {
//     instructions: Vec<Box<dyn ValidateInstruction>>,
// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ConstantExpression {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         match tokens.next().expect_keyword()? {
//             Keyword::Const(ty) => {
//                 let number = read_number(ty.clone(), tokens.next().expect_number()?)?;
//                 tokens.next().expect_right_paren()?;
//                 Ok(ConstantExpression {
//                     instructions: vec![Box::new(Const::new(ty, number))],
//                 })
//             }
//             key => Err(Error::new(
//                 None,
//                 format!("Expected constant expression. Found {:?}", key),
//             )),
//         }
//     }
// }

// impl ValidateInstruction for ConstantExpression {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         for index in 0..self.instructions.len() {
//             // TODO(Alec): I believe this bit of code validates all instructions.
//             // https://webassembly.github.io/spec/core/valid/instructions.html#non-empty-instruction-sequence-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-mathit-instr-n
//             // TODO(Alec): Validate that the expression thats ran is only constant expressions.
//             // because right now other parts of the program depend on these only being constant commands
//             let output = self.instructions[index].validate(ctx, inputs)?;
//             if index == self.instructions.len() - 1 {
//                 return Ok(output);
//             } else {
//                 inputs.0.extend(output);
//             }
//         }
//         Err(ValidationError::new())
//     }
// }

// pub struct Expr {

// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Expr {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         match tokens.next().expect_keyword()? {
//             Keyword::LocalGet => todo!(),
//             Keyword::LocalSet => todo!(),
//             Keyword::LocalTee => todo!(),
//             Keyword::GlobalGet => todo!(),
//             Keyword::GlobalSet => todo!(),
//             Keyword::TableGet => todo!(),
//             Keyword::TableSet => todo!(),
//             Keyword::TableSize => todo!(),
//             Keyword::TableGrow => todo!(),
//             Keyword::TableFill => todo!(),
//             Keyword::TableCopy => todo!(),
//             Keyword::TableInit => todo!(),
//             Keyword::ElemDrop => todo!(),
//             Keyword::MemorySize => todo!(),
//             Keyword::MemoryGrow => todo!(),
//             Keyword::MemoryFill => todo!(),
//             Keyword::MemoryCopy => todo!(),
//             Keyword::MemoryInit => todo!(),
//             Keyword::DataDrop => todo!(),
//             Keyword::Load(_) => todo!(),
//             Keyword::Store(_) => todo!(),
//             Keyword::I32Load8(_) => todo!(),
//             Keyword::I32Load16(_) => todo!(),
//             Keyword::I64Load8(_) => todo!(),
//             Keyword::I64Load16(_) => todo!(),
//             Keyword::I64Load32(_) => todo!(),
//             Keyword::I32Store8 => todo!(),
//             Keyword::I32Store16 => todo!(),
//             Keyword::I64Store8 => todo!(),
//             Keyword::I64Store16 => todo!(),
//             Keyword::I64Store32 => todo!(),
//             Keyword::MemArgsAlign(_) => todo!(),
//             Keyword::MemArgsOffset(_) => todo!(),
//             Keyword::Type => todo!(),
//             Keyword::Func => todo!(),
//             Keyword::Param => todo!(),
//             Keyword::Result => todo!(),
//             Keyword::Start => todo!(),
//             Keyword::Local => todo!(),
//             Keyword::Global => todo!(),
//             Keyword::Table => todo!(),
//             Keyword::Memory => todo!(),
//             Keyword::Elem => todo!(),
//             Keyword::Data => todo!(),
//             Keyword::Declare => todo!(),
//             Keyword::Offset => todo!(),
//             Keyword::Item => todo!(),
//             Keyword::Import => todo!(),
//             Keyword::Export => todo!(),
//             Keyword::V128Load => todo!(),
//             Keyword::V128Store => todo!(),
//             Keyword::VecLoad8x8(_) => todo!(),
//             Keyword::VecLoad16x4(_) => todo!(),
//             Keyword::VecLoad32x2(_) => todo!(),
//             Keyword::VecLoadSplat(_) => todo!(),
//             Keyword::VecLoadZero(_) => todo!(),
//             Keyword::VecLoadLane(_) => todo!(),
//             Keyword::VecStoreLane(_) => todo!(),
//             Keyword::Const(_) => todo!(),
//             Keyword::ConstV128 => todo!(),
//             Keyword::RefNull => todo!(),
//             Keyword::RefFunc => todo!(),
//             Keyword::RefExtern => todo!(),
//             Keyword::RefIsNull => todo!(),
//             Keyword::IntClz(_) => todo!(),
//             Keyword::IntCtz(_) => todo!(),
//             Keyword::IntPopCnt(_) => todo!(),
//             Keyword::IntExtend8Signed(_) => todo!(),
//             Keyword::IntExtend16Signed(_) => todo!(),
//             Keyword::I64Extend32Signed => todo!(),
//             Keyword::NegativeFloat(_) => todo!(),
//             Keyword::AbsoluteFloat(_) => todo!(),
//             Keyword::SquareRootFloat(_) => todo!(),
//             Keyword::CeilFloat(_) => todo!(),
//             Keyword::FloorFLoat(_) => todo!(),
//             Keyword::TruncateFloat(_) => todo!(),
//             Keyword::NearestFloat(_) => todo!(),
//             Keyword::AddInt(_) => todo!(),
//             Keyword::SubInt(_) => todo!(),
//             Keyword::MultiplyInt(_) => todo!(),
//             Keyword::AndInt(_) => todo!(),
//             Keyword::OrInt(_) => todo!(),
//             Keyword::XORInt(_) => todo!(),
//             Keyword::ShiftLeftInt(_) => todo!(),
//             Keyword::DivideInt { shape, sign } => todo!(),
//             Keyword::RemainderInt { shape, sign } => todo!(),
//             Keyword::ShiftRightInt { shape, sign } => todo!(),
//             Keyword::RotateInt { shape, direction } => todo!(),
//             Keyword::AddFloat(_) => todo!(),
//             Keyword::SubFloat(_) => todo!(),
//             Keyword::MultiplyFloat(_) => todo!(),
//             Keyword::DivideFloat(_) => todo!(),
//             Keyword::MinFloat(_) => todo!(),
//             Keyword::MaxFloat(_) => todo!(),
//             Keyword::CopySign(_) => todo!(),
//             Keyword::I32EqualTest => todo!(),
//             Keyword::I64EqualTest => todo!(),
//             Keyword::CompareIntEqual(_) => todo!(),
//             Keyword::CompareIntNotEqual(_) => todo!(),
//             Keyword::CompareIntLessThen { shape, sign } => todo!(),
//             Keyword::CompareIntLessOrEqual { shape, sign } => todo!(),
//             Keyword::CompareIntGreaterThen { shape, sign } => todo!(),
//             Keyword::CompareIntGreaterOrEqual { shape, sign } => todo!(),
//             Keyword::CompareFloatEqual(_) => todo!(),
//             Keyword::CompareFloatNotEqual(_) => todo!(),
//             Keyword::CompareFloatLessThen(_) => todo!(),
//             Keyword::CompareFloatLessOrEqual(_) => todo!(),
//             Keyword::CompareFloatGreaterThen(_) => todo!(),
//             Keyword::CompareFloatGreaterOrEqual(_) => todo!(),
//             Keyword::I32WrapI64 => todo!(),
//             Keyword::I64ExtendI32(_) => todo!(),
//             Keyword::F32DemoteF64 => todo!(),
//             Keyword::F64PromoteF32 => todo!(),
//             Keyword::I32TruncateF32(_) => todo!(),
//             Keyword::I64TruncateF32(_) => todo!(),
//             Keyword::I32TruncateF64(_) => todo!(),
//             Keyword::I64TruncateF64(_) => todo!(),
//             Keyword::I32TruncateSatF32(_) => todo!(),
//             Keyword::I64TruncateSatF32(_) => todo!(),
//             Keyword::I32TruncateSatF64(_) => todo!(),
//             Keyword::I64TruncateSatF64(_) => todo!(),
//             Keyword::F32ConvertI32(_) => todo!(),
//             Keyword::F32ConvertI64(_) => todo!(),
//             Keyword::F64ConvertI32(_) => todo!(),
//             Keyword::F64ConvertI64(_) => todo!(),
//             Keyword::F32ReinterpretI32 => todo!(),
//             Keyword::F64ReinterpretI64 => todo!(),
//             Keyword::I32ReinterpretI32 => todo!(),
//             Keyword::I64ReinterpretI64 => todo!(),
//             Keyword::V128Not => todo!(),
//             Keyword::V128And => todo!(),
//             Keyword::V128AndNot => todo!(),
//             Keyword::V128Or => todo!(),
//             Keyword::V128XOr => todo!(),
//             Keyword::V128BitSelect => todo!(),
//             Keyword::V128AnyTrue => todo!(),
//             Keyword::VecIntNegative(_) => todo!(),
//             Keyword::VecIntAbsolute(_) => todo!(),
//             Keyword::VecI8x16PopCnt => todo!(),
//             Keyword::VecI8x16AverageUnsigned => todo!(),
//             Keyword::VecI16x8AverageUnsigned => todo!(),
//             Keyword::VecFloatNegative(_) => todo!(),
//             Keyword::VecFloatAbsolute(_) => todo!(),
//             Keyword::VecFloatSquareRoot(_) => todo!(),
//             Keyword::VecFloatCeil(_) => todo!(),
//             Keyword::VecFloatFloor(_) => todo!(),
//             Keyword::VecFloatTruncate(_) => todo!(),
//             Keyword::VecFloatNearest(_) => todo!(),
//             Keyword::I32x4TruncSatF32x4(_) => todo!(),
//             Keyword::I32x4TruncSatF64x2Zero(_) => todo!(),
//             Keyword::F64x2PromoteLowF32x4 => todo!(),
//             Keyword::F32x4PemoteF64x2Zero => todo!(),
//             Keyword::F32x4ConvertI32x4(_) => todo!(),
//             Keyword::F64x2ConvertLowI32x4(_) => todo!(),
//             Keyword::I16x8ExtendAddPairwiseI8x16(_) => todo!(),
//             Keyword::I32x4ExtaddPairwiseI16x8(_) => todo!(),
//             Keyword::VecIntEqual(_) => todo!(),
//             Keyword::VecIntNotEqual(_) => todo!(),
//             Keyword::VecIntLessThen { shape, sign } => todo!(),
//             Keyword::VecIntLessOrEqual { shape, sign } => todo!(),
//             Keyword::VecIntGreaterThen { shape, sign } => todo!(),
//             Keyword::VecIntGreaterOrEqual { shape, sign } => todo!(),
//             Keyword::VecEqualFloat(_) => todo!(),
//             Keyword::VecNotEqualFloat(_) => todo!(),
//             Keyword::VecLessThenFloat(_) => todo!(),
//             Keyword::VecLessOrEqualFloat(_) => todo!(),
//             Keyword::VecGreaterThenFloat(_) => todo!(),
//             Keyword::VecGreaterOrEqualFloat(_) => todo!(),
//             Keyword::VecSwizzleFloatI8x16 => todo!(),
//             Keyword::VecIntAdd(_) => todo!(),
//             Keyword::VecIntSub(_) => todo!(),
//             Keyword::VecIntMultiplyI16x8 => todo!(),
//             Keyword::VecIntMultiplyI32x4 => todo!(),
//             Keyword::VecIntMultiplyI64x2 => todo!(),
//             Keyword::VecAddSatI16x8(_) => todo!(),
//             Keyword::VecSubtractSatI16x8(_) => todo!(),
//             Keyword::VecI32x4DotProductOfI16x8Signed => todo!(),
//             Keyword::VecMinInt { shape, sign } => todo!(),
//             Keyword::VecMaxInt { shape, sign } => todo!(),
//             Keyword::VecSubFloat(_) => todo!(),
//             Keyword::VecAddSatI8x16(_) => todo!(),
//             Keyword::VecSubtractSatI8x16(_) => todo!(),
//             Keyword::VecAddFloat(_) => todo!(),
//             Keyword::VecDivFloat(_) => todo!(),
//             Keyword::VecMulFloat(_) => todo!(),
//             Keyword::VecMinFloat(_) => todo!(),
//             Keyword::VecMaxFloat(_) => todo!(),
//             Keyword::VecPMin(_) => todo!(),
//             Keyword::VecPMax(_) => todo!(),
//             Keyword::I16x8Q15mulrSatS => todo!(),
//             Keyword::I8x16NarrowI16x8(_) => todo!(),
//             Keyword::I16x8NarrowI32x4(_) => todo!(),
//             Keyword::I16x8ExtendI8x16 { half, sign } => todo!(),
//             Keyword::I32x4ExtendI16x8 { half, sign } => todo!(),
//             Keyword::I64x2ExtendI32x4 { half, sign } => todo!(),
//             Keyword::I16x8ExtendMultiplyI8x16 { half, sign } => todo!(),
//             Keyword::I32x4ExtendMultiplyI16x8 { half, sign } => todo!(),
//             Keyword::I64x2ExtendMultiplyI32x4 { half, sign } => todo!(),
//             Keyword::VecTest(_) => todo!(),
//             Keyword::VecBitmask(_) => todo!(),
//             Keyword::VecShiftLeft(_) => todo!(),
//             Keyword::VecShiftRight { shape, sign } => todo!(),
//             Keyword::VecShuffle => todo!(),
//             Keyword::VecSplat(_) => todo!(),
//             Keyword::VecExtract { shape, sign } => todo!(),
//             Keyword::VecReplate(_) => todo!(),
//             Keyword::Module => todo!(),
//             Keyword::Bin => todo!(),
//             Keyword::Quote => todo!(),
//             Keyword::Script => todo!(),
//             Keyword::Register => todo!(),
//             Keyword::Invoke => todo!(),
//             Keyword::Get => todo!(),
//             Keyword::AssertMalformed => todo!(),
//             Keyword::AssertInvalid => todo!(),
//             Keyword::AssertUnlinkable => todo!(),
//             Keyword::AssertReturn => todo!(),
//             Keyword::AssertTrap => todo!(),
//             Keyword::AssertExhaustion => todo!(),
//             Keyword::NaNCanonical => todo!(),
//             Keyword::NaNArithmetic(_) => todo!(),
//             Keyword::Infinit => todo!(),
//             Keyword::NaN => todo!(),
//             Keyword::Input => todo!(),
//             Keyword::Output => todo!(),
//         }
//     }
// }
