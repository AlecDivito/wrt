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
    FunctionIndex, FunctionType, GlobalType, MemoryType, RefType, ResultType, TableType, ValueType
};

/// Representation of the validation context of a [Data] segment inside of a
/// Web Assembly [Module].
pub struct Ok {}

pub struct Context {
    ty: Vec<FunctionType>,
    functions: Vec<FunctionType>,
    tables: Vec<TableType>,
    memories: Vec<MemoryType>,
    elements: Vec<RefType>,
    globals: Vec<GlobalType>,
    datas: Vec<Ok>,
    locals: Vec<ValueType>,
    labels: Vec<ResultType>,
    returning: Option<ResultType>,
    references: Vec<FunctionIndex>,
}

impl Context {
    pub fn get_type(&self, index: u32) -> Option<&FunctionType> {
        let index = usize::try_from(index).expect("TO be able to convert u32 to usize");
        self.ty.get(index)
    }

    pub fn get_function(&self, index: u32) -> Option<&FunctionType> {
        let index = usize::try_from(index).expect("TO be able to convert u32 to usize");
        self.functions.get(index)
    }

    pub fn get_element(&self, index: u32) -> Result<&RefType, ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        self.elements.get(index).ok_or_else(ValidationError::new)
    }

    pub fn get_data(&self, index: u32) -> Result<&Ok, ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        self.datas.get(index).ok_or_else(ValidationError::new)
    }

    pub fn get_memory(&self, index: Option<u32>) -> Result<&MemoryType, ValidationError> {
        let i =  match index {
            Some(x) => usize::try_from(x).map_err(|_| ValidationError::new())?,
            None => 0,
        };
        self.memories.get(i).ok_or_else(ValidationError::new)
    }

    pub fn get_label(&self, index: u32) -> Result<&ResultType, ValidationError> {
        // Note: We are supposed to push labels to the index 0 when we add them,
        // but thats wasteful so here we go, pre-mature optimization! :) 
        //
        // We will push and pop labels instead basically meaning the list is in
        // reverse. Meaning indexing will also be in reverse
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        if self.labels.is_empty() || self.labels.len() <= index {
            return Err(ValidationError::new())
        } else {
            let max_size = self.labels.len() - 1;
            self.labels.get(max_size - index).ok_or_else(ValidationError::new)
        }
    }

    pub fn prepend_label(&mut self, ty: ResultType) {
        self.labels.push(ty)
    }

    pub fn remove_prepend_label(&mut self)-> Result<ResultType, ValidationError> {
        self.labels.pop().ok_or_else(ValidationError::new)
    }

    pub fn get_local(&self, index: u32) -> Result<&ValueType, ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        self.locals.get(index).ok_or_else(ValidationError::new)
    }

    pub fn set_local(&mut self, index: u32, ty: ValueType) -> Result<(), ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        if self.locals.get(index).is_some() {
            self.locals[index] = ty;
            Ok(())
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn get_global(&self, index: u32) -> Result<&GlobalType, ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        self.globals.get(index).ok_or_else(ValidationError::new)
    }

    pub fn set_global(&mut self, index: u32, ty: ValueType) -> Result<(), ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        match self.globals.get(index) {
            Some(GlobalType::Var(_)) => {
                self.globals[index] = GlobalType::Var(ty);
                Ok(())
            }
            Some(GlobalType::Const(_)) => Err(ValidationError::new()),
            None => Err(ValidationError::new())
        }
    }

    pub fn get_table(&self, index: u32) -> Result<&TableType, ValidationError> {
        let index = usize::try_from(index).map_err(|_| ValidationError::new())?;
        self.tables.get(index).ok_or_else(ValidationError::new)
    }

    pub fn get_ref(&self, index: u32) -> Option<&FunctionIndex> {
        let index = usize::try_from(index).expect("TO be able to convert u32 to usize");
        self.references.get(index)
    }

    fn contains_reference(&self, arg: u32) -> bool {
        self.references.contains(&arg)
    }
    
    pub fn returning(&self) -> Option<&ResultType> {
        self.returning.as_ref()
    }
}


pub trait Validation<Extra> {
    /// Validate if the structure is valid.
    fn validate(&self, ctx: &Context, args: Extra) -> Result<(), ValidationError>;
}

pub struct ValidationError {}

impl ValidationError {
    pub fn new() -> Self {
        Self {}
    }
}

pub type ValidateResult<T> = Result<T, ValidationError>;

pub struct Input(pub Vec<ValueType>);

impl Input {
    pub fn pop(&mut self) -> ValidateResult<ValueType> {
        self.0.pop().ok_or_else(ValidationError::new)
    }
}


pub trait ValidateInstruction {
    // type Output: IntoIterator<Item = ValueType>;
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>>;
}

pub struct InstructionSequence {
    instructions: Vec<Box<dyn ValidateInstruction>>,
}

impl ValidateInstruction for InstructionSequence {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        for index in 0..self.instructions.len() {
            // TODO(Alec): I believe this bit of code validates all instructions.
            // https://webassembly.github.io/spec/core/valid/instructions.html#non-empty-instruction-sequence-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-mathit-instr-n
            let output = self.instructions[index].validate(ctx, inputs)?;
            if index == self.instructions.len() - 1 {
                return Ok(output)
            } else {
                inputs.0.extend(output);
            }
        }
        Err(ValidationError::new())
    }
}

/// Used for function bodies, global initialization values, elements and offsets of 
/// element segments and offsets of data segments are given as expressions which
/// are sequences of instructions terminated by an end marker.
pub struct Expression {
    instructions: Vec<Box<dyn ValidateInstruction>>,
}

impl ValidateInstruction for Expression {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        for index in 0..self.instructions.len() {
            // TODO(Alec): I believe this bit of code validates all instructions.
            // https://webassembly.github.io/spec/core/valid/instructions.html#non-empty-instruction-sequence-xref-syntax-instructions-syntax-instr-mathit-instr-ast-xref-syntax-instructions-syntax-instr-mathit-instr-n
            let output = self.instructions[index].validate(ctx, inputs)?;
            if index == self.instructions.len() - 1 {
                return Ok(output)
            } else {
                inputs.0.extend(output);
            }
        }
        Err(ValidationError::new())
    }
}

// TODO(Alec): I'm not sure we need these expressions just yet. Maybe when we
// get into parsing.
pub struct ConstantExpression {
    instructions: Vec<Box<dyn ValidateInstruction>>,
}
