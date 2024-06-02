//! Web assembly executes code when instantiating a module or invoking an exported
//! function on a module instance. Execution is done inside of a stack machine.
//! The machine records operand values and controls constructs and storing state.
//!
//! The rules are as follows for executing web assembly
//! - There is a given store
//! - There is a stack that can be modified by pushing and popping values, labels and frames
//! - Some rules require the stack to have one frame. The most recent is called _the current frame_
//! - Both store and frame mutate by replacing their components. Replacements should apply globally
//! - When exceptions (traps) are raised, computation should be aborted
//! - Execution can end in a _jump_ to a designated target
//! - Execution can _enter_ and _exit_ [InstructionSequence] that form [Block]s
//! - Assertions are expressed when crucial invariants about program state.
//!
//! A program is made up of the current [Store], the current call [Frame] and
//! a [InstructionSequence].

// https://webassembly.github.io/spec/core/exec/conventions.html

pub mod instrunctions;

use crate::{
    structure::{
        module::Function,
        types::{FunctionType, GlobalType, MemoryType, NumType, RefType, TableType, ValueType},
    },
    validation::InstructionSequence,
};

/**
 * Constants
 * Page Size must be 64Ki, or 65536 bytes
 */
pub const PAGE_SIZE: usize = 65536;

/**
 * Addresses are dynamic, globally unique references to runtime objects. Indices
 * (which are static), module-local references to their original definitions. A
 * _memory address [MemoryAddress]_ denotes the abstract address of a memory
 * _instance_ in the store, not an offset inside of a memory instance.
 *
 * Every address should be unique. There should never be an address that overlaps.
 * Addresses should always be growing.
 */

pub type FunctionAddress = usize;
pub type TableAddress = usize;
pub type MemoryAddress = usize;
pub type GlobalAddress = usize;
pub type ElementAddress = usize;
pub type DataAddress = usize;
pub type ExternAddress = usize;

// i32, i64, f32, f64 default value is 0
#[derive(Debug, Clone, Copy)]
pub enum Number {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

// impl FromStr for Number {
//     type Err = ;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         todo!()
//     }
// }

impl Into<Value> for Number {
    fn into(self) -> Value {
        Value::Num(self)
    }
}

impl Number {
    pub fn is_int(&self) -> bool {
        match self {
            Number::I32(_) | Number::I64(_) => true,
            _ => false,
        }
    }

    pub fn ty(&self) -> NumType {
        match self {
            Number::I32(_) => NumType::I32,
            Number::I64(_) => NumType::I64,
            Number::F32(_) => NumType::F32,
            Number::F64(_) => NumType::F64,
        }
    }
}

// RefType default value is null
pub enum Reference {
    Null,
    Func(FunctionAddress),
    Extern(ExternAddress),
}

// v128 default value is 0
pub enum Value {
    Num(Number),
    Vec(i128),
    Ref(Reference),
}

impl Value {
    pub fn try_into_num(self) -> Result<Number, Trap> {
        if let Self::Num(v) = self {
            Ok(v)
        } else {
            Err(Trap::new())
        }
    }
}

pub struct Trap {}
impl Trap {
    pub fn new() -> Self {
        Self {}
    }
}

// Result is a sequence of values or a trap
type WasmResult = Result<Vec<Value>, Trap>;

/// A runtime representation of a function. A closure of the original function
/// over the runtime module instance of it's originating module. Use the module
/// instate to resolve references to other definitions during execution.
///
/// Function instance are immutable and their identity is not observable. The
/// embedder might provide implicit or explicit means for distinguishing their
/// [Address]
pub enum FunctionInstance {
    Module {
        ty: FunctionType,
        module: ModuleInstance,
        code: Function, // TODO(Alec): Is this a pointer?
                        // https://webassembly.github.io/spec/core/syntax/modules.html#syntax-func
    },
    // A function expressed outside of web assembly and passed as an import.
    Host {
        ty: FunctionType,
        // Assume host code is non-deterministic. Assume that it still works with
        // the runtime.
        host_code: (), // TODO(Alec): I will need to figure out how to implement this.
    },
}

/// Record type and hold a list of [Reference]s. Mutably through table instructions,
/// execution of an active element segment or by external means like an embedder.
///
/// It's a **requirement** that all elements in the list have the table type `ty`.
/// It is also a **requirement** that elements does not grow above it's limit.
pub struct TableInstance {
    ty: TableType,
    elements: Vec<Reference>,
}

/// Representation of linear memory. The length of memory is always a multiple of
/// Wasm _page size_, which is 65536 bytes, or 64Ki. Mutated through Memory instructions,
/// active data segment or external means like an embedder.
///
/// It's expected the length of bytes, divided by page size, never exceeds the
/// max size.
pub struct MemoryInstance {
    ty: MemoryType,
    data: Vec<u8>,
}

/// Mutable through global instructions or through an embedder.
///
/// Value is **required** to have the type recorded in `ty`.
pub struct GlobalInstance {
    ty: GlobalType,
    value: Value,
}

/// Representation of an element segment. Hold a vector of references and their
/// common type.
pub struct ElementInstance {
    ty: RefType,
    elements: Vec<Reference>,
}

/// Runtime representation of data Segment in bytes.
pub struct DataInstance {
    data: Vec<u8>,
}

/// Runtime representation of an entity that can be imported or exported. Keep
/// an address that can be used in the [Store]
pub enum ExternalValue {
    Func(FunctionAddress),
    Table(TableAddress),
    Memory(MemoryAddress),
    Global(GlobalAddress),
}

/// RUntime representation of an export. Defines the name and value of the export.
pub struct ExportInstance {
    name: String,
    value: ExternalValue,
}

/// Represent all global state. Has the runtime representation of all instances
/// of functions, tables, memories, globals, element segments and data segments
/// that were allocated during the life time of the abstract machine.
///
/// It's assumed that other modules do not have access to the store.
///
/// > In practice, store may use techniques to clean up objects that are no longer
/// > in use (garbage collection).
pub struct Store {
    functions: Vec<FunctionInstance>,
    tables: Vec<TableInstance>,
    memories: Vec<MemoryInstance>,
    globals: Vec<GlobalInstance>,
    elements: Vec<ElementInstance>,
    datas: Vec<DataInstance>,
}

/// Runtime instance of a module. Created after instantiating a module. Collect
/// all runtime representations of all entities that are imported, defined or
/// exported by the module
///
/// Each component references a runtime instance that corresponds to it's respective
/// declaration from the original module - whether imported or defined - in order
/// of their static indices.
///
/// *_addresses variables can be found using the address in the store. Understand
/// that the fields in here are stored as a static indices list. The actual address
/// maybe different. The Module instance Addresses point to the index to use in the
/// [Store].
pub struct ModuleInstance {
    types: Vec<FunctionType>,
    function_addresses: Vec<FunctionAddress>,
    table_addresses: Vec<TableAddress>,
    memory_addresses: Vec<MemoryAddress>,
    global_addresses: Vec<GlobalAddress>,
    element_addresses: Vec<ElementAddress>,
    data_addresses: Vec<DataAddress>,
    // All exports have a different name.
    exports: Vec<ExportInstance>,
}

/// Carry the return arity _n_ of the respective function, hold the values of the
/// locals (args included) in the order corresponding to their static local indicies,
/// and references to the functions own modules instance
pub struct Frame {
    locals: Vec<Value>,
    module: ModuleInstance, // TODO(Alec): this needs to be a pointer? right?
}

pub struct Label {
    // TODO(Alec): DO i need an index here or something?
    // carry an argument arity _n_ and their associated branch _target_
    // Not sure what the above means...
    //
    // InstructionSequence is the continuation to execute when the branch is taken,
    // in place of the original control
    instructions: InstructionSequence,
}

/// Most instructions interact with a [Stack].
///
/// > It's possible to model wasm semantic using separate stacks for operands,
/// > control structure, and calls. However, because the stack are interdependent,
/// > additional book keeping about associated stack heights would be required.
///
/// TODO(Alec): Probably implement the above :) How hard can it be...right???
pub struct Stack {
    // Operands of instructions
    values: Vec<Value>,
    // Active structured control instructions that can be targeted by branches
    labels: Label,
    // Call frame of active function calls
    frame: Frame,
}

impl Stack {
    pub fn push(&mut self, value: impl Into<Value>) {
        self.values.push(value.into());
    }

    pub fn pop_and_assert_int(&mut self) -> Result<Number, Trap> {
        let number = self.values.pop().ok_or_else(Trap::new)?.try_into_num()?;
        if number.is_int() {
            Ok(number)
        } else {
            Err(Trap::new())
        }
    }

    pub fn pop_and_assert_num(&mut self, ty: NumType) -> Result<Number, Trap> {
        let value = self.values.pop().ok_or_else(Trap::new)?;
        let number = value.try_into_num()?;
        if number.ty() != ty {
            Err(Trap::new())
        } else {
            Ok(number)
        }
    }
}

/// A computation over instructions that operates relative to the state of a current
/// frame referring to the module instance in which the computations runs, ie. where
/// the current function originates from
pub struct Thread {
    frame: Frame,
    instructions: InstructionSequence,
}

pub struct Configuration {
    store: Store,
    thread: Thread,
}
