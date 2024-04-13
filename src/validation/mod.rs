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
//! Continue reading: https://webassembly.github.io/spec/core/valid/conventions.html#cite-pldi2017

use crate::structure::types::{FunctionIndex, FunctionType, MemoryType, RefType, ResultType, TableType, ValueType};

/// Representation of the validation context of a [Data] segment inside of a 
/// Web Assembly [Module].
pub struct Ok {

}

pub struct Context {
    ty: Vec<FunctionType>,
    functions: Vec<FunctionType>,
    tables: Vec<TableType>,
    memories: Vec<MemoryType>,
    elements: Vec<RefType>,
    datas: Vec<Ok>,
    locals: Vec<ValueType>,
    labels: Vec<ResultType>,
    returning: Option<ResultType>,
    references: Vec<FunctionIndex>,
}