use super::{func::FunctionType, instruction::Expression, types::{FunctionIndex, TypeIndex, ValueType}};


/// The definition of a function
pub struct Function {
    // An index to the signature of the function. Can be found in the module.
    // Accessible through local indices.
    types: TypeIndex,

    // Vector of mutable local variables and their types. Index of the first local
    // is the smallest index not referencing a parameter (ex. if there are 3 parameters
    // we would use 3 to reference the lowest local)
    locals: Vec<ValueType>,

    // A sequence of instructions. When complete the stack should match the function
    // result type.
    body: Expression,
}

pub struct Table {
    
}

pub struct Module {
    types: Vec<FunctionType>,
    functions: Vec<Function>,
    tables: Vec<Table>,
    memories: Vec<Memory>,
    globals: Vec<Global>,
    elements: Vec<Element>,
    datas: Vec<Data>,
    imports: Vec<Import>,
    export: Vec<Export>,
    start: FunctionIndex,
}