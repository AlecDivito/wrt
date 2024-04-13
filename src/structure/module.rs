use super::{instruction::Expression, types::{FunctionIndex, FunctionType, GlobalIndex, GlobalType, MemoryIndex, MemoryType, RefType, TableIndex, TableType, TypeIndex, ValueType}};


/// The definition of a function
pub struct Function {
    // An index to the signature of the function. Can be found in the module.
    // Accessible through local indices.
    ty: TypeIndex,

    // Vector of mutable local variables and their types. Index of the first local
    // is the smallest index not referencing a parameter (ex. if there are 3 parameters
    // we would use 3 to reference the lowest local)
    locals: Vec<ValueType>,

    // A sequence of instructions. When complete the stack should match the function
    // result type.
    body: Expression,
}

/// A vector of opaque values of a particular [ReferenceType]. The min size in the
/// limits of the table type specifies the initial size of that table.
/// 
/// Initialized through element segments
pub struct Table {
    ty: TableType,
}

/// Describe memory as their memory type. It's a vector of raw uninterpreted bytes.
pub struct Memory {
    ty: MemoryType
}

// Store the type of global. Initialize it by evaluating the globals expression.
pub struct Global {
    ty: GlobalType,
    // Requirement, must be a constant initializer expression.
    init: Expression,
}

// Element Mode
pub enum ElementMode {
    // Can copy to a table using `table.init`
    Passive,
    // not available at runtime. Serves to forward-declare references that are
    // formed in code with instructions like `ref.func`
    Declarative,
    // Active element copies it's elements into a table.
    Active {
        // the table to use
        table: TableIndex,
        // constant express defining an offset into the table
        offset: Expression,
    }
}

/// Initial contents of a table is uninitialized. Use [Element] to initialize
/// a subrange of a table from a static vector of elements.
pub struct Element {
    // Defined reference type
    ty: RefType,
    // Required to be a list of constant element expressions
    init: Vec<Expression>,
    // Identify type of element
    mode: ElementMode
}

/// Data mode type
pub enum DataMode {
    // Can copy memory using `memory.init`
    Passive,
    // Copy contents into a memory during instantiation, specified by the
    // memory index.
    Active {
        memory: MemoryIndex,
        // Expression must be a constant expression that defines an offset into memory.
        offset: Expression,
    }
}

/// Can be used to initialize a range of memory from a static vector of types.
pub struct Data {
    // Data to copy into memory
    init: Vec<u8>,
    // mode of how to copy
    mode: DataMode
}

/// The index to the implementation of the type of export required.
pub enum ExportDescription {
    Func(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex)
}

/// Export functionality to be accessed by the host environment. Only accessible
/// after module has been instantiated.
pub struct Export {
    // Name is unique
    name: String,
    description: ExportDescription,
}

/// The index of the expected type that definition should provide and match during
/// instantiation.
pub enum ImportDescription {
    Func(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex)
}

/// Imports required for instantiation
/// 
/// NOTE: Possible to import the same "module.name" pair. Depends on how a embedded
/// resolves imports with the same name.
pub struct Import {
    // name of module to import from
    module: String,
    // name of entity within the above module
    // NOTE: Name may not be unique.
    name: String,
    // The definition to import
    description: ImportDescription,
}

pub struct Module {
    types: Vec<FunctionType>,
    functions: Vec<Function>,

    // Select tables through [TableIndex]. Starts at 0. Implicitly reference 0.
    tables: Vec<Table>,

    // Select memory through [MemoryIndex]. Starts at 0. Implicitly reference 0.
    memories: Vec<Memory>,

    // Referenced through [GlobalIndex]. Start with smallest index not referencing a global import.
    globals: Vec<Global>,

    // Referenced through [ElementIndex]
    elements: Vec<Element>,

    // Referenced through [DataIndex]
    datas: Vec<Data>,

    imports: Vec<Import>,

    // Every import defines an index in the respective index space. In each index space
    // the indices of imports go before the first index of any definition contained
    // in the module itself.
    export: Vec<Export>,

    // Automatically invoked function when module is instantiated, after Tables
    // and memories have been initialized.
    //
    // NOTE: Intended for use to initialize the state of a module.
    start: FunctionIndex,
}