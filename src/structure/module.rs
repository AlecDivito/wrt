use crate::parse::{
    ast::{Error, Expect, IntoResult, TryGet},
    Keyword,
};
use std::{collections::HashSet, iter::Peekable, ops::Deref};

use crate::{
    parse::{ast::Parse, Token},
    validation::{
        ConstantExpression, Context, Expression, Input, ValidateInstruction, ValidateResult,
        Validation, ValidationError,
    },
};

use super::types::{
    FunctionIndex, FunctionType, GlobalIndex, GlobalType, Limit, MemoryIndex, MemoryType, RefType,
    TableIndex, TableType, TypeIndex, ValueType,
};

/// The definition of a function
pub struct Function {
    // TODO(Alec): This is temporary work around for not having an identifier list
    // instead we will just keep our id's on the object
    id: Option<String>,

    // An index to the signature of the function. Can be found in the module.
    // Accessible through local indices.
    ty_index: TypeIndex,

    // Vector of mutable local variables and their types. Index of the first local
    // is the smallest index not referencing a parameter (ex. if there are 3 parameters
    // we would use 3 to reference the lowest local)
    locals: Vec<ValueType>,

    // A sequence of instructions. When complete the stack should match the function
    // result type.
    body: Expression,
}

impl ValidateInstruction for Function {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = ctx.get_type(self.ty_index)?;
        let mut ctx1 = ctx.clone();
        // locals set to the sequence [func input args, locals]
        // labels set to singular sequence containing only [func result type]
        // return set to the [func result type]
        let locals = [ty.input().deref().clone(), self.locals.clone()].concat();
        ctx1.prepare_function_execution(locals, ty);

        let output = self.body.validate(&mut ctx1, inputs)?;
        if output == *ty.output().deref() {
            Ok(output)
        } else {
            Err(ValidationError::new())
        }
    }
}

/// A vector of opaque values of a particular [ReferenceType]. The min size in the
/// limits of the table type specifies the initial size of that table.
///
/// Initialized through element segments
pub struct Table {
    ty: TableType,
}

impl ValidateInstruction for Table {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.ty.validate(ctx, ())?;
        Ok(vec![])
    }
}

/// Describe memory as their memory type. It's a vector of raw uninterpreted bytes.
pub struct Memory {
    // TODO(Alec): This is a temporary measure. Keep the Id on the block. This
    // will need to be refactored later.
    id: Option<String>,
    ty: MemoryType,
}

impl Memory {
    pub fn new(id: Option<String>, limit: Limit) -> Self {
        Self {
            id,
            ty: MemoryType::new(limit),
        }
    }
}

impl ValidateInstruction for Memory {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.ty.validate(ctx, ())?;
        Ok(vec![])
    }
}

// Store the type of global. Initialize it by evaluating the globals expression.
pub struct Global {
    ty: GlobalType,
    // Requirement, must be a constant initializer expression.
    init: ConstantExpression,
}

impl ValidateInstruction for Global {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.ty.validate(ctx, ())?;
        // TODO(Alec): The expression must be a constant expressions only
        // Add validation for the condition above
        let mut output = self.init.validate(ctx, inputs)?;
        if output.len() == 1 {
            let ty = output.pop().unwrap();
            self.ty.validate(ctx, ty)?;
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
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
    },
}

/// Initial contents of a table is uninitialized. Use [Element] to initialize
/// a subrange of a table from a static vector of elements.
pub struct Element {
    // Defined reference type
    ty: RefType,
    // Required to be a list of constant element expressions
    inits: Vec<ConstantExpression>,
    // Identify type of element
    mode: ElementMode,
}

impl ValidateInstruction for Element {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        for init in &self.inits {
            let output = init
                .validate(ctx, inputs)?
                .pop()
                .ok_or_else(ValidationError::new)?;
            if ValueType::RefType(self.ty) != output {
                return Err(ValidationError::new());
            }
        }
        match &self.mode {
            ElementMode::Passive | ElementMode::Declarative => Ok(vec![]),
            ElementMode::Active { table, offset } => {
                let _ = ctx.get_table(*table)?;
                let mut output = offset.validate(ctx, inputs)?;
                output
                    .pop()
                    .ok_or_else(ValidationError::new)?
                    .try_into_num()?
                    .try_into_i32()?;
                if output.len() != 0 {
                    Err(ValidationError::new())
                } else {
                    Ok(vec![])
                }
            }
        }
    }
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
        offset: ConstantExpression,
    },
}

/// Can be used to initialize a range of memory from a static vector of types.
pub struct Data {
    // Data to copy into memory
    init: Vec<u8>,
    // mode of how to copy
    mode: DataMode,
}

impl ValidateInstruction for Data {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        match &self.mode {
            DataMode::Passive => Ok(vec![]),
            DataMode::Active { memory, offset } => {
                let _ = ctx.get_memory(Some(*memory))?;
                let mut output = offset.validate(ctx, inputs)?;
                output
                    .pop()
                    .ok_or_else(ValidationError::new)?
                    .try_into_num()?
                    .try_into_i32()?;
                if output.len() != 0 {
                    Err(ValidationError::new())
                } else {
                    Ok(vec![])
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct StartFunction(FunctionIndex);
impl ValidateInstruction for StartFunction {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = ctx.get_function(self.0)?;
        if ty.input().is_empty() && ty.output().is_empty() {
            Ok(vec![])
        } else {
            Err(ValidationError::new())
        }
    }
}

/// The index to the implementation of the type of export required.
pub enum ExportDescription {
    Func(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}

impl ValidateInstruction for ExportDescription {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        match self {
            ExportDescription::Func(index) => {
                ctx.get_function(*index)?.validate(ctx, ())?;
            }
            ExportDescription::Table(index) => {
                ctx.get_table(*index)?.validate(ctx, ())?;
            }
            ExportDescription::Memory(index) => {
                ctx.get_memory(Some(*index))?.validate(ctx, ())?;
            }
            ExportDescription::Global(index) => {
                ctx.get_global(*index)?.validate(ctx, ())?;
            }
        };
        Ok(vec![])
    }
}

/// Export functionality to be accessed by the host environment. Only accessible
/// after module has been instantiated.
pub struct Export {
    // Name is unique
    name: String,
    description: ExportDescription,
}

impl ValidateInstruction for Export {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.description.validate(ctx, inputs)
    }
}

/// The index of the expected type that definition should provide and match during
/// instantiation.
pub enum ImportDescription {
    Func(FunctionIndex),
    Table(TableIndex),
    Memory(MemoryIndex),
    Global(GlobalIndex),
}

impl ValidateInstruction for ImportDescription {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        match self {
            ImportDescription::Func(index) => {
                ctx.get_function(*index)?.validate(ctx, ())?;
            }
            ImportDescription::Table(index) => {
                ctx.get_table(*index)?.validate(ctx, ())?;
            }
            ImportDescription::Memory(index) => {
                ctx.get_memory(Some(*index))?.validate(ctx, ())?;
            }
            ImportDescription::Global(index) => {
                ctx.get_global(*index)?.validate(ctx, ())?;
            }
        };
        Ok(vec![])
    }
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

impl ValidateInstruction for Import {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.description.validate(ctx, inputs)
    }
}

#[derive(Default)]
pub struct Module {
    // Types definitions inside of the web assembly
    types: Vec<FunctionType>,

    // Functions inside of the module
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

    // List of imports that are required for the module to work
    imports: Vec<Import>,

    // Every import defines an index in the respective index space. In each index space
    // the indices of imports go before the first index of any definition contained
    // in the module itself.
    export: Vec<Export>,

    // Automatically invoked function when module is instantiated, after Tables
    // and memories have been initialized.
    //
    // NOTE: Intended for use to initialize the state of a module.
    start: Option<StartFunction>,
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for Module {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
        let mut this = Module::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Module)?;

        loop {
            tokens.next().expect_left_paren()?;
            let token = tokens.next();
            match token.expect_keyword()? {
                Keyword::Type => {
                    let id = tokens.peek().copied().try_id();
                    if id.is_some() {
                        let _ = tokens.next();
                    }
                }
                Keyword::Func => {
                    let id = tokens.peek().copied().try_id();
                    if id.is_some() {
                        let _ = tokens.next();
                    }

                    // let next = tokens.next();

                    // match tokens.next().expect_keyword()? {
                    //     Keyword::Export => {}
                    //     Keyword::
                    // }
                }
                Keyword::Table => {
                    let id = tokens.peek().copied().try_id();
                    if id.is_some() {
                        let _ = tokens.next();
                    }
                }
                Keyword::Memory => {
                    let id = tokens.peek().copied().try_id();
                    if id.is_some() {
                        let _ = tokens.next();
                    }
                    let limit = Limit::parse(tokens)?;
                    this.memories.push(Memory::new(id, limit));
                }
                Keyword::Global => {
                    let id = tokens.peek().copied().try_id();
                    if id.is_some() {
                        let _ = tokens.next();
                    }
                }
                Keyword::Elem => {}
                Keyword::Data => {}
                Keyword::Import => {}
                Keyword::Export => {}
                Keyword::Start => {}
                keyword => {
                    return Err(Error::new(
                        token.cloned(),
                        format!(
                            "Expected top level module import. Got {:?} instead.",
                            keyword
                        ),
                    ));
                }
            }
            tokens.next().expect_right_paren()?;
            // check if closing
            if let Ok(()) = tokens.peek().copied().expect_right_paren() {
                break;
            }
        }
        tokens.next().expect_right_paren()?;

        Ok(this)
    }
}

fn order_lists<I, O, FI, FD>(
    list: &[I],
    get_index: FI,
    get_data: FD,
) -> Result<Vec<O>, ValidationError>
where
    FI: Fn(&I) -> u32,
    FD: Fn(&I) -> Result<O, ValidationError>,
    O: Clone,
{
    let mut new_list: Vec<(O, u32)> = vec![];
    for item in list.iter() {
        let index = get_index(item);
        let data = get_data(item)?;
        new_list.push((data, index));
    }
    new_list.sort_by_key(|tuple| tuple.1);
    Ok(new_list.iter().map(|tuple| tuple.0.clone()).collect::<_>())
}

impl ValidateInstruction for Module {
    // TODO(Alec): This module validation is confusing and there are a lot of checks
    // to validate. I think coming back to this when we are closer done the program
    // will give me a better understanding of what needs to be done.
    // https://webassembly.github.io/spec/core/valid/modules.html#valid-module

    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        // internal function types, in index order
        let ctx_ref = &ctx;
        let ft = order_lists(
            &self.functions,
            |f| f.ty_index,
            move |f| ctx_ref.get_type(f.ty_index),
        )?;
        // Internal table types, in index order
        let tt = &self.tables.iter().map(|t| &t.ty).collect::<Vec<_>>();
        // Internal memory types, in index order
        let mt = &self.memories.iter().map(|t| &t.ty).collect::<Vec<_>>();
        // Internal Global types, in index order
        let gt = &self.globals.iter().map(|t| &t.ty).collect::<Vec<_>>();
        // reference types, in index order
        // let rt = self.ref

        // validate types are equal
        if ctx.types() != &self.types {
            return Err(ValidationError::new());
        }
        // validate functions are equal
        // let external_functions = ctx.function_exports()
        // let ft_star = ctx.functions();
        // let functions = [self.functions, ]
        // ctx.functions() == self.functions

        // Validate all values are ok
        if !ctx.datas().iter().all(crate::validation::Ok::is_ok) {
            return Err(ValidationError::new());
        }
        // locals is empty
        if !ctx.locals().is_empty() {
            return Err(ValidationError::new());
        }
        if !ctx.labels().is_empty() {
            return Err(ValidationError::new());
        }
        if ctx.returning().is_some() {
            return Err(ValidationError::new());
        }

        let ctx1 = ctx.clone();

        // ## Under context of ctx1

        // ## Under context of ctx
        // Every function in our module must be valid
        for func in &self.functions {
            func.validate(ctx, inputs)?;
        }

        // If module start exists, it must be valid
        if let Some(start) = &self.start {
            start.validate(ctx, inputs)?;
        }

        // Every import must have a valid external type
        for import in &self.imports {
            import.validate(ctx, inputs)?;
        }

        // Every export must have a valid external type
        for export in &self.export {
            export.validate(ctx, inputs)?;
        }

        // Memory must not be larger then 1 (can we change this?)
        if ctx.memories().len() > 1 {
            return Err(ValidationError::new());
        }

        // All export names must be unique
        let export_names = self.export.iter().map(|s| &s.name).collect::<HashSet<_>>();
        if export_names.len() != self.export.len() {
            return Err(ValidationError::new());
        }

        Ok(vec![])
    }
}
