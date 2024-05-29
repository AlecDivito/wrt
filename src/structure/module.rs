use crate::parse::{
    ast::{Error, Expect, IntoResult, TryGet},
    Keyword, TokenType,
};
use std::{collections::HashSet, convert::TryInto, iter::Peekable, ops::Deref};

use crate::{
    parse::{ast::Parse, Token},
    validation::{
        ConstantExpression, Context, Expression, Input, ValidateInstruction, ValidateResult,
        Validation, ValidationError,
    },
};

use super::{
    types::{
        BlockInstruction, FuncParam, FuncResult, FuncType, FunctionIndex, FunctionType,
        GlobalIndex, GlobalType, Limit, MemoryIndex, MemoryType, RefType, RelativeExport,
        RelativeImport, ResultType, TableIndex, TableType, TypeIndex, ValueType,
    },
    util::IndexedVec,
};

pub enum FnType {
    Index(TypeIndex),
    Coded(Box<FunctionType>),
}

/// The definition of a function
pub struct Function {
    // An index to the signature of the function. Can be found in the module.
    // Accessible through local indices.
    ty_index: FnType,

    // Vector of mutable local variables and their types. Index of the first local
    // is the smallest index not referencing a parameter (ex. if there are 3 parameters
    // we would use 3 to reference the lowest local)
    locals: Vec<ValueType>,

    // A sequence of instructions. When complete the stack should match the function
    // result type.
    body: Expression,
}

impl Function {
    pub fn new(ty_index: FnType, locals: Vec<ValueType>, body: Expression) -> Self {
        Self {
            ty_index,
            locals,
            body,
        }
    }
}

impl ValidateInstruction for Function {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        let ty = match &self.ty_index {
            FnType::Index(index) => ctx.get_type(*index)?,
            FnType::Coded(ty) => &*ty,
        };
        let mut ctx1 = ctx.clone();
        // locals set to the sequence [func input args, locals]
        // labels set to singular sequence containing only [func result type]
        // return set to the [func result type]
        let locals = [ty.input().values(), &self.locals].concat();
        ctx1.prepare_function_execution(locals, ty);

        let output = self.body.validate(&mut ctx1, inputs)?;
        if output == *ty.output().values() {
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
    id: Option<String>,
    ty: GlobalType,
    // Requirement, must be a constant initializer expression.
    init: ConstantExpression,
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Global {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Global)?;
        let id = get_id(tokens);
        let ty = GlobalType::parse(tokens)?;
        let init = ConstantExpression::parse(tokens)?;
        tokens.next().expect_right_paren()?;
        Ok(Self { id, ty, init })
    }
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
        if ty.input().values().is_empty() && ty.output().values().is_empty() {
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

impl Export {
    pub fn new(name: String, description: ExportDescription) -> Self {
        Self { name, description }
    }
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
pub struct IDCtx {
    funcs: Vec<Option<String>>,
}

#[derive(Default)]
pub struct Module {
    id_ctx: IDCtx,

    // Types definitions inside of the web assembly
    types: IndexedVec<FunctionType>,

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

pub fn get_id<'a, I: Iterator<Item = &'a Token>>(iter: &mut Peekable<I>) -> Option<String> {
    iter.next_if(|t| t.ty() == &TokenType::Id)
        .map(|token| token.source().to_string())
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Module {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
        let mut this = Module::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Module)?;

        loop {
            tokens.peek().copied().expect_left_paren()?;
            match tokens.clone().nth(1).expect_keyword()? {
                Keyword::Type => this.types.push_tuple(FuncType::parse(tokens)?.into_parts()),
                Keyword::Func => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Func)?;
                    this.id_ctx.funcs.push(get_id(tokens));
                    let mut params = ResultType::new(vec![]);
                    let mut result = ResultType::new(vec![]);
                    let mut locals = vec![];
                    let mut import = None;
                    let mut export = None;
                    let mut func_ty = None;
                    let mut instructions = vec![];

                    // Get the function definition
                    loop {
                        if tokens.peek().copied().try_right_paran().is_some() {
                            break;
                        }

                        tokens.peek().copied().expect_left_paren()?;
                        match tokens.clone().nth(1).expect_keyword()? {
                            Keyword::Type => {
                                if func_ty.is_some() {
                                    return Err(Error::new(None, "multiple 'type' blocks on func"));
                                }
                                let (id, ty) = FuncType::parse(tokens)?.into_parts();
                                match id {
                                    Some(id) if ty.is_empty() => {
                                        func_ty = Some(this.types.get(&id).ok_or_else(|| {
                                            Error::new(
                                                None,
                                                format!("func type id {id} does not exist"),
                                            )
                                        })?);
                                    }
                                    Some(id) => {
                                        return Err(Error::new(
                                            None,
                                            format!(
                                            "func type id {id} defines types. thats not allowed"
                                        ),
                                        ))
                                    }
                                    None => {
                                        return Err(Error::new(
                                            None,
                                            "func type was not associated with any id",
                                        ))
                                    }
                                }
                            }
                            Keyword::Export => match &export {
                                Some(_) => {
                                    return Err(Error::new(
                                        None,
                                        "multiple 'export' blocks on func",
                                    ))
                                }
                                None => {
                                    export = Some(RelativeExport::parse(tokens)?);
                                }
                            },
                            Keyword::Import => match &import {
                                Some(_) => {
                                    return Err(Error::new(
                                        None,
                                        "multiple 'import' blocks on func",
                                    ))
                                }
                                None => {
                                    import = Some(RelativeImport::parse(tokens)?);
                                }
                            },
                            Keyword::Param => params.extend(FuncParam::parse(tokens)?.0),
                            Keyword::Result => result.extend(FuncResult::parse(tokens)?.0),
                            _ => break,
                        }
                    }
                    // Get all locals
                    loop {
                        if tokens.peek().copied().try_right_paran().is_some() {
                            break;
                        }
                        tokens.peek().copied().expect_left_paren()?;
                        match tokens.clone().nth(1).expect_keyword()? {
                            Keyword::Local => {
                                tokens.next().expect_left_paren()?;
                                tokens.next().expect_keyword_token(Keyword::Local)?;
                                locals.push(ValueType::parse(tokens)?)
                            }
                            _ => break,
                        }
                        tokens.next().expect_right_paren()?;
                    }
                    // Get the instructions for the function
                    loop {
                        if tokens.peek().copied().try_right_paran().is_some() {
                            break;
                        }
                        tokens.peek().copied().expect_left_paren()?;
                        match tokens.clone().nth(1).expect_keyword()? {
                            // Block Instruction
                            Keyword::Block => {
                                // BlockInstruction::parse(tokens)?;
                            }
                            Keyword::Loop => {}
                            Keyword::If => {}

                            // Plain Instruction
                            Keyword::Unreachable => {}
                            Keyword::Nop => {}
                            Keyword::Br => {}
                            Keyword::BrIf => {}
                            Keyword::BrTable => {}
                            Keyword::Return => {}
                            Keyword::Call => {}
                            Keyword::CallIndirect => {}

                            _ => todo!("hello"),
                        }
                        tokens.next().expect_right_paren()?;
                    }

                    // Function loop completed. Add everything parsed to the module
                    let ty = match func_ty {
                        Some(ty) if params.is_empty() && result.is_empty() => ty.clone(),
                        Some(ty)
                            if ty.input().values() == params.values()
                                && ty.output().values() == result.values() =>
                        {
                            ty.clone()
                        }
                        None => FunctionType::new(params, result),
                        Some(ty) => {
                            return Err(Error::new(
                                None,
                                "function type and parameters do not match",
                            ))
                        }
                    };

                    let expression = Expression::new(instructions);
                    // this.types.push(None, FunctionType::new(params, result));
                    // let ty_index = (this.types.len() - 1).try_into().unwrap();
                    this.functions.push(Function::new(
                        FnType::Coded(Box::new(ty)),
                        locals,
                        expression,
                    ));
                    if let Some(name) = export {
                        let index = (this.functions.len() - 1).try_into().unwrap();
                        this.export
                            .push(Export::new(name.name, ExportDescription::Func(index)))
                    }
                }
                Keyword::Table => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Table)?;
                    let id = get_id(tokens);
                }
                Keyword::Memory => {
                    todo!()
                } // this.memories.push(Memory::parse(tokens)?),
                Keyword::Global => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Global)?;
                    let id = get_id(tokens);
                }
                Keyword::Elem => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Elem)?;
                }
                Keyword::Data => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Data)?;
                }
                Keyword::Import => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Import)?;
                }
                Keyword::Export => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Export)?;
                }
                Keyword::Start => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(Keyword::Start)?;
                }
                keyword => {
                    tokens.next().expect_left_paren()?;
                    return Err(Error::new(
                        tokens.next().cloned(),
                        format!(
                            "Expected top level module import. Got {:?} instead.",
                            keyword
                        ),
                    ));
                }
            }
            if tokens.peek().copied().try_right_paran().is_some() {
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
        // let ft = order_lists(
        //     &self.functions,
        //     |f| f.ty_index,
        //     move |f| ctx_ref.get_type(f.ty_index),
        // )?;
        // Internal table types, in index order
        let tt = &self.tables.iter().map(|t| &t.ty).collect::<Vec<_>>();
        // Internal memory types, in index order
        let mt = &self.memories.iter().map(|t| &t.ty).collect::<Vec<_>>();
        // Internal Global types, in index order
        let gt = &self.globals.iter().map(|t| &t.ty).collect::<Vec<_>>();
        // reference types, in index order
        // let rt = self.ref

        // validate types are equal
        // if ctx.types() != &self.types {
        //     return Err(ValidationError::new());
        // }
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
        if self.memories.len() > 1 {
            return Err(ValidationError::multiple_memory(format!(
                "Found {} memories. There should only exist 1",
                self.memories.len()
            )));
        }

        // All export names must be unique
        let export_names = self.export.iter().map(|s| &s.name).collect::<HashSet<_>>();
        if export_names.len() != self.export.len() {
            return Err(ValidationError::new());
        }

        Ok(vec![])
    }
}
