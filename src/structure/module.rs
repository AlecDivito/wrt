// use crate::{
//     lex::{
//         ast::{read_u32, Error, Expect, TryGet},
//         Keyword, TokenType,
//     },
//     validation::instruction::{Const, Operation},
// };
// use std::{collections::HashSet, iter::Peekable};

// use crate::{
//     lex::{ast::Parse, Token},
//     validation::{
//         Context, Input, ValidateInstruction, ValidateResult, Validation, ValidationError,
//     },
// };

// use super::types::{
//     FunctionDefinition,
//     FunctionType,
//     Index,
//     Limit,
//     MemoryOpts,
//     MemoryType,
//     OffsetExpr,
//     StartOpts,
//     // RefType,
//     // DataDefinition, ElementDefinition,  GlobalDefinition,
//     // GlobalType, TableDefinition, TableType,
//     TableUse,
//     TypeDefinition,
//     TypeIndex,
//     TypeUse,
//     ValueType,
// };

// pub enum FnType {
//     Index(TypeIndex),
//     Coded(Box<FunctionType>),
// }

// /// The definition of a function
// pub struct Function {
//     // An index to the signature of the function. Can be found in the module.
//     // Accessible through local indices.
//     ty_index: FnType,

//     // Vector of mutable local variables and their types. Index of the first local
//     // is the smallest index not referencing a parameter (ex. if there are 3 parameters
//     // we would use 3 to reference the lowest local)
//     locals: Vec<ValueType>,

//     // A sequence of instructions. When complete the stack should match the function
//     // result type.
//     body: Operation,
// }

// impl Function {
//     pub fn new(ty_index: FnType, locals: Vec<ValueType>, body: Operation) -> Self {
//         Self {
//             ty_index,
//             locals,
//             body,
//         }
//     }
// }

// impl ValidateInstruction for Function {
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("Alec validate insturction validate call is not implemented")
//         // let ty = match &self.ty_index {
//         //     FnType::Index(index) => ctx.get_type(*index)?,
//         //     FnType::Coded(ty) => &*ty,
//         // };
//         // let mut ctx1 = ctx.clone();
//         // // locals set to the sequence [func input args, locals]
//         // // labels set to singular sequence containing only [func result type]
//         // // return set to the [func result type]
//         // let locals = [ty.input().values(), self.locals.clone()].concat();
//         // ctx1.prepare_function_execution(locals, ty);

//         // let output = self.body.validate(&mut ctx1, inputs)?;
//         // if output == ty.output().values() {
//         //     Ok(output)
//         // } else {
//         //     Err(ValidationError::new())
//         // }
//     }
// }

// /// A vector of opaque values of a particular [ReferenceType]. The min size in the
// /// limits of the table type specifies the initial size of that table.
// ///
// /// Initialized through element segments
// // pub struct Table {
// //     ty: TableType,
// // }

// // impl ValidateInstruction for Table {
// //     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
// //         self.ty.validate(ctx, ())?;
// //         Ok(vec![])
// //     }
// // }

// /// Describe memory as their memory type. It's a vector of raw uninterpreted bytes.
// pub struct Memory {
//     // TODO(Alec): This is a temporary measure. Keep the Id on the block. This
//     // will need to be refactored later.
//     id: Option<String>,
//     ty: MemoryType,
// }

// impl Memory {
//     pub fn new(id: Option<String>, limit: Limit) -> Self {
//         Self {
//             id,
//             ty: MemoryType::new(limit),
//         }
//     }
// }

// impl ValidateInstruction for Memory {
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         self.ty.validate(ctx, ())?;
//         Ok(vec![])
//     }
// }

// // Element Mode
// #[derive(Debug, Clone, Default)]
// pub enum ElementMode {
//     // Can copy to a table using `table.init`
//     #[default]
//     Passive,
//     // not available at runtime. Serves to forward-declare references that are
//     // formed in code with instructions like `ref.func`
//     Declarative,
//     // Active element copies it's elements into a table.
//     Active {
//         // the table to use
//         table: TableUse,
//         // constant express defining an offset into the table
//         offset: OffsetExpr,
//     },
// }

// impl std::fmt::Display for ElementMode {
//     fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         todo!()
//     }
// }

// /// Initial contents of a table is uninitialized. Use [Element] to initialize
// /// a subrange of a table from a static vector of elements.
// // pub struct Element {
// //     // Defined reference type
// //     ty: RefType,
// //     // Required to be a list of constant element expressions
// //     inits: Vec<Const>,
// //     // Identify type of element
// //     mode: ElementMode,
// // }

// // impl ValidateInstruction for Element {
// //     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
// //         for init in &self.inits {
// //             let output = init
// //                 .validate(ctx, inputs)?
// //                 .pop()
// //                 .ok_or_else(ValidationError::new)?;
// //             if ValueType::RefType(self.ty) != output {
// //                 return Err(ValidationError::new());
// //             }
// //         }
// //         match &self.mode {
// //             ElementMode::Passive | ElementMode::Declarative => Ok(vec![]),
// //             ElementMode::Active { table, offset } => {
// //                 let _ = ctx.get_table(table.index())?;
// //                 let mut output = offset.validate(ctx, inputs)?;
// //                 output
// //                     .pop()
// //                     .ok_or_else(ValidationError::new)?
// //                     .try_into_num()?
// //                     .try_into_i32()?;
// //                 if !output.is_empty() {
// //                     Err(ValidationError::new())
// //                 } else {
// //                     Ok(vec![])
// //                 }
// //             }
// //         }
// //     }
// // }

// /// Data mode type
// #[derive(Clone)]
// pub enum DataMode {
//     // Can copy memory using `memory.init`
//     Passive,
//     // Copy contents into a memory during instantiation, specified by the
//     // memory index.
//     Active {
//         memory: Index,
//         // Expression must be a constant expression that defines an offset into memory.
//         offset: Const,
//     },
// }

// impl Default for DataMode {
//     fn default() -> Self {
//         Self::Passive
//     }
// }

// /// Can be used to initialize a range of memory from a static vector of types.
// pub struct Data {
//     // Data to copy into memory
//     init: Vec<u8>,
//     // mode of how to copy
//     mode: DataMode,
// }

// impl ValidateInstruction for Data {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         match &self.mode {
//             DataMode::Passive => Ok(vec![]),
//             DataMode::Active { memory, offset } => {
//                 let _ = ctx.get_memory(Some(memory))?;
//                 let mut output = offset.validate(ctx, inputs)?;
//                 output
//                     .pop()
//                     .ok_or_else(ValidationError::new)?
//                     .try_into_num()?
//                     .try_into_i32()?;
//                 if !output.is_empty() {
//                     Err(ValidationError::new())
//                 } else {
//                     Ok(vec![])
//                 }
//             }
//         }
//     }
// }

// #[derive(Clone)]
// pub struct StartFunction(u32);
// impl ValidateInstruction for StartFunction {
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         let ty = ctx.get_function(&Index::Index(self.0))?;
//         if ty.input().values().is_empty() && ty.output().values().is_empty() {
//             Ok(vec![])
//         } else {
//             Err(ValidationError::new())
//         }
//     }
// }

// /// The index to the implementation of the type of export required.
// #[derive(Debug, Clone)]
// pub enum ExportDescription {
//     Func(Index),
//     Table(Index),
//     Memory(Index),
//     Global(Index),
// }

// impl ValidateInstruction for ExportDescription {
//     fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         match self {
//             ExportDescription::Func(index) => {
//                 ctx.get_function(index)?.validate(ctx, ())?;
//             }
//             ExportDescription::Table(index) => {
//                 ctx.get_table(index)?.validate(ctx, ())?;
//             }
//             ExportDescription::Memory(index) => {
//                 ctx.get_memory(Some(index))?.validate(ctx, ())?;
//             }
//             ExportDescription::Global(index) => {
//                 ctx.get_global(index)?.validate(ctx, ())?;
//             }
//         };
//         Ok(vec![])
//     }
// }

// impl std::fmt::Display for ExportDescription {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ExportDescription::Func(idx) => write!(f, "(func {})", idx),
//             ExportDescription::Table(idx) => write!(f, "(table {})", idx),
//             ExportDescription::Memory(idx) => write!(f, "(memory {})", idx),
//             ExportDescription::Global(idx) => write!(f, "(global {})", idx),
//         }
//     }
// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ExportDescription {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         let keyword = tokens.next().expect_keyword()?;
//         let id = Index::parse(tokens)?;
//         let description = match keyword {
//             Keyword::Func => ExportDescription::Func(id),
//             Keyword::Table => ExportDescription::Table(id),
//             Keyword::Memory => ExportDescription::Memory(id),
//             Keyword::Global => ExportDescription::Global(id),
//             keyword => {
//                 return Err(Error::new(
//                     tokens.next().cloned(),
//                     format!(
//                         "Expected Export Description but found {:?} instead",
//                         keyword
//                     ),
//                 ))
//             }
//         };
//         tokens.next().expect_right_paren()?;
//         Ok(description)
//     }
// }

// /// Export functionality to be accessed by the host environment. Only accessible
// /// after module has been instantiated.
// pub struct Export {
//     // Name is unique
//     name: String,
//     description: ExportDescription,
// }

// impl Export {
//     pub fn new(name: String, description: ExportDescription) -> Self {
//         Self { name, description }
//     }
// }

// impl ValidateInstruction for Export {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         self.description.validate(ctx, inputs)
//     }
// }

// impl std::fmt::Display for Export {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "(export \"{}\" {})", self.name, self.description)
//     }
// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Export {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         tokens.next().expect_keyword_token(Keyword::Export)?;
//         let name = tokens.next().expect_string()?;
//         let description = ExportDescription::parse(tokens)?;
//         tokens.next().expect_right_paren()?;
//         Ok(Self { name, description })
//     }
// }

// /// The index of the expected type that definition should provide and match during
// /// instantiation.
// #[derive(Debug, Clone)]
// pub struct ImportDescription {
//     id: Option<String>,
//     ty: ImportDescriptionType,
// }

// #[derive(Debug, Clone)]
// enum ImportDescriptionType {
//     Func(TypeUse),
//     // Table(TableType),
//     Memory(MemoryType),
//     // Global(GlobalType),
// }

// impl ValidateInstruction for ImportDescription {
//     fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         todo!("help");
//         // match self.ty {
//         //     ImportDescriptionType::Func(index) => {
//         //         ctx.get_function(&Index::Index(*index))?.validate(ctx, ())?;
//         //     }
//         //     ImportDescriptionType::Table(index) => {
//         //         ctx.get_table(&Index::Index(*index))?.validate(ctx, ())?;
//         //     }
//         //     ImportDescriptionType::Memory(index) => {
//         //         ctx.get_memory(Some(&Index::Index(*index)))?
//         //             .validate(ctx, ())?;
//         //     }
//         //     ImportDescriptionType::Global(index) => {
//         //         ctx.get_global(&Index::Index(*index))?.validate(ctx, ())?;
//         //     }
//         // };
//     }
// }

// impl std::fmt::Display for ImportDescription {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let id = match self.id.as_ref() {
//             Some(id) => format!(" ${} ", id),
//             None => " ".to_string(),
//         };
//         match &self.ty {
//             ImportDescriptionType::Func(item) => write!(f, "(func{}{})", id, item),
//             // ImportDescriptionType::Table(item) => write!(f, "(table{}{})", id, item),
//             ImportDescriptionType::Memory(item) => write!(f, "(memory{}{})", id, item),
//             // ImportDescriptionType::Global(item) => write!(f, "(global{}{})", id, item),
//         }
//     }
// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ImportDescription {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         let keyword = tokens.next().expect_keyword()?;
//         let id = get_id(tokens);

//         let description = match keyword {
//             Keyword::Func => ImportDescriptionType::Func(TypeUse::parse(tokens)?),
//             // Keyword::Table => ImportDescriptionType::Table(TableType::parse(tokens)?),
//             Keyword::Memory => ImportDescriptionType::Memory(MemoryType::parse(tokens)?),
//             // Keyword::Global => ImportDescriptionType::Global(GlobalType::parse(tokens)?),
//             value => {
//                 return Err(Error::new(
//                     tokens.next().cloned(),
//                     format!(
//                         "Failed to parse Import Description. Did not expect keyword {:?}",
//                         value
//                     ),
//                 ))
//             }
//         };

//         tokens.next().expect_right_paren()?;
//         Ok(Self {
//             id,
//             ty: description,
//         })
//     }
// }

// /// Imports required for instantiation
// ///
// /// NOTE: Possible to import the same "module.name" pair. Depends on how a embedded
// /// resolves imports with the same name.
// pub struct Import {
//     // name of module to import from
//     module: String,
//     // name of entity within the above module
//     // NOTE: Name may not be unique.
//     name: String,
//     // The definition to import
//     description: ImportDescription,
// }

// impl ValidateInstruction for Import {
//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         self.description.validate(ctx, inputs)
//     }
// }

// impl std::fmt::Display for Import {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "(import \"{}\" \"{}\" {})",
//             self.module, self.name, self.description
//         )
//     }
// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Import {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::lex::ast::Error> {
//         tokens.next().expect_left_paren()?;
//         tokens.next().expect_keyword_token(Keyword::Import)?;
//         let module = tokens.next().expect_string()?;
//         let name = tokens.next().expect_string()?;
//         let description = ImportDescription::parse(tokens)?;
//         tokens.next().expect_right_paren()?;
//         Ok(Import {
//             module,
//             name,
//             description,
//         })
//     }
// }

// #[derive(Default)]
// pub struct IDCtx {
//     funcs: Vec<Option<String>>,
// }

// #[derive(Default)]
// pub struct Module {
//     id: Option<String>,

//     id_ctx: IDCtx,

//     // Types definitions inside of the web assembly
//     types: Vec<TypeDefinition>,

//     // Functions inside of the module
//     functions: Vec<FunctionDefinition>,

//     // Select tables through [TableIndex]. Starts at 0. Implicitly reference 0.
//     // tables: Vec<TableDefinition>,

//     // Select memory through [MemoryIndex]. Starts at 0. Implicitly reference 0.
//     memories: Vec<MemoryOpts>,

//     // Referenced through [GlobalIndex]. Start with smallest index not referencing a global import.
//     // globals: Vec<GlobalDefinition>,

//     // Referenced through [ElementIndex]
//     // elements: Vec<ElementDefinition>,

//     // Referenced through [DataIndex]
//     // datas: Vec<DataDefinition>,

//     // List of imports that are required for the module to work
//     imports: Vec<Import>,

//     // Every import defines an index in the respective index space. In each index space
//     // the indices of imports go before the first index of any definition contained
//     // in the module itself.
//     export: Vec<Export>,

//     // Automatically invoked function when module is instantiated, after Tables
//     // and memories have been initialized.
//     //
//     // NOTE: Intended for use to initialize the state of a module.
//     start: Option<StartOpts>,
// }

// impl std::fmt::Display for Module {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         writeln!(f, "(module")?;
//         for ty in &self.types {
//             f.pad(&" ".repeat(2))?;
//             writeln!(f, "{}", ty)?;
//         }

//         for import in self.imports.iter() {
//             f.pad(&" ".repeat(2))?;
//             writeln!(f, "{}", import)?;
//         }

//         for func in &self.functions {
//             f.pad(&" ".repeat(2))?;
//             writeln!(f, "{}", func)?;
//         }

//         // for table in &self.tables {
//         //     f.pad(&" ".repeat(2))?;
//         //     writeln!(f, "{}", table)?;
//         // }

//         for memory in &self.memories {
//             f.pad(&" ".repeat(2))?;
//             writeln!(f, "{}", memory)?;
//         }

//         // for global in &self.globals {
//         //     f.pad(&" ".repeat(2))?;
//         //     writeln!(f, "{}", global)?;
//         // }

//         for export in &self.export {
//             f.pad(&" ".repeat(2))?;
//             writeln!(f, "{}", export)?;
//         }

//         if let Some(start) = self.start.as_ref() {
//             f.pad(&" ".repeat(2))?;
//             writeln!(f, "{}", start)?;
//         }

//         // for elem in &self.elements {
//         //     f.pad(&" ".repeat(2))?;
//         //     writeln!(f, "{}", elem)?;
//         // }

//         // for data in &self.datas {
//         //     f.pad(&" ".repeat(2))?;
//         //     writeln!(f, "{}", data)?;
//         // }

//         // ending
//         f.pad("")?;
//         writeln!(f, ")")?;

//         Ok(())
//     }
// }

// pub fn get_id<'a, I: Iterator<Item = &'a Token>>(iter: &mut Peekable<I>) -> Option<String> {
//     iter.next_if(|t| t.ty() == &TokenType::Id)
//         .map(|token| token.source().to_string())
// }

// /// Get an index or fail
// pub fn get_index<'a, I: Iterator<Item = &'a Token>>(
//     tokens: &mut Peekable<I>,
// ) -> Result<Index, Error> {
//     match tokens.next_if(|t| t.ty() == &TokenType::Id || t.ty() == &TokenType::Number) {
//         Some(token) if token.ty() == &TokenType::Id => Ok(Index::Id(token.source().to_string())),
//         Some(token) if token.ty() == &TokenType::Number => Ok(Index::Index(read_u32(token)?)),
//         _ => Err(Error::new(tokens.next().cloned(), "Expected number or id.")),
//     }
// }

// pub fn get_next_keyword<'a, I: Iterator<Item = &'a Token> + Clone>(
//     tokens: &mut Peekable<I>,
// ) -> Option<Keyword> {
//     tokens.peek().copied().expect_left_paren().ok()?;
//     let keyword = tokens.clone().nth(1).expect_keyword().ok()?;
//     Some(keyword)
// }

// impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Module {
//     fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
//         let mut this = Module::default();
//         tokens.next().expect_left_paren()?;
//         tokens.next().expect_keyword_token(Keyword::Module)?;
//         this.id = get_id(tokens);

//         loop {
//             if tokens.peek().copied().try_right_paran().is_some() {
//                 break;
//             }
//             match tokens.clone().nth(1).expect_keyword()? {
//                 Keyword::Type => this.types.push(TypeDefinition::parse(tokens)?),
//                 Keyword::Func => this.functions.push(FunctionDefinition::parse(tokens)?),
//                 // Keyword::Table => this.tables.push(TableDefinition::parse(tokens)?),
//                 Keyword::Memory => this.memories.push(MemoryOpts::parse(tokens)?),
//                 // Keyword::Global => this.globals.push(GlobalDefinition::parse(tokens)?),
//                 // Keyword::Elem => this.elements.push(ElementDefinition::parse(tokens)?),
//                 // Keyword::Data => this.datas.push(DataDefinition::parse(tokens)?),
//                 Keyword::Import => this.imports.push(Import::parse(tokens)?),
//                 Keyword::Export => this.export.push(Export::parse(tokens)?),
//                 Keyword::Start if this.start.is_none() => {
//                     // TODO(Alec): This maybe a feature we want for our own modules.
//                     // Having multiple start functions just means we'll call them
//                     // as we see them.
//                     this.start = Some(StartOpts::parse(tokens)?)
//                 }
//                 keyword => {
//                     tokens.next().expect_left_paren()?;
//                     return Err(Error::new(
//                         tokens.next().cloned(),
//                         format!(
//                             "Expected top level module import. Got {:?} instead.",
//                             keyword
//                         ),
//                     ));
//                 }
//             }
//         }
//         tokens.next().expect_right_paren()?;

//         Ok(this)
//     }
// }

// fn order_lists<I, O, FI, FD>(
//     list: &[I],
//     get_index: FI,
//     get_data: FD,
// ) -> Result<Vec<O>, ValidationError>
// where
//     FI: Fn(&I) -> u32,
//     FD: Fn(&I) -> Result<O, ValidationError>,
//     O: Clone,
// {
//     let mut new_list: Vec<(O, u32)> = vec![];
//     for item in list.iter() {
//         let index = get_index(item);
//         let data = get_data(item)?;
//         new_list.push((data, index));
//     }
//     new_list.sort_by_key(|tuple| tuple.1);
//     Ok(new_list.iter().map(|tuple| tuple.0.clone()).collect::<_>())
// }

// impl ValidateInstruction for Module {
//     // TODO(Alec): This module validation is confusing and there are a lot of checks
//     // to validate. I think coming back to this when we are closer done the program
//     // will give me a better understanding of what needs to be done.
//     // https://webassembly.github.io/spec/core/valid/modules.html#valid-module

//     fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
//         // Build up context
//         for ty in &self.types {
//             ctx.push_type(ty);
//         }

//         // C.funcs import function types concataed with function types
//         // for import in self.imports {
//         //     match import.description {
//         //         ImportDescription::Func(func) => todo!(),
//         //         ImportDescription::Table(_) => todo!(),
//         //         ImportDescription::Memory(_) => todo!(),
//         //         ImportDescription::Global(_) => todo!(),
//         //     }
//         // }

//         for func in &self.functions {
//             ctx.push_function(func)?;
//         }

//         // internal function types, in index order
//         let ctx_ref = &ctx;

//         // let ft = order_lists(
//         //     &self.functions,
//         //     |f| f.ty_index,
//         //     move |f| ctx_ref.get_type(f.ty_index),
//         // )?;
//         // Internal table types, in index order
//         // let tt = &self.tables.iter().map(|t| &t.ty).collect::<Vec<_>>();
//         // Internal memory types, in index order
//         // let mt = &self.memories.iter().map(|t| &t.ty).collect::<Vec<_>>();
//         // Internal Global types, in index order
//         // let gt = &self.globals.iter().map(|t| &t.ty).collect::<Vec<_>>();
//         // reference types, in index order
//         // let rt = self.ref

//         // validate types are equal
//         // if ctx.types() != &self.types {
//         //     return Err(ValidationError::new());
//         // }
//         // validate functions are equal
//         // let external_functions = ctx.function_exports()
//         // let ft_star = ctx.functions();
//         // let functions = [self.functions, ]
//         // ctx.functions() == self.functions

//         // Validate all values are ok
//         // if !ctx.datas().iter().all(crate::validation::Ok::is_ok) {
//         //     return Err(ValidationError::new());
//         // }

//         // validate all locals are empty, after all validation has been completed.
//         // if !ctx.locals().is_empty() {
//         //     return Err(ValidationError::new());
//         // }
//         // if !ctx.labels().is_empty() {
//         //     return Err(ValidationError::new());
//         // }
//         if ctx.returning().is_some() {
//             return Err(ValidationError::new());
//         }

//         let ctx1 = ctx.clone();

//         // ## Under context of ctx1

//         // ## Under context of ctx
//         // Every function in our module must be valid
//         for func in &self.functions {
//             func.validate(ctx, inputs)?;
//         }

//         // If module start exists, it must be valid
//         // if let Some(start) = &self.start {
//         //     start.validate(ctx, inputs)?;
//         // }

//         // Every import must have a valid external type
//         for import in &self.imports {
//             import.validate(ctx, inputs)?;
//         }

//         // Every export must have a valid external type
//         for export in &self.export {
//             export.validate(ctx, inputs)?;
//         }

//         // Memory must not be larger then 1 (can we change this?)
//         if self.memories.len() > 1 {
//             return Err(ValidationError::multiple_memory(format!(
//                 "Found {} memories. There should only exist 1",
//                 self.memories.len()
//             )));
//         }

//         // All export names must be unique
//         let export_names = self.export.iter().map(|s| &s.name).collect::<HashSet<_>>();
//         if export_names.len() != self.export.len() {
//             return Err(ValidationError::new());
//         }

//         Ok(vec![])
//     }
// }
