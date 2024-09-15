use std::iter::Peekable;

use crate::{
    execution::{ModuleInstance, Store, PAGE_SIZE},
    parse::{
        ast::{read_u32, write_optional, Error, Expect, Parse, TryGet},
        tokenize, Keyword, Token, TokenType,
    },
    validation::{
        instruction::{Const, Execute, Operation, RefFunc},
        Context, Input, ValidateInstruction, ValidateResult, Validation, ValidationError,
    },
};

use super::module::{get_id, get_index, get_next_keyword, DataMode, ElementMode, Module};

/// Type of sign an integer is meant to taken as
///
/// Some integer instructions come in two flavors, where a signedness distinguishes
/// whether the operands are to be interpreted as signed or unsigned. Use two compliment
/// for signed interpretation means that they behave the same regardless of signedness
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignType {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HalfType {
    Low,
    High,
}

#[derive(Clone, Debug)]
pub enum BlockType {
    Index(Index),
    Value(Vec<Variable>), // return variables
}
impl std::fmt::Display for BlockType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockType::Index(obj) => write!(f, "(type {})", obj),
            BlockType::Value(onj) => {
                let results = onj
                    .iter()
                    .map(|v| v.ty().to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "(result {})", results)
            }
        }
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for BlockType {
    fn parse(tokens: &mut std::iter::Peekable<I>) -> Result<Self, Error> {
        match get_next_keyword(tokens) {
            Some(Keyword::Type) => Ok(BlockType::Index(TypeUse::parse(tokens)?.try_into_index()?)),
            Some(Keyword::Result) => {
                let mut results = vec![];
                while Some(Keyword::Result) == get_next_keyword(tokens) {
                    results.extend(FuncResult::parse(tokens)?.0);
                }
                Ok(BlockType::Value(results))
            }
            Some(keyword) => Err(Error::new(
                tokens.nth(1).cloned(),
                format!(
                    "keyword {:?} was not expected for 'block' return type",
                    keyword
                ),
            )),
            None => Err(Error::new(
                tokens.next().cloned(),
                "keyword was not found for 'block' return type".to_string(),
            )),
        }
    }
}
impl BlockType {
    pub fn get_function_type(&self, ctx: &Context) -> Result<FunctionType, ValidationError> {
        match self {
            BlockType::Index(index) => ctx.get_type(index).cloned(),
            BlockType::Value(value) => Ok(FunctionType::new(
                ResultType::new(vec![]),
                ResultType::new(value.clone()),
            )),
        }
    }
}
impl Validation<FunctionType> for BlockType {
    fn validate(&self, ctx: &Context, _: FunctionType) -> Result<(), ValidationError> {
        match self {
            BlockType::Index(index) => ctx.get_type(index).map(|_| ()),
            // This one is always ok, because the type is defined on the block type
            // already meaning that if we have parsed it, it's valid...
            // short hand for function type [] -> [ValueType?]
            BlockType::Value(_) => Ok(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    id: Option<String>,
    ty: ValueType,
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.id {
            Some(id) => write!(f, "${} {}", id, self.ty),
            None => write!(f, "{}", self.ty),
        }
    }
}

impl Variable {
    pub fn new(id: String, ty: ValueType) -> Self {
        Self { id: Some(id), ty }
    }

    pub fn unset(ty: ValueType) -> Self {
        Self { id: None, ty }
    }

    pub fn ty(&self) -> &ValueType {
        &self.ty
    }

    pub fn id(&self) -> Option<&String> {
        self.id.as_ref()
    }
}

#[derive(Debug, Clone)]
pub enum Index {
    Id(String),
    Index(u32),
}
impl std::fmt::Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Index::Id(id) => write!(f, "${}", id),
            Index::Index(idx) => write!(f, "{}", idx),
        }
    }
}

impl Default for Index {
    fn default() -> Self {
        Index::Index(0)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Index {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let index = match get_id(tokens) {
            Some(id) => Index::Id(id),
            None => Index::Index(read_u32(tokens.next().expect_number()?)?),
        };
        Ok(index)
    }
}

pub type TypeIndex = u32;
pub type FunctionIndex = u32;
pub type TableIndex = u32;
pub type MemoryIndex = u32;
pub type GlobalIndex = u32;
pub type ElementIndex = u32;
pub type DataIndex = u32;
pub type LocalIndex = u32;
pub type LabelIndex = u32;

/// Classify Imports and external values with their respective types
pub enum ExternalType {
    Func(FunctionType),
    Table(TableType),
    Mem(MemoryType),
    Global(GlobalType),
}

impl Validation<()> for ExternalType {
    fn validate(&self, ctx: &Context, args: ()) -> Result<(), ValidationError> {
        match self {
            ExternalType::Func(inner) => inner.validate(ctx, args),
            ExternalType::Table(inner) => inner.validate(ctx, args),
            ExternalType::Mem(inner) => inner.validate(ctx, args),
            ExternalType::Global(inner) => inner.validate(ctx, args),
        }
    }
}

/// [GlobalType] hold a global value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalType {
    // Immutiable global value
    Const(ValueType),
    // Mutable global value
    Var(ValueType),
}
impl Default for GlobalType {
    fn default() -> Self {
        Self::Const(ValueType::RefType(RefType::default()))
    }
}
impl std::fmt::Display for GlobalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalType::Const(value) => write!(f, "{}", value),
            GlobalType::Var(var) => write!(f, "(mut {})", var),
        }
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for GlobalType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        Ok(if tokens.peek().copied().try_left_paran().is_some() {
            tokens.next().expect_left_paren()?;
            tokens.next().expect_keyword_token(Keyword::Mut)?;
            let var = Self::Var(ValueType::parse(tokens)?);
            tokens.next().expect_right_paren()?;
            var
        } else {
            Self::Const(ValueType::parse(tokens)?)
        })
    }
}

impl Validation<()> for GlobalType {
    fn validate(&self, _: &Context, _: ()) -> Result<(), ValidationError> {
        Ok(())
    }
}

impl Validation<ValueType> for GlobalType {
    fn validate(&self, _: &Context, args: ValueType) -> Result<(), ValidationError> {
        let value = match self {
            GlobalType::Const(value) => value,
            GlobalType::Var(value) => value,
        };
        if *value == args {
            Ok(())
        } else {
            Err(ValidationError::new())
        }
    }
}

impl Validation<GlobalType> for GlobalType {
    fn validate(&self, _: &Context, g2: GlobalType) -> Result<(), ValidationError> {
        if *self == g2 {
            Ok(())
        } else {
            Err(ValidationError::new())
        }
    }
}

/// Classify ["Table"] over elements of ["RefType"] within the size range.
///
/// Note: The type must contain only the one type of RefType... (maybe)
/// TODO(Alec): Validate...
///
/// Like ["Memory"] tables have a size limit.
#[derive(Debug, Clone, Default)]
pub struct TableType {
    limit: Limit,
    ref_type: RefType,
}

impl TableType {
    pub fn ref_type(&self) -> &RefType {
        &self.ref_type
    }
}

impl std::fmt::Display for TableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.limit, self.ref_type)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
        let limit = Limit::parse(tokens)?;
        let ref_type = RefType::parse(tokens)?;
        Ok(Self { limit, ref_type })
    }
}

impl Validation<()> for TableType {
    fn validate(&self, ctx: &Context, _: ()) -> Result<(), ValidationError> {
        let max = 2_u32.pow(32) - 1;
        self.limit.validate(ctx, max)?;
        // Otherwise, reftype is always ok.
        Ok(())
    }
}

impl Validation<TableType> for TableType {
    fn validate(&self, ctx: &Context, t2: TableType) -> Result<(), ValidationError> {
        if self.ref_type == t2.ref_type {
            self.limit.validate(ctx, t2.limit)
        } else {
            Err(ValidationError::new())
        }
    }
}

pub enum MemoryLoadNumber {
    Load8,
    Load16,
    Load32,
}

impl MemoryLoadNumber {
    pub fn bit_width(&self) -> u32 {
        match self {
            MemoryLoadNumber::Load8 => 8,
            MemoryLoadNumber::Load16 => 16,
            MemoryLoadNumber::Load32 => 32,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryWidth {
    I8,
    I16,
    I32,
    I64,
}

impl MemoryWidth {
    pub fn bit_width(&self) -> u32 {
        match self {
            MemoryWidth::I8 => 8,
            MemoryWidth::I16 => 16,
            MemoryWidth::I32 => 32,
            MemoryWidth::I64 => 64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryZeroWidth {
    I32,
    I64,
}

impl MemoryZeroWidth {
    pub fn bit_width(&self) -> u32 {
        match self {
            MemoryZeroWidth::I32 => 32,
            MemoryZeroWidth::I64 => 64,
        }
    }
}

pub struct MemoryArgument {
    offset: u32,
    align: u32,
}

impl MemoryArgument {
    pub fn align(&self) -> u32 {
        self.align
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }
}

/// Classify linear ['Memories'] and their size range.
///
/// Contains the min and max of memory size, given in units of Page Size.
#[derive(Debug, Clone, Default)]
pub struct MemoryType {
    limit: Limit,
}

impl MemoryType {
    pub fn new(limit: Limit) -> Self {
        Self { limit }
    }
}

impl std::fmt::Display for MemoryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.limit)
    }
}

impl Validation<()> for MemoryType {
    fn validate(&self, ctx: &Context, _: ()) -> Result<(), ValidationError> {
        let max = 2_u32.pow(16);
        self.limit.validate(ctx, max)
    }
}

impl Validation<MemoryType> for MemoryType {
    fn validate(&self, ctx: &Context, args: MemoryType) -> Result<(), ValidationError> {
        self.limit.validate(ctx, args.limit)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for MemoryType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        Ok(Self {
            limit: Limit::parse(tokens)?,
        })
    }
}

/// ["Limit"] size range of resizable storage. Associated with ["Memory"] and
/// ["Table"] types. Max is optional.
#[derive(Debug, Clone, Default)]
pub struct Limit {
    min: u32,
    max: Option<u32>,
}

impl Limit {
    pub fn new(min: u32, max: Option<u32>) -> Self {
        Self { min, max }
    }
}

impl std::fmt::Display for Limit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.min)?;
        if let Some(max) = &self.max {
            write!(f, " {}", max)?;
        }
        Ok(())
    }
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for Limit {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let min = read_u32(tokens.next().expect_number()?)?;
        let max = if tokens.peek().copied().expect_number().is_ok() {
            Some(read_u32(tokens.next().expect_number()?)?)
        } else {
            None
        };
        Ok(Limit { min, max })
    }
}

impl Validation<u32> for Limit {
    /// Validate the limit is within range of K.
    fn validate(&self, _ctx: &Context, k: u32) -> Result<(), ValidationError> {
        if self.min > k {
            Err(ValidationError::new())
        } else if let Some(max) = self.max.as_ref() {
            if *max > k || self.min > *max {
                Err(ValidationError::new())
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

impl Validation<Limit> for Limit {
    fn validate(&self, _: &Context, args: Limit) -> Result<(), ValidationError> {
        let min_invalid = self.min < args.min;
        if let (Some(max1), Some(max2)) = (&self.max, &args.max) {
            if *max1 > *max2 || min_invalid {
                Err(ValidationError::new())
            } else {
                Ok(())
            }
        } else if min_invalid {
            // https://webassembly.github.io/spec/core/valid/types.html#import-subtyping
            // Imported limit must less then or equal to current limit
            Err(ValidationError::new())
        } else {
            Ok(())
        }
        // https://webassembly.github.io/spec/core/valid/types.html
    }
}

/// ["FunctionType"] is a classification signature of a function. It maps a vector
/// of result types (as parameters) to return types (as the return value).
///
/// They are also used to classify the input and outputs of ["Instructions"].
///
/// Can be represented in rust as (Box<dyn Fn(ResultType) -> ResultType>)
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct FunctionType {
    input: ResultType,
    output: ResultType,
}

impl FunctionType {
    pub fn new(input: ResultType, output: ResultType) -> Self {
        Self { input, output }
    }

    pub fn empty() -> Self {
        Self {
            input: ResultType { values: vec![] },
            output: ResultType { values: vec![] },
        }
    }

    pub fn anonymous(output: Variable) -> Self {
        Self {
            input: ResultType { values: vec![] },
            output: ResultType {
                values: vec![output],
            },
        }
    }

    pub fn output(&self) -> &ResultType {
        &self.output
    }

    pub fn input(&self) -> &ResultType {
        &self.input
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty() && self.output.is_empty()
    }
}

impl Validation<()> for FunctionType {
    fn validate(&self, _: &Context, _: ()) -> Result<(), ValidationError> {
        Ok(())
    }
}

impl Validation<FunctionType> for FunctionType {
    fn validate(&self, _: &Context, f2: FunctionType) -> Result<(), ValidationError> {
        // Both FunctionTypes must be equal, otherwise error
        if *self == f2 {
            Ok(())
        } else {
            Err(ValidationError::new())
        }
    }
}

/// ["ResultType"] contains the return values from exiting instructions or calling
/// a function. Its a sequence of values
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ResultType {
    values: Vec<Variable>,
}
impl std::fmt::Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for var in self.values.iter() {
            write!(f, "{}", var)?;
        }
        Ok(())
    }
}
impl ResultType {
    pub fn new(values: Vec<Variable>) -> Self {
        Self { values }
    }

    pub fn as_ref(&self) -> &[Variable] {
        &self.values
    }

    pub fn values(&self) -> Vec<ValueType> {
        self.values.iter().map(|i| *i.ty()).collect::<Vec<_>>()
    }

    pub fn take(self) -> Vec<Variable> {
        self.values
    }

    pub fn extend(&mut self, list: Vec<Variable>) {
        self.values.extend(list);
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

#[derive(Clone, Default)]
pub struct FuncParam {
    var: Vec<Variable>,
}
impl FuncParam {
    pub fn ty(&self) -> ResultType {
        ResultType::new(self.var.clone())
    }
}
impl std::fmt::Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for var in self.var.iter() {
            write!(f, "(param {})", var)?;
        }
        Ok(())
    }
}
impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for FuncParam {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Param)?;
        let var = match get_id(tokens) {
            Some(id) => vec![Variable::new(id, ValueType::parse(tokens)?)],
            None => {
                let mut tys = vec![];
                while tokens.peek().copied().expect_right_paren().is_err() {
                    tys.push(Variable::unset(ValueType::parse(tokens)?));
                }
                tys
            }
        };
        tokens.next().expect_right_paren()?;
        Ok(Self { var })
    }
}

pub struct LocalParam {
    var: Vec<Variable>,
}
impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for LocalParam {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Local)?;
        let var = match get_id(tokens) {
            Some(id) => vec![Variable::new(id, ValueType::parse(tokens)?)],
            None => {
                let mut tys = vec![];
                while tokens.peek().copied().expect_right_paren().is_err() {
                    tys.push(Variable::unset(ValueType::parse(tokens)?));
                }
                tys
            }
        };
        tokens.next().expect_right_paren()?;
        Ok(Self { var })
    }
}

#[derive(Default, Clone)]
pub struct FuncResult(pub Vec<Variable>);
impl FuncResult {
    pub fn ty(&self) -> ResultType {
        ResultType::new(self.0.clone())
    }
}
impl std::fmt::Display for FuncResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for var in self.0.iter() {
            write!(f, "(result {})", var)?;
        }
        Ok(())
    }
}
impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for FuncResult {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Result)?;
        let mut values = vec![];
        while tokens.peek().copied().expect_right_paren().is_err() {
            values.push(Variable::unset(ValueType::parse(tokens)?));
        }
        tokens.next().expect_right_paren()?;
        Ok(FuncResult(values))
    }
}

#[derive(Default)]
pub struct TypeDefinition {
    id: Option<String>,
    params: FuncParam,
    result: FuncResult,
}
impl TypeDefinition {
    pub fn id(&self) -> Option<&String> {
        self.id.as_ref()
    }

    pub fn ty(&self) -> FunctionType {
        FunctionType::new(self.params.ty(), self.result.ty())
    }
}

impl std::fmt::Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(type ")?;
        write_optional(f, " $", self.id.as_ref())?;
        write!(f, "(func {} {}))", self.params, self.result)?;
        Ok(())
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TypeDefinition {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Type)?;
        this.id = get_id(tokens);
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Func)?;

        while Some(Keyword::Param) == get_next_keyword(tokens) {
            this.params.var.extend(FuncParam::parse(tokens)?.var);
        }
        while Some(Keyword::Result) == get_next_keyword(tokens) {
            this.result.0.extend(FuncResult::parse(tokens)?.0);
        }

        // For `func`
        tokens.next().expect_right_paren()?;

        // For `type`
        tokens.next().expect_right_paren()?;
        Ok(this)
    }
}

// FunctionReference is just a pointer to a function
pub type FunctionReference = i32;
// ExternRef is just a pointer to a object
pub type ExternRef = i32;

/// ValueType are individual values that wasm can compute and a value that a
/// variable can use.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueType {
    Num(NumType),
    VecType(VecType),
    RefType(RefType),
}
impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Num(num) => write!(f, "{}", num),
            ValueType::VecType(num) => write!(f, "{}", num),
            ValueType::RefType(num) => write!(f, "{}", num),
        }
    }
}
impl Default for ValueType {
    fn default() -> Self {
        Self::RefType(RefType::default())
    }
}
impl ValueType {
    /// Returns `true` if the value type is [`Num`].
    ///
    /// [`Num`]: ValueType::Num
    #[must_use]
    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(..))
    }

    /// Returns `true` if the value type is [`VecType`].
    ///
    /// [`VecType`]: ValueType::VecType
    #[must_use]
    pub fn is_vec_type(&self) -> bool {
        matches!(self, Self::VecType(..))
    }

    /// Returns `true` if the value type is [`RefType`].
    ///
    /// [`RefType`]: ValueType::RefType
    #[must_use]
    pub fn is_ref_type(&self) -> bool {
        matches!(self, Self::RefType(..))
    }

    pub fn try_into_ref_type(self) -> Result<RefType, ValidationError> {
        if let Self::RefType(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_vec_type(self) -> Result<VecType, ValidationError> {
        if let Self::VecType(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_num(self) -> Result<NumType, ValidationError> {
        if let Self::Num(v) = self {
            Ok(v)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_value_type(self, other: &Self) -> Result<Self, ValidationError> {
        if self == *other {
            Ok(self)
        } else {
            Err(ValidationError::new())
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for ValueType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let token = tokens.peek();
        let value = match token.copied().expect_keyword()? {
            Keyword::I32 => ValueType::Num(NumType::I32),
            Keyword::I64 => ValueType::Num(NumType::I64),
            Keyword::F32 => ValueType::Num(NumType::F32),
            Keyword::F64 => ValueType::Num(NumType::F64),
            _ => return Err(Error::new(token.cloned().cloned(), "Unexpected keyword")),
        };
        let _ = tokens.next();
        Ok(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElementRefType {
    Ref(RefType),
    Heap(HeapType),
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ElementRefType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
        Ok(match tokens.peek().cloned().expect_keyword()? {
            Keyword::ExternRef | Keyword::FuncRef => Self::Ref(RefType::parse(tokens)?),
            Keyword::Extern | Keyword::Func => Self::Heap(HeapType::parse(tokens)?),
            keyword => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    format!("Expected ElementRefType token. Got {:?} instead.", keyword),
                ))
            }
        })
    }
}

impl std::fmt::Display for ElementRefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ElementRefType::Ref(v) => write!(f, "{}", v),
            ElementRefType::Heap(v) => write!(f, "{}", v),
        }
    }
}

impl Default for ElementRefType {
    fn default() -> Self {
        // TODO(Alec): This is not based in reality.
        ElementRefType::Ref(RefType::ExternRef)
    }
}

/// First class references to objects in the runtime ["Store"].
///
/// Reference types are opaque, meaning that neither their size nor their bit
/// pattern can be observed. Values of reference type can be stored in ["Tables"]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefType {
    // FunctionReference must exist, however, we don't know what it takes and we don't
    // know what it returns. The Function must exist in the program.
    FuncRef,
    // A reference to a host resource. The resource should be owned by the ["Embedder"].
    // This is a type of pointer.
    ExternRef,
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for RefType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
        Ok(match tokens.next().expect_keyword()? {
            // TODO(Alec): This technically isn't right...is it? because we can reference
            // a heap type. Maybe it doesn't matter. I guess the future will tell us.
            Keyword::Func | Keyword::FuncRef => Self::FuncRef,
            Keyword::Extern | Keyword::ExternRef => Self::ExternRef,
            keyword => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    format!("Expected RefType token. Got {:?} instead.", keyword),
                ))
            }
        })
    }
}

impl std::fmt::Display for RefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefType::FuncRef => write!(f, "funcref"),
            RefType::ExternRef => write!(f, "externref"),
        }
    }
}
impl Default for RefType {
    fn default() -> Self {
        Self::ExternRef
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeapType {
    // FunctionReference must exist, however, we don't know what it takes and we don't
    // know what it returns. The Function must exist in the program.
    Func,
    // A reference to a host resource. The resource should be owned by the ["Embedder"].
    // This is a type of pointer.
    Extern,
}

impl From<HeapType> for RefType {
    fn from(val: HeapType) -> Self {
        match val {
            HeapType::Func => RefType::FuncRef,
            HeapType::Extern => RefType::ExternRef,
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for HeapType {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error> {
        Ok(match tokens.next().expect_keyword()? {
            Keyword::Func => Self::Func,
            Keyword::Extern => Self::Extern,
            keyword => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    format!("Expected RefType token. Got {:?} instead.", keyword),
                ))
            }
        })
    }
}

impl std::fmt::Display for HeapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapType::Func => write!(f, "func"),
            HeapType::Extern => write!(f, "extern"),
        }
    }
}

/// Vector types classify numeric values processed by SIMD instructions.
/// Also known as v128. It can be interpreted as signed or unsigned,
/// floating point, integer numbers or a single 128 bit type.
///
/// These are transparent (like ["NumType"]), meaning their bit patterns can
/// be observed. Values of vector type can be stored in memory
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VecType;
impl std::fmt::Display for VecType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v128")
    }
}
// TODO(Alec): Implement all the permutations of VecType. Because the value
// is "transparent"

pub enum VectorMemoryOp {
    I8x8,
    I16x4,
    I32x2,
}

impl VectorMemoryOp {
    pub fn bit_width(&self) -> u32 {
        match self {
            VectorMemoryOp::I8x8 => 8 / (8 * 8),
            VectorMemoryOp::I16x4 => 16 / (8 * 4),
            VectorMemoryOp::I32x2 => 32 / (8 * 2),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VectorShape {
    Int(IntegerVectorShape),
    Float(FloatVectorShape),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerVectorShape {
    I8x16,
    I16x8,
    I32x4,
    I64x2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatVectorShape {
    F32x4,
    F64x2,
}

pub type Boolean = i32;
pub type Pointer = i32;

/// Number types are transparent, meaning that their bit patterns can be observed.
/// Values of number type can be stored in ["Memory"].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumType {
    I32, // servers as Booleans and Memory Addresses
    I64,
    F32,
    F64,
}
impl std::fmt::Display for NumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumType::I32 => write!(f, "i32"),
            NumType::I64 => write!(f, "i64"),
            NumType::F32 => write!(f, "f32"),
            NumType::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IntType {
    #[default]
    I32,
    I64,
}
impl std::fmt::Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntType::I32 => write!(f, "i32"),
            IntType::I64 => write!(f, "i62"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    F32,
    F64,
}
impl From<FloatType> for NumType {
    fn from(val: FloatType) -> Self {
        match val {
            FloatType::F32 => NumType::F32,
            FloatType::F64 => NumType::F64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Left,
    Rigth,
}

impl NumType {
    /// Returns `true` if the num type is [`I32`].
    ///
    /// [`I32`]: NumType::I32
    #[must_use]
    pub fn is_i32(&self) -> bool {
        matches!(self, Self::I32)
    }

    pub fn try_into_i32(self) -> Result<NumType, ValidationError> {
        if self.is_i32() {
            Ok(self)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn try_into_ty(self, ty: NumType) -> Result<NumType, ValidationError> {
        if self == ty {
            Ok(self)
        } else {
            Err(ValidationError::new())
        }
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            NumType::I32 => 32,
            NumType::I64 => 64,
            NumType::F32 => 32,
            NumType::F64 => 64,
        }
    }
}

// impl ValueType {
//     pub fn set_value(self, value: &str) -> crate::error::Result<ValueType> {
//         let err = WasmError::err(format!(
//             "value defined '{}' does not match defined type of {}",
//             value, self
//         ));
//         match self {
//             ValueType::I32(_) => Ok(ValueType::I32(i32::from_str(value).map_err(|_| err)?)),
//             ValueType::I64(_) => Ok(ValueType::I64(i64::from_str(value).map_err(|_| err)?)),
//             ValueType::F32(_) => Ok(ValueType::F32(f32::from_str(value).map_err(|_| err)?)),
//             ValueType::F64(_) => Ok(ValueType::F64(f64::from_str(value).map_err(|_| err)?)),
//         }
//     }

//     pub fn same_stack_types(p1: &[ValueType], p2: &[ValueType]) -> bool {
//         if p1.len() != p2.len() {
//             false
//         } else {
//             for (i1, i2) in p1.iter().zip(p2) {
//                 //TODO(Alec): This should return an error so we can let the user know
//                 if !i1.type_equality(i2) {
//                     return false;
//                 }
//             }
//             true
//         }
//     }

//     pub fn type_equality(&self, other: &ValueType) -> bool {
//         let mut result = false;
//         match self {
//             ValueType::I32(_) => {
//                 if let ValueType::I32(_) = other {
//                     result = true
//                 }
//             }
//             ValueType::I64(_) => {
//                 if let ValueType::I64(_) = other {
//                     result = true
//                 }
//             }
//             ValueType::F32(_) => {
//                 if let ValueType::F32(_) = other {
//                     result = true
//                 }
//             }
//             ValueType::F64(_) => {
//                 if let ValueType::F64(_) = other {
//                     result = true
//                 }
//             }
//         };
//         result
//     }

//     pub fn value_equality(&self, other: &ValueType) -> bool {
//         let mut result = false;
//         match self {
//             ValueType::I32(i1) => {
//                 if let ValueType::I32(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//             ValueType::I64(i1) => {
//                 if let ValueType::I64(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//             ValueType::F32(i1) => {
//                 if let ValueType::F32(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//             ValueType::F64(i1) => {
//                 if let ValueType::F64(i2) = other {
//                     result = *i1 == *i2
//                 }
//             }
//         };
//         result
//     }

//     pub fn add(&self, other: ValueType) -> crate::error::Result<ValueType> {
//         match self {
//             ValueType::I32(i1) => {
//                 if let ValueType::I32(i2) = other {
//                     Ok(ValueType::I32(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//             ValueType::I64(i1) => {
//                 if let ValueType::I64(i2) = other {
//                     Ok(ValueType::I64(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//             ValueType::F32(i1) => {
//                 if let ValueType::F32(i2) = other {
//                     Ok(ValueType::F32(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//             ValueType::F64(i1) => {
//                 if let ValueType::F64(i2) = other {
//                     Ok(ValueType::F64(*i1 + i2))
//                 } else {
//                     Err(WasmError::err(format!(
//                         "failed to 'add' {:?} with {:?}. Incorrect types",
//                         self, other
//                     )))
//                 }
//             }
//         }
//     }

//     pub fn type_id_string(&self) -> String {
//         match self {
//             ValueType::I32(_) => "i32".to_string(),
//             ValueType::I64(_) => "i64".to_string(),
//             ValueType::F32(_) => "f32".to_string(),
//             ValueType::F64(_) => "f64".to_string(),
//         }
//     }

//     pub fn value_string(&self) -> String {
//         match self {
//             ValueType::I32(i) => i.to_string(),
//             ValueType::I64(i) => i.to_string(),
//             ValueType::F32(i) => i.to_string(),
//             ValueType::F64(i) => i.to_string(),
//         }
//     }
// }

#[derive(Default, Clone)]
pub struct RelativeExport {
    pub name: String,
}

impl std::fmt::Display for RelativeExport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(export \"{}\")", self.name)
    }
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for RelativeExport {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Export)?;
        let name = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        Ok(RelativeExport { name })
    }
}

#[derive(Default, Clone)]
pub struct RelativeImport {
    module: String,
    name: String,
}

impl std::fmt::Display for RelativeImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(import \"{}\" \"{}\")", self.module, self.name)
    }
}

impl<'a, I: Iterator<Item = &'a Token>> Parse<'a, I> for RelativeImport {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Import)?;
        let module = tokens.next().expect_string()?;
        let name = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        Ok(RelativeImport { module, name })
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeUse {
    index: Option<Index>,
    params: ResultType,
    results: ResultType,
}

impl TypeUse {
    pub fn try_into_index(&self) -> Result<Index, Error> {
        if let Some(v) = self.index.as_ref() {
            Ok(v.clone())
        } else {
            Err(Error::new(
                None,
                format!(
                    "Attempted to get index but found function definition instead: {}",
                    self
                ),
            ))
        }
    }
}
impl std::fmt::Display for TypeUse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(index) = self.index.as_ref() {
            write!(f, " (type {})", index)?;
        }
        for param in self.params.as_ref() {
            write!(f, " (param {})", param)?;
        }
        for result in self.results.as_ref() {
            write!(f, " (result {})", result)?;
        }
        Ok(())
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TypeUse {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        match get_next_keyword(tokens) {
            Some(Keyword::Type) => {
                tokens.next().expect_left_paren()?;
                tokens.next().expect_keyword_token(Keyword::Type)?;
                let index = Index::parse(tokens)?;
                tokens.next().expect_right_paren()?;
                this.index = Some(index)
            }
            Some(Keyword::Param) | Some(Keyword::Result) => {}
            _ => return Ok(TypeUse::default()),
        }

        while Some(Keyword::Param) == get_next_keyword(tokens) {
            this.params.extend(FuncParam::parse(tokens)?.var);
        }
        while Some(Keyword::Result) == get_next_keyword(tokens) {
            this.results.extend(FuncResult::parse(tokens)?.0);
        }

        Ok(this)
    }
}

#[derive(Debug, Clone, Default)]
pub struct TableUse {
    index: Index,
}

impl TableUse {
    pub fn index(&self) -> &Index {
        &self.index
    }
}
impl std::fmt::Display for TableUse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(table {})", self.index)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableUse {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Table)?;
        let index = Index::parse(tokens)?;
        tokens.next().expect_right_paren()?;
        Ok(Self { index })
    }
}

pub struct StartOpts {
    func_id: Index,
}

impl std::fmt::Display for StartOpts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(start {})", self.func_id)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for StartOpts {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Start)?;
        let func_id = if let Some(id) = get_id(tokens) {
            Index::Id(id)
        } else if let Ok(index) = read_u32(tokens.peek().copied().expect_number()?) {
            Index::Index(index)
        } else {
            return Err(Error::new(
                tokens.next().cloned(),
                "'start' block expected index of number or function id.",
            ));
        };
        tokens.next().expect_right_paren()?;
        Ok(Self { func_id })
    }
}

#[derive(Clone)]
pub struct UseMemory(Index);
impl std::fmt::Display for UseMemory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(memory {})", self.0)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for UseMemory {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Memory)?;
        let id = get_index(tokens)?;
        tokens.next().expect_right_paren()?;
        Ok(Self(id))
    }
}

#[derive(Default, Clone, Debug)]
pub struct Instruction {
    instructions: Vec<Operation>,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in self.instructions.iter() {
            f.pad(&" ".repeat(4))?;
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for Instruction {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut instructions = vec![];
        // TODO(Alec): we need to work on this :( very sad
        loop {
            if tokens.peek().copied().expect_left_paren().is_ok() {
                tokens.next().expect_left_paren()?;
                let op = Operation::parse(tokens)?;
                while tokens.peek().copied().expect_left_paren().is_ok() {
                    let instr = Instruction::parse(tokens)?;
                    instructions.extend(instr.instructions);
                }
                instructions.push(op);
                tokens.next().expect_right_paren()?;
            } else if tokens.peek().copied().expect_right_paren().is_err() {
                let op = Operation::parse(tokens)?;
                while tokens.peek().copied().expect_left_paren().is_ok() {
                    let instr = Instruction::parse(tokens)?;
                    instructions.extend(instr.instructions);
                }
                instructions.push(op);
            } else if tokens.peek().copied().expect_right_paren().is_ok() {
                return Ok(Instruction { instructions });
            } else {
                unreachable!()
            }
        }

        // while tokens.peek().copied().expect_left_paren().is_ok() {
        //     tokens.next().expect_left_paren()?;
        //     let op = Operation::parse(tokens)?;
        //     // If the instructions are written in S-Expression, process this first
        //     while tokens.peek().copied().expect_left_paren().is_ok() {
        //         let instr = Instruction::parse(tokens)?;
        //         instructions.extend(instr.instructions);
        //     }
        //     instructions.push(op);
        //     tokens.next().expect_right_paren()?;
        // }
    }
}
impl ValidateInstruction for Instruction {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        for expr in &self.instructions {
            expr.validate(ctx, inputs)?;
        }
        Ok(vec![])
    }
}
impl Instruction {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
    }

    pub fn create(instructions: Vec<Operation>) -> Self {
        Self { instructions }
    }

    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

#[derive(Debug, Clone, Default)]
pub struct OffsetExpr(Instruction);
impl std::fmt::Display for OffsetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(offset {})", self.0)
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for OffsetExpr {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let instr = match get_next_keyword(tokens) {
            Some(Keyword::Offset) => {
                tokens.next().expect_left_paren()?;
                tokens.next().expect_keyword_token(Keyword::Offset)?;
                let instruction = Instruction::parse(tokens)?;
                tokens.next().expect_right_paren()?;
                instruction
            }
            Some(key) if key.has_unary_return_ty_i32() => {
                tokens.next().expect_left_paren()?;
                let instruction = Instruction::create(vec![Operation::parse(tokens)?]);
                tokens.next().expect_right_paren()?;
                instruction
            }
            key => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    format!("'{:?}' keyword not expected in offset block.", key),
                ))
            }
        };

        Ok(Self(instr))
    }
}
impl ValidateInstruction for OffsetExpr {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.0.validate(ctx, inputs)
    }
}

#[derive(Clone)]
pub enum DataOpsMode {
    Passive,
    Active,
}

#[derive(Clone)]
pub struct DataOps {
    id: Option<String>,
    pub data: String,
    mode: DataOpsMode,
    memory: UseMemory,
    offset: Option<OffsetExpr>,
}

impl DataOps {
    pub fn limit(&self) -> Limit {
        let max = self.data.as_bytes().len().div_ceil(PAGE_SIZE);
        Limit::new(0, Some(max as u32))
    }
}

impl std::fmt::Display for DataOps {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("This is for memory, but i don't want to deal with it right now. Will come back.")
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for DataOps {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self {
            id: None,
            data: String::new(),
            mode: DataOpsMode::Passive,
            memory: UseMemory(Index::Index(0)),
            offset: None,
        };

        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Data)?;
        this.id = get_id(tokens);
        if tokens.peek().copied().try_left_paran().is_some() {
            this.memory = UseMemory::parse(tokens)?;
            this.offset = Some(OffsetExpr::parse(tokens)?);
            this.mode = DataOpsMode::Active
        }
        this.data = tokens.peek().copied().expect_string().unwrap_or_default();
        tokens.next().expect_right_paren()?;

        Ok(this)
    }
}

#[derive(Default, Clone)]
pub struct MemoryOpts {
    id: Option<String>,
    limit: Limit,
    import: Option<RelativeImport>,
    export: Option<RelativeExport>,
    data: Option<DataOps>,
}
impl std::fmt::Display for MemoryOpts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(memory")?;
        write_optional(f, " $", self.id.as_ref())?;
        write_optional(f, " ", self.import.as_ref())?;
        write_optional(f, " ", self.export.as_ref())?;
        write!(f, " {})", self.limit)?;
        match self.data.as_ref() {
            Some(data) => write!(f, "\n{}", data),
            None => Ok(()),
        }
    }
}
impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for MemoryOpts {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Memory)?;
        this.id = get_id(tokens);
        // import / export
        match get_next_keyword(tokens) {
            Some(Keyword::Import) => this.import = Some(RelativeImport::parse(tokens)?),
            Some(Keyword::Export) => this.export = Some(RelativeExport::parse(tokens)?),
            _ => {}
        }
        // data
        // match get_next_keyword(tokens) {
        //     Some(Keyword::Data) => DataDefinition::parse(tokens)?
        //     _ => this.limit = Limit::parse(tokens)?,
        // }

        let mut data: Option<DataOps> = None;
        if tokens.peek().copied().try_left_paran().is_some() {
            let opt = match tokens.clone().nth(1).expect_keyword()? {
                // Keyword::Import => import.push(RelativeImport::parse(tokens)?),
                // Keyword::Export => export.push(RelativeExport::parse(tokens)?),
                Keyword::Data => DataOps::parse(tokens)?,
                key => {
                    return Err(Error::new(
                        tokens.next().cloned(),
                        format!(
                        "Memory inner block only expected 'import', 'export' or 'data'. Got {:?}",
                        key
                    ),
                    ))
                }
            };
            match data.as_mut() {
                Some(inner) => inner.data.push_str(&opt.data),
                None => data = Some(opt),
            }
        }
        this.limit = if tokens.peek().copied().try_right_paran().is_some() {
            if let Some(data) = &data {
                data.limit()
            } else {
                return Err(Error::new(
                    tokens.next().cloned(),
                    "expected memory to declare a limit. No limit found.",
                ));
            }
        } else {
            Limit::parse(tokens)?
        };
        tokens.next().expect_right_paren()?;
        this.data = data;
        Ok(this)
    }
}

pub struct AssertMalformed {
    wat: Vec<String>,
    expected_error: String,
}

impl AssertMalformed {
    pub fn test(&self) -> Result<(), Error> {
        for wat in self.wat.iter() {
            let module = format!("(module {})", wat);
            let tokens = tokenize(&module)?;
            let mut iter = tokens.iter().peekable();
            match Module::parse(&mut iter) {
                Ok(_) => {
                    return Err(Error::new(
                        None,
                        "Module compiled successfully when it was expected to fail",
                    ))
                }
                Err(err) if err.error_ty() == self.expected_error => {}
                Err(err) => {
                    return Err(Error::new(
                        err.token().cloned(),
                        format!(
                            "Error '{}' != '{}'. Expected '{}'",
                            self.expected_error,
                            err.error_ty(),
                            err.error()
                        ),
                    ))
                }
            }
        }
        Ok(())
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for AssertMalformed {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens
            .next()
            .expect_keyword_token(Keyword::AssertMalformed)?;
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Module)?;
        tokens.next().expect_keyword_token(Keyword::Quote)?;
        let mut wat = vec![];
        while tokens.peek().copied().expect_string().is_ok() {
            wat.push(tokens.next().expect_string()?);
        }
        tokens.next().expect_right_paren()?;
        let expected_error = tokens.next().expect_string()?;
        tokens.next().expect_right_paren()?;
        Ok(Self {
            wat,
            expected_error,
        })
    }
}

pub struct AssertInvalid {
    module: Module,
    expected_error: String,
}

impl AssertInvalid {
    pub fn test(&self) -> Result<(), Error> {
        let mut ctx = Context::default();
        let mut input = Input::new();
        match self.module.validate(&mut ctx, &mut input) {
            Ok(_) => Err(Error::new(
                None,
                "Module validated successfully when it was expected to fail",
            )),
            Err(err) if err.error_ty() == self.expected_error => Ok(()),
            Err(err) => Err(Error::new(
                None,
                format!(
                    "Error '{}' != '{}'. Expected '{}'",
                    self.expected_error,
                    err.error_ty(),
                    err.error()
                ),
            )),
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for AssertInvalid {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::AssertInvalid)?;

        let module = Module::parse(tokens)?;
        let expected_error = tokens.next().expect_string()?;

        tokens.next().expect_right_paren()?;
        Ok(Self {
            module,
            expected_error,
        })
    }
}

pub struct AssertReturn {
    function_name: String,
    variables: Vec<Box<dyn Execute>>,
    expected: Vec<Box<dyn Execute>>,
}

impl AssertReturn {
    pub fn test(&self, module: &Module) -> Result<(), Error> {
        // store is global state
        let mut store = Store::default();

        // instance is
        let instance = ModuleInstance::new(&mut store, module).expect("success");

        todo!("ahahahaha, i should never reach this yet :P");
        // match module.call(&mut ctx, &mut input) {
        //     Ok(_) => Err(Error::new(
        //         None,
        //         "Module validated successfully when it was expected to fail",
        //     )),
        //     Err(err) if err.error_ty() == self.expected_error => Ok(()),
        //     Err(err) => Err(Error::new(
        //         None,
        //         format!(
        //             "Error '{}' != '{}'. Expected '{}'",
        //             self.expected_error,
        //             err.error_ty(),
        //             err.error()
        //         ),
        //     )),
        // }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for AssertReturn {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::AssertReturn)?;

        // Parse Inoke
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Invoke)?;
        let function_name = tokens.next().expect_string()?;

        let mut variables: Vec<Box<dyn Execute>> = vec![];
        while tokens.peek().copied().expect_left_paren().is_ok() {
            tokens.next().expect_left_paren()?;
            variables.push(Box::new(Const::parse(tokens)?));
            tokens.next().expect_right_paren()?;
        }
        tokens.next().expect_right_paren()?;

        // Parse Result
        let mut expected: Vec<Box<dyn Execute>> = vec![];
        while tokens.peek().copied().expect_left_paren().is_ok() {
            tokens.next().expect_left_paren()?;
            expected.push(Box::new(Const::parse(tokens)?));
            tokens.next().expect_right_paren()?;
        }

        tokens.next().expect_right_paren()?;
        Ok(Self {
            function_name,
            variables,
            expected,
        })
    }
}

#[derive(Default, Clone)]
pub struct FunctionDefinition {
    // id
    id: Option<String>,
    // module level information
    import: Option<RelativeImport>,
    export: Vec<RelativeExport>,
    // type t
    ty: TypeUse,
    // locals l
    locals: Vec<Variable>,
    // instructions
    instructions: Instruction,
}

impl std::fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(func")?;
        write_optional(f, " $", self.id.as_ref())?;
        write_optional(f, " ", self.import.as_ref())?;
        for export in self.export.iter() {
            write!(f, " {}", export)?;
        }
        write!(f, "{}", self.ty)?;
        for local in self.locals.iter() {
            write!(f, " (local {})", local)?;
        }
        if self.instructions.is_empty() {
            write!(f, ")")?;
        } else {
            writeln!(f)?;
            write!(f, "{}", self.instructions)?;
            f.pad(&" ".repeat(2))?;
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl FunctionDefinition {
    pub fn id(&self) -> Option<&String> {
        self.id.as_ref()
    }

    pub fn ty(&self) -> &TypeUse {
        &self.ty
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for FunctionDefinition {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Func)?;
        this.id = get_id(tokens);
        if tokens.peek().copied().expect_right_paren().is_ok() {
            tokens.next().expect_right_paren()?;
            return Ok(this);
        }

        while let Some(keyword) = get_next_keyword(tokens) {
            match keyword {
                Keyword::Import if this.import.is_none() => {
                    this.import = Some(RelativeImport::parse(tokens)?);
                }
                Keyword::Export => this.export.push(RelativeExport::parse(tokens)?),
                _ => break,
            }
        }

        this.ty = TypeUse::parse(tokens)?;
        while Some(Keyword::Local) == get_next_keyword(tokens) {
            this.locals.extend(LocalParam::parse(tokens)?.var);
        }

        this.instructions = Instruction::parse(tokens)?;

        tokens.next().expect_right_paren()?;
        Ok(this)
    }
}

impl ValidateInstruction for FunctionDefinition {
    fn validate(&self, _: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        todo!("Alec, we need to fix the TypeUse issue, everything should change into an index :(");
        // let ty = if let Some(ty) = &self.ty {
        //     ctx.get_type(ty.try_into_index().as_ref().unwrap())?.clone()
        // } else {
        //     FunctionType::new(self.params.clone(), self.result.clone())
        // };

        // let mut ctx1 = ctx.clone();
        // for parameters in &self.params.values {
        //     ctx1.push_local(parameters);
        // }
        // for local in &self.locals {
        //     ctx1.push_local(local);
        // }
        // // TODO(Alec): Labels, we don't store them right now on our function defintion
        // // https://webassembly.github.io/spec/core/valid/modules.html#functions
        // ctx1.set_returning(Some(self.result.clone()));

        // let return_ty = self.instructions.validate(&mut ctx1, inputs)?;

        // if return_ty == self.result.values() {
        //     Ok(return_ty)
        // } else {
        //     Err(ValidationError::new())
        // }
    }
}

#[derive(Default, Clone)]
pub struct GlobalDefinition {
    // id
    id: Option<String>,
    // module level information
    import: Option<RelativeImport>,
    export: Option<RelativeExport>,
    // Global information
    ty: GlobalType,
    // instructions
    // init: Instruction,
    init: Const,
}

impl std::fmt::Display for GlobalDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(global",)?;
        write_optional(f, " $", self.id.as_ref())?;
        write_optional(f, " ", self.import.as_ref())?;
        write_optional(f, " ", self.export.as_ref())?;
        write!(f, " {} {})", self.ty, self.init)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for GlobalDefinition {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Global)?;
        this.id = get_id(tokens);

        match get_next_keyword(tokens) {
            Some(Keyword::Import) => this.import = Some(RelativeImport::parse(tokens)?),
            Some(Keyword::Export) => this.export = Some(RelativeExport::parse(tokens)?),
            _ => {}
        }

        this.ty = GlobalType::parse(tokens)?;

        // Parse instruction
        tokens.next().expect_left_paren()?;
        this.init = Const::parse(tokens)?;
        tokens.next().expect_right_paren()?;

        // Complete block
        tokens.next().expect_right_paren()?;
        Ok(this)
    }
}

impl ValidateInstruction for GlobalDefinition {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.ty.validate(ctx, ())?;
        // TODO(Alec): The expression must be a constant expressions only
        // Add validation for the condition above
        // TODO(Alec): This is only supposed to be constant? Do we need a special function for this?
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

#[derive(Clone, Default)]
pub struct TableDefinition {
    // id
    id: Option<String>,
    // module level information
    import: Option<RelativeImport>,
    export: Option<RelativeExport>,
    // Global information
    ty: TableType,
}

impl std::fmt::Display for TableDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(table")?;
        write_optional(f, " $", self.id.as_ref())?;
        write_optional(f, " ", self.import.as_ref())?;
        write_optional(f, " ", self.export.as_ref())?;
        write!(f, " {})", self.ty)
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for TableDefinition {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Table)?;

        this.id = get_id(tokens);

        match get_next_keyword(tokens) {
            Some(Keyword::Import) => this.import = Some(RelativeImport::parse(tokens)?),
            Some(Keyword::Export) => this.export = Some(RelativeExport::parse(tokens)?),
            _ => {}
        }

        this.ty = TableType::parse(tokens)?;

        // Complete block
        tokens.next().expect_right_paren()?;
        Ok(this)
    }
}

impl ValidateInstruction for TableDefinition {
    fn validate(&self, ctx: &mut Context, _: &mut Input) -> ValidateResult<Vec<ValueType>> {
        self.ty.validate(ctx, ())?;
        Ok(vec![])
    }
}

#[derive(Clone, Default)]
pub struct DataDefinition {
    id: Option<String>,
    mode: DataMode,
    data: Vec<u8>,
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for DataDefinition {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Data)?;
        this.id = get_id(tokens);

        let memory = if let Some(Keyword::Memory) = get_next_keyword(tokens) {
            tokens.next().expect_left_paren()?;
            tokens.next().expect_keyword_token(Keyword::Memory)?;
            let index = if tokens.peek().cloned().expect_number().is_ok() {
                Index::Index(read_u32(tokens.next().expect_number()?)?)
            } else {
                let id = get_id(tokens)
                    .ok_or_else(|| Error::new(tokens.next().cloned(), "Expected Id".to_string()))?;
                Index::Id(id)
            };
            tokens.next().expect_right_paren()?;
            Some(index)
        } else {
            None
        };

        let offset = match get_next_keyword(tokens) {
            Some(Keyword::Offset) => {
                tokens.next().expect_left_paren()?;
                tokens.next().expect_keyword_token(Keyword::Offset)?;
                tokens.next().expect_left_paren()?;
                let offset = Const::parse(tokens)?;
                tokens.next().expect_right_paren()?;
                tokens.next().expect_right_paren()?;
                Some(offset)
            }
            Some(Keyword::Const(_)) => {
                tokens.next().expect_left_paren()?;
                let offset = Const::parse(tokens)?;
                tokens.next().expect_right_paren()?;
                Some(offset)
            }
            _ => None,
        };

        if tokens.peek().copied().expect_string().is_ok() {
            let mut str = Vec::new();
            while tokens.peek().copied().expect_string().is_ok() {
                str.extend(tokens.next().expect_string()?.as_bytes());
            }
            this.data = str;
        };

        this.mode = match (memory, offset) {
            (Some(memory), Some(offset)) => DataMode::Active { memory, offset },
            (None, Some(offset)) => DataMode::Active {
                memory: Default::default(),
                offset,
            },
            (Some(_), None) => DataMode::Passive,
            (None, None) => DataMode::Passive,
        };

        tokens.next().expect_right_paren()?;
        Ok(this)
    }
}

impl std::fmt::Display for DataDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(data")?;
        if let Some(id) = self.id.as_ref() {
            write!(f, " ${}", id)?;
        }
        match &self.mode {
            DataMode::Passive => {}
            DataMode::Active { memory, offset } => {
                write!(f, " (memory {}) (offset {})", memory, offset)?;
            }
        }
        // TODO(Alec): data.clone()...really...
        write!(f, " \"{}\")", String::from_utf8(self.data.clone()).unwrap())?;
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct ElementDefinition {
    id: Option<String>,
    // Default table is 0
    mode: ElementMode,
    ty: RefType,
    init: Vec<Instruction>,
}

impl std::fmt::Display for ElementDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(elem")?;
        write_optional(f, " $", self.id.as_ref())?;
        match &self.mode {
            ElementMode::Passive => {}
            ElementMode::Declarative => write!(f, " declare")?,
            ElementMode::Active { table, offset } => write!(f, " {} {}", table, offset)?,
        }
        if self.init.is_empty() {
            write!(f, " {})", self.ty)
        } else {
            writeln!(f, " {}", self.ty)?;
            for init in &self.init {
                write!(f, " {}", init)?;
            }
            write!(f, "  )")
        }
    }
}

impl<'a, I: Iterator<Item = &'a Token> + Clone> Parse<'a, I> for ElementDefinition {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, crate::parse::ast::Error> {
        let mut this = Self::default();
        tokens.next().expect_left_paren()?;
        tokens.next().expect_keyword_token(Keyword::Elem)?;

        this.id = get_id(tokens);

        this.mode = if let Some(Keyword::Table) = get_next_keyword(tokens) {
            ElementMode::Active {
                table: TableUse::parse(tokens)?,
                // TODO(Alec): We have similar logic here and at the bottom of this function :(
                // It would be better to understand offset and item instead and then
                // parse a constant expression.
                offset: OffsetExpr::parse(tokens)?,
            }
        } else if let Some(Keyword::Offset) = get_next_keyword(tokens) {
            ElementMode::Active {
                table: TableUse::default(),
                // TODO(Alec): We have similar logic here and at the bottom of this function :(
                // It would be better to understand offset and item instead and then
                // parse a constant expression.
                offset: OffsetExpr::parse(tokens)?,
            }
        } else if tokens
            .peek()
            .cloned()
            .expect_keyword_token(Keyword::Declare)
            .is_ok()
        {
            tokens.next().expect_keyword_token(Keyword::Declare)?;
            ElementMode::Declarative
        } else {
            ElementMode::Passive
        };

        // The default RefType seems to be `funcref` with a list of `ref.func` instructions (which would be 0)
        if tokens.peek().cloned().expect_right_paren().is_ok() {
            tokens.next().expect_right_paren()?;
            this.ty = RefType::FuncRef;
            return Ok(this);
        }

        if tokens.peek().cloned().try_id().is_some() {
            while let Some(token) = tokens.peek() {
                if *token.ty() == TokenType::Id || *token.ty() == TokenType::Number {
                    this.init
                        .push(Instruction::create(vec![Operation::RefFunc(RefFunc::new(
                            Index::parse(tokens)?,
                        ))]));
                } else {
                    break;
                }
            }
            tokens.next().expect_right_paren()?;
            this.ty = RefType::FuncRef;
            return Ok(this);
        }

        let ty = match ElementRefType::parse(tokens) {
            Ok(ty) => ty,
            Err(_) => ElementRefType::Ref(RefType::FuncRef),
        };
        this.ty = match ty {
            ElementRefType::Ref(ty) => ty,
            ElementRefType::Heap(HeapType::Func) => {
                while let Some(token) = tokens.peek() {
                    if *token.ty() == TokenType::Id || *token.ty() == TokenType::Number {
                        this.init.push(Instruction::create(vec![Operation::RefFunc(
                            RefFunc::new(Index::parse(tokens)?),
                        )]));
                    } else {
                        break;
                    }
                }
                RefType::FuncRef
            }
            ElementRefType::Heap(HeapType::Extern) => {
                return Err(Error::new(
                    tokens.next().cloned(),
                    "Did not expect heap type of 'extern' in elem block".to_string(),
                ))
            }
        };

        if tokens.peek().cloned().expect_right_paren().is_ok() {
            tokens.next().expect_right_paren()?;
            return Ok(this);
        }

        while let Some(keyword) = get_next_keyword(tokens) {
            match keyword {
                Keyword::Item | Keyword::Offset => {
                    tokens.next().expect_left_paren()?;
                    tokens.next().expect_keyword_token(keyword)?;
                    if tokens.peek().cloned().expect_left_paren().is_ok() {
                        this.init.push(Instruction::parse(tokens)?);
                    } else {
                        this.init
                            .push(Instruction::create(vec![Operation::parse(tokens)?]))
                    }
                    tokens.next().expect_right_paren()?;
                }
                _ => {
                    tokens.next().expect_left_paren()?;
                    this.init
                        .push(Instruction::create(vec![Operation::parse(tokens)?]));
                    tokens.next().expect_right_paren()?;
                }
            }
        }
        tokens.next().expect_right_paren()?;
        Ok(this)
    }
}
