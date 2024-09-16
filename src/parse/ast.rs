use std::{
    convert::TryInto,
    fmt::{self, Display, Formatter},
    iter::Peekable,
    num::TryFromIntError,
    string::ParseError,
};

use crate::{
    execution::Number,
    structure::{
        module::{get_next_keyword, Module},
        types::{AssertInvalid, AssertMalformed, AssertReturn, NumType},
    },
};

use super::{Keyword, Token, TokenType};

pub trait Parse<'a, I: Iterator<Item = &'a Token>>: Sized {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error>;
}

pub trait Visit: Sized {
    fn visit(tee: &Tee) -> Result<Self, Error>;
}

#[derive(Debug)]
pub enum ErrorTy {
    UnexpectedToken,
    UnknownNumber,
}

impl std::fmt::Display for ErrorTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorTy::UnexpectedToken => write!(f, "unexpected token"),
            ErrorTy::UnknownNumber => write!(f, "unknown number"),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    ty: ErrorTy,
    token: Option<Token>,
    error: String,
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error {:?} on token {:?} with message {}",
            self.ty, self.token, self.error
        )
    }
}

impl From<TryFromIntError> for Error {
    fn from(value: TryFromIntError) -> Self {
        Error {
            ty: ErrorTy::UnknownNumber,
            token: None,
            error: format!("{:?}", value),
        }
    }
}

impl Error {
    pub fn new(token: Option<Token>, error: impl ToString) -> Self {
        Self {
            ty: ErrorTy::UnexpectedToken,
            token,
            error: error.to_string(),
        }
    }

    pub fn token(&self) -> Option<&Token> {
        self.token.as_ref()
    }

    pub fn error(&self) -> &str {
        &self.error
    }

    pub fn error_ty(&self) -> String {
        self.ty.to_string()
    }
}

impl From<ParseError> for Error {
    fn from(val: ParseError) -> Self {
        Error::new(
            None,
            format!("Failed to parse wat tokens. ERROR: {:?}", val),
        )
    }
}

pub trait Expect<'a> {
    fn expect_left_paren(&self) -> Result<(), Error>;

    fn expect_right_paren(&self) -> Result<(), Error>;

    fn expect_number(&'a self) -> Result<&'a Token, Error>;

    fn expect_string(&self) -> Result<String, Error>;

    fn expect_keyword_token(&self, ty: Keyword) -> Result<(), Error>;

    fn expect_keyword(&self) -> Result<Keyword, Error>;
}

pub trait TryGet {
    fn try_id(&self) -> Option<String>;

    fn try_right_paran(&self) -> Option<()>;

    fn try_left_paran(&self) -> Option<()>;
}

pub trait IntoResult<T> {
    fn into_result(self) -> Result<T, Error>;
}

impl<'a> IntoResult<&'a Token> for Option<&'a Token> {
    fn into_result(self) -> Result<&'a Token, Error> {
        if let Some(token) = self {
            Ok(token)
        } else {
            Err(Error::new(None, "Expected token"))
        }
    }
}

impl TryGet for Option<&Token> {
    fn try_id(&self) -> Option<String> {
        if (*self)?.ty == TokenType::Id {
            Some((*self)?.source.clone())
        } else {
            None
        }
    }

    fn try_right_paran(&self) -> Option<()> {
        if (*self)?.ty == TokenType::RightParen {
            Some(())
        } else {
            None
        }
    }

    fn try_left_paran(&self) -> Option<()> {
        if (*self)?.ty == TokenType::LeftParen {
            Some(())
        } else {
            None
        }
    }
}

impl<'a> Expect<'a> for Option<&'a Token> {
    fn expect_left_paren(&self) -> Result<(), Error> {
        if let Some(token) = self {
            if token.ty == TokenType::LeftParen {
                return Ok(());
            }
        }
        Err(Error::new(self.map(Clone::clone), "Expected left paren"))
    }

    fn expect_right_paren(&self) -> Result<(), Error> {
        if let Some(token) = self {
            if token.ty == TokenType::RightParen {
                return Ok(());
            }
        }
        Err(Error::new(self.map(Clone::clone), "Expected right paren"))
    }

    fn expect_number(&self) -> Result<&'a Token, Error> {
        if let Some(token) = self {
            if token.ty == TokenType::Number {
                return Ok(token);
            }
        }
        Err(Error::new(self.map(Clone::clone), "Expected number"))
    }

    fn expect_keyword_token(&self, ty: Keyword) -> Result<(), Error> {
        let keyword = self.expect_keyword()?;
        if keyword == ty {
            Ok(())
        } else {
            Err(Error::new(
                self.map(Clone::clone),
                format!("Expected keyword {:?}", ty),
            ))
        }
    }

    fn expect_string(&self) -> Result<String, Error> {
        if let Some(token) = self {
            if token.ty == TokenType::String {
                return Ok(token.source.clone());
            }
        }
        Err(Error::new(self.map(Clone::clone), "Expected string"))
    }

    fn expect_keyword(&self) -> Result<Keyword, Error> {
        if let Some(token) = self {
            if let TokenType::Keyword(key) = &token.ty {
                return Ok(key.clone());
            }
        }
        Err(Error::new(self.map(Clone::clone), "Expected keyword"))
    }
}

// pub fn read_sign<I: Iterator<Item = char>>(iter: Peekable<I>) -> Result<Option<SignType>, Error> {
//     match iter.peek() {
//        Some('+') => Ok(SignType::),
//        Some('-') => todo!(),
//        Some(_) => Ok(None),
//        None =>
//    }
// }

pub fn write_optional<D: std::fmt::Display>(
    f: &mut Formatter<'_>,
    prefix: &'static str,
    opt: Option<&D>,
) -> std::fmt::Result {
    match opt {
        Some(display) => write!(f, "{}{}", prefix, display),
        None => Ok(()),
    }
}

fn read_number_err<In, T: TryInto<In>>(input: T, token: &Token) -> Result<In, Error>
where
    T::Error: std::fmt::Debug,
{
    match input.try_into() {
        Ok(b) => Ok(b),
        Err(err) => Err(Error::new(
            Some(token.clone()),
            format!("Failed to convert number with err {:?}", err),
        )),
    }
}

pub fn read_number(expected: NumType, token: &Token) -> Result<Number, Error> {
    Ok(match expected {
        NumType::I32 if token.source.starts_with('-') => token
            .source
            .parse::<i32>()
            .map(Number::I32)
            .map_err(|err| {
                Error::new(
                    Some(token.clone()),
                    format!("Error parsing negative i32: {:?}", err),
                )
            })?,
        // I just learnt this, but for some reason, we are supposed to dis-regard
        // the left most bit. In the test suite there is a i32 of 0x80000000
        // and the correct representation is supposed to be -0.
        // REF: https://github.com/rust-lang/rust/issues/108269
        // NumType::I32 => Number::I32(read_number_err(read_u32(token)?, token)?),
        // NumType::I64 => Number::I64(read_number_err(read_u64(token)?, token)?),
        NumType::I32 => Number::I32(read_u32(token)? as i32),
        NumType::I64 => Number::I64(read_u64(token)? as i64),
        NumType::F32 => Number::F32(read_f32(token)?),
        NumType::F64 => Number::F64(read_f64(token)?),
    })
}

pub fn read_u32(token: &Token) -> Result<u32, Error> {
    let number = if token.source.starts_with("0x") {
        u32::from_str_radix(&token.source.trim_start_matches("0x").replace('_', ""), 16)
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    } else {
        token
            .source
            .parse::<u32>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    }?;
    Ok(number)
}

pub fn read_u64(token: &Token) -> Result<u64, Error> {
    let number = if token.source.starts_with("0x") {
        u64::from_str_radix(token.source.trim_start_matches("0x"), 16)
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u64: {:?}", err)))
    } else {
        token
            .source
            .parse::<u64>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u64: {:?}", err)))
    }?;
    Ok(number)
}

pub fn read_f32(token: &Token) -> Result<f32, Error> {
    if token.source.starts_with("0x") {
        Ok(f32::from_bits(read_u32(token)?))
    } else {
        token
            .source
            .parse::<f32>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing f32: {:?}", err)))
    }
}

pub fn read_f64(token: &Token) -> Result<f64, Error> {
    if token.source.starts_with("0x") {
        Ok(f64::from_bits(read_u64(token)?))
    } else {
        token
            .source
            .parse::<f64>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing f64: {:?}", err)))
    }
}

pub enum TestBlock {
    Module(Module),
    AssertMalformed(AssertMalformed),
    AssertInvalid(AssertInvalid),
    AssertReturn(AssertReturn),
}

pub fn parse_test_block<'a, I: Iterator<Item = &'a Token> + Clone>(
    peek_iter: &mut Peekable<I>,
    tokens: &[Token],
) -> Result<TestBlock, Error> {
    let result = match get_next_keyword(peek_iter) {
        Some(Keyword::Module) => Module::parse(peek_iter).map(TestBlock::Module),
        Some(Keyword::AssertMalformed) => {
            AssertMalformed::parse(peek_iter).map(TestBlock::AssertMalformed)
        }
        Some(Keyword::AssertInvalid) => {
            AssertInvalid::parse(peek_iter).map(TestBlock::AssertInvalid)
        }
        Some(Keyword::AssertReturn) => AssertReturn::parse(peek_iter).map(TestBlock::AssertReturn),
        keyword => {
            return Err(Error::new(
                peek_iter.next().cloned(),
                format!("{keyword:?} was not expected as a top level directive in .wast files"),
            ))
        }
    };
    match result {
        Ok(block) => Ok(block),
        Err(err) => {
            let range = if let Some(token) = &err.token() {
                if let Some(index) = tokens.iter().position(|t| t == *token) {
                    if index == 0 {
                        tokens.get(0..=5).unwrap().to_vec()
                    } else {
                        tokens.get(index - 2..=index + 2).unwrap().to_vec()
                    }
                } else {
                    vec![]
                }
            } else {
                vec![]
            };

            println!("ERROR");
            println!("========================================");
            println!("{:?}", range);
            let tokens = range.iter().map(|t| t.source()).collect::<Vec<_>>();
            println!("Tokens around error: {:?}", tokens);
            println!("Parsing encounted error {:?}", err);
            Err(err)
        }
    }
}

pub fn parse_module_test<'a, I: Iterator<Item = &'a Token> + Clone>(
    tokens: &mut Peekable<I>,
    passed: Option<&Module>,
) -> Result<Option<Module>, Error> {
    let mut peek_iter = tokens.clone();
    peek_iter.next().expect_left_paren()?;
    let keyword = peek_iter.next().expect_keyword()?;

    let mut module = None;
    match keyword {
        Keyword::Module => {
            module = Some(Module::parse(tokens)?);
        }
        Keyword::AssertMalformed => AssertMalformed::parse(tokens)?.test()?,
        Keyword::AssertInvalid => AssertInvalid::parse(tokens)?.test()?,
        Keyword::AssertReturn if passed.is_some() => {
            AssertReturn::parse(tokens)?.test(passed.as_ref().unwrap())?
        }
        _ => {
            return Err(Error::new(
                tokens.next().cloned(),
                format!("{keyword:?} was not expected as a top level directive in .wast files"),
            ))
        }
    };
    Ok(module)
}

#[derive(Debug)]
pub struct Tee {
    root: Keyword,
    left_attributes: Vec<Token>,
    children: Vec<Tee>,
    right_attributes: Vec<Token>,
}

impl Tee {
    pub fn find_all_attributes(&self, ty: TokenType) -> Vec<&Token> {
        let mut tokens = vec![];
        for attr in self.left_attributes.iter() {
            if attr.ty == ty {
                tokens.push(attr)
            }
        }
        tokens
    }

    fn find_attribute(&self, ty: TokenType) -> Option<&Token> {
        self.left_attributes.iter().find(|&attr| attr.ty == ty)
    }

    // pub fn find_parameters(&self) ->

    // pub fn find_id(&self) -> Option<String> {
    //     self.find_attribute(TokenType::Id).map(|t| t.source.clone())
    // }

    pub fn expect_keyword(&self, ty: Keyword) -> Result<(), Error> {
        if self.root == ty {
            Ok(())
        } else {
            Err(Error::new(None, format!("Expected keyword {:?}", ty)))
        }
    }

    pub fn children(&self) -> &[Tee] {
        &self.children
    }

    pub fn root(&self) -> &Keyword {
        &self.root
    }
}

pub fn walk_tee(tee: &Tee, passed: Option<&Module>) -> Result<Option<Module>, Error> {
    let mut module = None;
    match tee.root() {
        Keyword::Module => {
            // module = Some(Module::visit(tee)?);
        }
        // Keyword::AssertMalformed => AssertMalformed::visit(tee)?.test()?,
        // Keyword::AssertInvalid => AssertInvalid::visit(tee)?.test()?,
        // Keyword::AssertReturn if passed.is_some() => {
        //     AssertReturn::visit(tee)?.test(passed.as_ref().unwrap())?
        // }
        keyword => {
            return Err(Error::new(
                None,
                format!("{keyword:?} was not expected as a top level directive in .wast files"),
            ))
        }
    };
    Ok(module)
}

pub fn parse_module_test_2<'a, I: Iterator<Item = &'a Token> + Clone>(
    tokens: &mut Peekable<I>,
) -> Result<Tee, Error> {
    tokens.next().expect_left_paren()?;
    let root = tokens.next().expect_keyword()?;

    let mut left_attributes = vec![];
    while tokens.peek().copied().try_left_paran().is_none()
        && tokens.peek().copied().try_right_paran().is_none()
    {
        left_attributes.push(tokens.next().unwrap().clone())
    }

    let mut children = vec![];
    while tokens.peek().copied().try_left_paran().is_some() {
        children.push(parse_module_test_2(tokens)?);
    }

    let mut right_attributes = vec![];
    while tokens.peek().copied().try_right_paran().is_none() {
        right_attributes.push(tokens.next().unwrap().clone())
    }

    tokens.next().expect_right_paren()?;

    Ok(Tee {
        root,
        left_attributes,
        children,
        right_attributes,
    })
}

impl Display for Tee {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        print_tee_with_indentation(f, self, 0)
    }
}

fn print_tee_with_indentation(f: &mut Formatter<'_>, tee: &Tee, indent: usize) -> fmt::Result {
    write!(f, "{}", " ".repeat(indent))?;
    write!(f, "(key:{:?}", tee.root)?;
    for attr in &tee.left_attributes {
        write!(f, " left:{}", attr.source)?;
    }
    if !tee.children.is_empty() {
        writeln!(f, "\n{}children:", " ".repeat(indent))?;
        for children in &tee.children {
            print_tee_with_indentation(f, children, indent + 1)?;
        }
    }
    for attr in &tee.right_attributes {
        match attr.ty {
            TokenType::String => write!(f, " right:\"{}\"", attr.source)?,
            TokenType::Id => write!(f, " right:${}", attr.source)?,
            TokenType::Reserved => todo!(),
            _ => write!(f, " right:{}", attr.source)?,
        }
    }
    if tee.children.is_empty() {
        writeln!(f, ") ")?;
    } else {
        writeln!(f, "{})", " ".repeat(indent))?;
    }
    Ok(())
}
