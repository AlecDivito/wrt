use std::{
    convert::TryInto,
    fmt::{self, Formatter},
    iter::Peekable,
    num::TryFromIntError,
    string::ParseError,
};

use crate::{ast::types::NumberType, execution::Number};

use super::{Keyword, Token, TokenType};

pub trait Parse<'a, I: Iterator<Item = &'a Token>>: Sized {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error>;
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

pub fn read_number(expected: NumberType, token: &Token) -> Result<Number, Error> {
    Ok(match expected {
        // I just learnt this, but for some reason, we are supposed to dis-regard
        // the left most bit. In the test suite there is a i32 of 0x80000000
        // and the correct representation is supposed to be -0.
        // REF: https://github.com/rust-lang/rust/issues/108269
        // NumberType::I32 => Number::I32(read_number_err(read_u32(token)?, token)?),
        // NumberType::I64 => Number::I64(read_number_err(read_u64(token)?, token)?),
        NumberType::I32 if token.source.starts_with('-') => {
            Number::I32((read_u32(token)? as i32).wrapping_neg())
        }
        NumberType::I64 if token.source.starts_with('-') => {
            Number::I64((read_u64(token)? as i64).wrapping_neg())
        }
        NumberType::I32 => Number::I32(read_u32(token)? as i32),
        NumberType::I64 => Number::I64(read_u64(token)? as i64),
        NumberType::F32 => Number::F32(read_f32(token)?),
        NumberType::F64 => Number::F64(read_f64(token)?),
    })
}

pub fn read_u128(token: &Token) -> Result<u128, Error> {
    let source = &token.source;
    let s = if source.starts_with('-') {
        &source[1..]
    } else {
        &source[..]
    };
    let number = if s.starts_with("0x") {
        let to_parse = s
            .trim_start_matches("0x")
            .trim_start_matches('0')
            .replace('_', "");
        if to_parse.is_empty() {
            return Ok(0);
        }
        u128::from_str_radix(&to_parse, 16)
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    } else {
        s.replace('_', "")
            .parse::<u128>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    }?;
    Ok(number)
}

pub fn read_u8(token: &Token) -> Result<u8, Error> {
    let source = &token.source;
    let s = if source.starts_with('-') {
        &source[1..]
    } else {
        &source[..]
    };
    let number = if s.starts_with("0x") {
        let to_parse = s
            .trim_start_matches("0x")
            .trim_start_matches('0')
            .replace('_', "");
        if to_parse.is_empty() {
            return Ok(0);
        }
        u8::from_str_radix(&to_parse, 16)
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    } else {
        s.replace('_', "")
            .parse::<u8>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    }?;
    Ok(number)
}

pub fn read_u32(token: &Token) -> Result<u32, Error> {
    let source = &token.source;
    let s = if source.starts_with('-') {
        &source[1..]
    } else {
        &source[..]
    };
    let number = if s.starts_with("0x") {
        let to_parse = s
            .trim_start_matches("0x")
            .trim_start_matches('0')
            .replace('_', "");
        if to_parse.is_empty() {
            return Ok(0);
        }
        u32::from_str_radix(&to_parse, 16)
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    } else {
        s.replace('_', "")
            .parse::<u32>()
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u32: {:?}", err)))
    }?;
    Ok(number)
}

pub fn read_u64(token: &Token) -> Result<u64, Error> {
    let source = &token.source;
    let s = if source.starts_with('-') {
        &source[1..]
    } else {
        &source[..]
    };
    let number = if s.starts_with("0x") {
        let to_parse = s
            .trim_start_matches("0x")
            .trim_start_matches('0')
            .replace('_', "");
        if to_parse.is_empty() {
            return Ok(0);
        }
        u64::from_str_radix(&to_parse, 16)
            .map_err(|err| Error::new(Some(token.clone()), format!("Error parsing u64: {:?}", err)))
    } else {
        s.replace('_', "")
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
    // Module(Module),
    // AssertMalformed(AssertMalformed),
    // AssertInvalid(AssertInvalid),
    // AssertReturn(AssertReturn),
    // AssertTrap(AssertTrap),
}

pub fn parse_test_block<'a, I: Iterator<Item = &'a Token> + Clone>(
    peek_iter: &mut Peekable<I>,
    tokens: &[Token],
) -> Result<TestBlock, Error> {
    todo!()
    // let result = match get_next_keyword(peek_iter) {
    //     // Some(Keyword::Module) => Module::parse(peek_iter).map(TestBlock::Module),
    //     // Some(Keyword::AssertMalformed) => {
    //     //     AssertMalformed::parse(peek_iter).map(TestBlock::AssertMalformed)
    //     // }
    //     // Some(Keyword::AssertInvalid) => {
    //     //     AssertInvalid::parse(peek_iter).map(TestBlock::AssertInvalid)
    //     // }
    //     // Some(Keyword::AssertReturn) => AssertReturn::parse(peek_iter).map(TestBlock::AssertReturn),
    //     // Some(Keyword::AssertTrap) => AssertTrap::parse(peek_iter).map(TestBlock::AssertTrap),
    //     keyword => {
    //         return Err(Error::new(
    //             peek_iter.next().cloned(),
    //             format!("{keyword:?} was not expected as a top level directive in .wast files"),
    //         ))
    //     }
    // };
    // match result {
    //     Ok(block) => Ok(block),
    //     Err(err) => {
    //         let range = if let Some(token) = &err.token() {
    //             if let Some(index) = tokens.iter().position(|t| t == *token) {
    //                 if index == 0 {
    //                     tokens.get(0..=5).unwrap().to_vec()
    //                 } else {
    //                     tokens.get(index - 2..=index + 2).unwrap().to_vec()
    //                 }
    //             } else {
    //                 vec![]
    //             }
    //         } else {
    //             vec![]
    //         };

    //         println!("ERROR");
    //         println!("========================================");
    //         println!("{:?}", range);
    //         let tokens = range.iter().map(|t| t.source()).collect::<Vec<_>>();
    //         println!("Tokens around error: {:?}", tokens);
    //         println!("Parsing encounted error {:?}", err);
    //         println!("========================================");
    //         Err(err)
    //     }
    // }
}

// pub fn parse_module_test<'a, I: Iterator<Item = &'a Token> + Clone>(
//     tokens: &mut Peekable<I>,
//     passed: Option<&Module>,
// ) -> Result<Option<Module>, Error> {
//     let mut peek_iter = tokens.clone();
//     peek_iter.next().expect_left_paren()?;
//     let keyword = peek_iter.next().expect_keyword()?;

//     let mut module = None;
//     match keyword {
//         Keyword::Module => {
//             module = Some(Module::parse(tokens)?);
//         }
//         // // Keyword::AssertMalformed => AssertMalformed::parse(tokens)?.test()?,
//         // // Keyword::AssertInvalid => AssertInvalid::parse(tokens)?.test()?,
//         // // Keyword::AssertReturn if passed.is_some() => {
//         //     AssertReturn::parse(tokens)?.test(passed.as_ref().unwrap())?
//         // }
//         _ => {
//             return Err(Error::new(
//                 tokens.next().cloned(),
//                 format!("{keyword:?} was not expected as a top level directive in .wast files"),
//             ))
//         }
//     };
//     Ok(module)
// }
