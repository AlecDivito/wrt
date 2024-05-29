use std::{convert::TryInto, iter::Peekable, string::ParseError};

use crate::{
    execution::Number,
    structure::{
        module::Module,
        types::{AssertInvalid, AssertMalformed, NumType, SignType},
    },
};

use super::{Keyword, Token, TokenType};

pub struct Ast {}

pub struct Binary {}

pub trait Parse<'a, I: Iterator<Item = &'a Token>>: Sized {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error>;
}

#[derive(Debug)]
pub enum ErrorTy {
    UnexpectedToken,
}

impl ToString for ErrorTy {
    fn to_string(&self) -> String {
        match self {
            ErrorTy::UnexpectedToken => "unexpected token",
        }
        .to_string()
    }
}

#[derive(Debug)]
pub struct Error {
    ty: ErrorTy,
    token: Option<Token>,
    error: String,
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

impl Into<Error> for ParseError {
    fn into(self) -> Error {
        Error::new(
            None,
            format!("Failed to parse wat tokens. ERROR: {:?}", self),
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

pub fn read_number(expected: NumType, token: &Token) -> Result<Number, Error> {
    Ok(match expected {
        NumType::I32 => Number::I32(read_u32(token)?.try_into().unwrap()),
        NumType::I64 => Number::I64(read_u64(token)?.try_into().unwrap()),
        NumType::F32 => todo!(),
        NumType::F64 => todo!(),
    })
}

pub fn read_u32(token: &Token) -> Result<u32, Error> {
    let number = if token.source.starts_with("0x") {
        u32::from_str_radix(token.source.trim_start_matches("0x"), 16)
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

pub fn parse_module_test<'a, I: Iterator<Item = &'a Token> + Clone>(
    tokens: &mut Peekable<I>,
) -> Result<(), Error> {
    let mut peek_iter = tokens.clone();
    let _ = peek_iter.next().expect_left_paren()?;
    let keyword = peek_iter.next().expect_keyword()?;

    match keyword {
        Keyword::Module => Module::parse(tokens).map(|_| ()),
        Keyword::AssertMalformed => AssertMalformed::parse(tokens)?.test(),
        Keyword::AssertInvalid => AssertInvalid::parse(tokens)?.test(),
        _ => Err(Error::new(
            tokens.next().cloned(),
            format!("{keyword:?} was not expected as a top level directive in .wast files"),
        )),
    }
}

#[derive(Debug)]
pub struct Tee {
    root: Keyword,
    attributes: Vec<Token>,
    children: Vec<Tee>,
}

pub fn parse_module_test_2<'a, I: Iterator<Item = &'a Token> + Clone>(
    tokens: &mut Peekable<I>,
) -> Result<Tee, Error> {
    tokens.next().expect_left_paren()?;
    let root = tokens.next().expect_keyword()?;

    let mut attributes = vec![];
    while tokens.peek().copied().try_left_paran().is_none()
        && tokens.peek().copied().try_right_paran().is_none()
    {
        attributes.push(tokens.next().unwrap().clone())
    }

    let mut children = vec![];
    while tokens.peek().copied().try_left_paran().is_some() {
        children.push(parse_module_test_2(tokens)?);
    }

    tokens.next().expect_right_paren()?;

    Ok(Tee {
        root,
        attributes,
        children,
    })
}

pub fn print_tee(tee: Tee) {
    print_tee_with_indentation(tee, 0);
}

fn print_tee_with_indentation(tee: Tee, indent: usize) {
    print!("{}", " ".repeat(indent));
    print!("({:?}", tee.root);
    for attr in tee.attributes {
        print!(" {}", attr.source);
    }
    if tee.children.is_empty() {
        println!(") ");
    } else {
        println!();
        for children in tee.children {
            print_tee_with_indentation(children, indent + 1)
        }
        println!("{})", " ".repeat(indent));
    }
}
