use std::iter::Peekable;

use crate::structure::module::Module;

use super::{Keyword, Token, TokenType};

pub struct Ast {}

pub struct Binary {}

pub trait Parse<'a, I: Iterator<Item = &'a Token>>: Sized {
    fn parse(tokens: &mut Peekable<I>) -> Result<Self, Error>;
}

#[derive(Debug)]
pub struct Error {
    token: Option<Token>,
    error: String,
}

impl Error {
    pub fn new(token: Option<Token>, error: String) -> Self {
        Self { token, error }
    }
}

pub trait Expect<'a> {
    fn expect_left_paren(&self) -> Result<(), Error>;

    fn expect_right_paren(&self) -> Result<(), Error>;

    fn expect_number(&'a self) -> Result<&'a Token, Error>;

    fn expect_keyword_token(&self, ty: Keyword) -> Result<(), Error>;

    fn expect_keyword(&self) -> Result<Keyword, Error>;
}

pub trait TryGet {
    fn try_id(&self) -> Option<String>;
}

pub trait IntoResult<T> {
    fn into_result(self) -> Result<T, Error>;
}

impl<'a> IntoResult<&'a Token> for Option<&'a Token> {
    fn into_result(self) -> Result<&'a Token, Error> {
        if let Some(token) = self {
            Ok(token)
        } else {
            Err(Error {
                token: None,
                error: "Expected token".to_string(),
            })
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
}

impl<'a> Expect<'a> for Option<&'a Token> {
    fn expect_left_paren(&self) -> Result<(), Error> {
        if let Some(token) = self {
            if token.ty == TokenType::LeftParen {
                return Ok(());
            }
        }
        Err(Error {
            token: self.map(Clone::clone),
            error: "Expected left paren".to_string(),
        })
    }

    fn expect_right_paren(&self) -> Result<(), Error> {
        if let Some(token) = self {
            if token.ty == TokenType::LeftParen {
                return Ok(());
            }
        }
        Err(Error {
            token: self.map(Clone::clone),
            error: "Expected right paren".to_string(),
        })
    }

    fn expect_number(&self) -> Result<&'a Token, Error> {
        if let Some(token) = self {
            if token.ty == TokenType::Number {
                return Ok(token);
            }
        }
        Err(Error {
            token: self.map(Clone::clone),
            error: "Expected number".to_string(),
        })
    }

    fn expect_keyword_token(&self, ty: Keyword) -> Result<(), Error> {
        let keyword = self.expect_keyword()?;
        if keyword == ty {
            Ok(())
        } else {
            Err(Error {
                token: self.map(Clone::clone),
                error: format!("Expected keyword {:?}", ty),
            })
        }
    }

    fn expect_keyword(&self) -> Result<Keyword, Error> {
        if let Some(token) = self {
            if let TokenType::Keyword(key) = &token.ty {
                return Ok(key.clone());
            }
        }
        Err(Error {
            token: self.map(Clone::clone),
            error: "Expected keyword".to_string(),
        })
    }
}

pub fn read_u32(str: &Token) -> Result<u32, Error> {
    todo!();
}

pub fn parse(tokens: Vec<Token>) -> Result<Module, Error> {
    let mut iter = tokens.iter().peekable();
    Module::parse(&mut iter)
}
