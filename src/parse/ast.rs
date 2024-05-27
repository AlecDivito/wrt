use std::iter::Peekable;

use crate::structure::{module::Module, types::SignType};

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
    pub fn new(token: Option<Token>, error: impl ToString) -> Self {
        Self {
            token,
            error: error.to_string(),
        }
    }

    pub fn token(&self) -> Option<&Token> {
        self.token.as_ref()
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

    fn try_right_paran(&self) -> Option<()> {
        if (*self)?.ty == TokenType::RightParen {
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
        Err(Error {
            token: self.map(Clone::clone),
            error: "Expected left paren".to_string(),
        })
    }

    fn expect_right_paren(&self) -> Result<(), Error> {
        if let Some(token) = self {
            if token.ty == TokenType::RightParen {
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

    fn expect_string(&self) -> Result<String, Error> {
        if let Some(token) = self {
            if token.ty == TokenType::String {
                return Ok(token.source.clone());
            }
        }
        Err(Error {
            token: self.map(Clone::clone),
            error: "Expected string".to_string(),
        })
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

// pub fn read_sign<I: Iterator<Item = char>>(iter: Peekable<I>) -> Result<Option<SignType>, Error> {
//     match iter.peek() {
//        Some('+') => Ok(SignType::),
//        Some('-') => todo!(),
//        Some(_) => Ok(None),
//        None =>
//    }
// }

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

pub fn parse(tokens: &[Token]) -> Result<Module, Error> {
    let mut iter = tokens.iter().peekable();
    Module::parse(&mut iter)
}
