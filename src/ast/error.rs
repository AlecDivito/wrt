use crate::lex::{Token, TokenType};

use super::tree::{Item, Node};

#[derive(Debug)]
pub enum Error {
    Unsupported {
        tried: TokenType,
    },
    Unexpected {
        expected: &'static str,
        found: Token,
    },
    Lex(crate::lex::ast::Error),
    IncompleteParse(Node),
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl From<crate::lex::ast::Error> for Error {
    fn from(error: crate::lex::ast::Error) -> Self {
        Self::Lex(error)
    }
}
