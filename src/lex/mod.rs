pub mod ast;
mod constants;
mod error;
mod reader;
mod token;
mod ty;

pub use error::LexError;
use reader::BufferedReader;
pub use token::{Token, TokenType, Tokenizer};
pub use ty::Keyword;

#[derive(Debug)]
pub struct Location {
    index: usize,
    line: usize,
    column: usize,
}

pub fn tokenize(wasm: &str) -> Result<Vec<Token>, LexError> {
    let reader = BufferedReader::new(wasm.chars());
    let mut tokenizer = Tokenizer::new(reader);
    match tokenizer.lex() {
        Ok(_) => Ok(tokenizer.take()),
        Err(err) => Err(err
            .with_location(tokenizer.reader().location())
            .with_tokens(tokenizer.tokens().to_vec())),
    }
}
