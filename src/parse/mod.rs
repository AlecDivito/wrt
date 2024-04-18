
pub struct ParseError {}
impl ParseError {
    pub fn new() -> Self {
        Self {}
    }
}

pub enum TokenType {
    LeftParen,
    RightParen,
    UnsignedNumber,
    UnsignedHex,
    SignedNumber,
    SignedHex,
    String,
    Id,
    Reserved,
    Keyword
}

pub enum Whitespace {
    Space,
    NewLine
}

pub enum Comment {
    LineComment,
    BlockComment
}

pub struct Token<'a> {
    line: usize,
    column: usize,
    source: &'a str,
    ty: TokenType
}

pub struct Tokenizer {
    line: usize,
    column: usize
}

impl Tokenizer {
    pub fn parse(&mut self, source: &str) {
    }
}