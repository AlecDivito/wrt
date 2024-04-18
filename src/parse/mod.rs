use std::iter::Peekable;


pub struct ParseError {}
impl ParseError {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug)]
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

pub struct BufferedReader<I: Iterator<Item = char>> {
    source: Peekable<I>,

    index: usize,
    line: usize,
    column: usize,
}

pub fn parse(wasm: &str) -> Vec<Token> {
    let reader = BufferedReader::new(wasm.chars());
    let mut tokenizer = Tokenizer::new(reader);
    tokenizer.parse()
}

pub enum SoftToken {
    Char(char),
    Whitespace,
    NewLine,
}

const SPACE: char = ' ' as char;
const TAB: char = 0x09 as char;
const NEW_LINE: char = 0x0A as char;
const NEW_LINE_2: char = 0x0D as char;
const CLCR: char = /* 0x0D0A */ 0xDA as char;

impl<I> BufferedReader<I> where I: Iterator<Item = char> {
    pub fn new(source: I) -> Self {
        let source = source.peekable();
        Self {
            source,
            index: 0,
            line: 1,
            column: 0
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    pub fn next(&mut self) -> Option<SoftToken> {
        if let Some(char) = self.source.next() {
            self.index += 1;
            // If new line: https://doc.rust-lang.org/stable/std/io/trait.BufRead.html#method.read_line
            if char == NEW_LINE || char == NEW_LINE_2 || char == CLCR {
                self.line += 1;
                self.column = 0;
                Some(SoftToken::NewLine)
            } else if char == SPACE || char == TAB {
                self.column += 1;
                Some(SoftToken::Whitespace)
            } else {
                self.column += 1;
                Some(SoftToken::Char(char))
            }
        } else {
            None
        }
    }

    pub fn is_next(&mut self, char: impl Into<char>) -> bool {
        if let Some(next_char) = self.peek() {
            if *next_char == char.into() {
                return true
            }
        }
        false
    }

    pub fn eof(&mut self) -> bool {
        self.source.peek().is_none()
    }
}

#[derive(Debug)]
pub struct Token {
    line: usize,
    column: usize,
    index: usize,
    source: String,
    ty: TokenType
}

pub struct Tokenizer<I: Iterator<Item = char>> {
    reader: BufferedReader<I>
}

impl<I> Tokenizer<I> where I: Iterator<Item = char> {
    pub fn new(reader: BufferedReader<I>) -> Self {
        Self { reader }
    }

    pub fn parse(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(character) = self.reader.next() {
            let c = match character {
                SoftToken::Char(char) => char,
                _ => continue,
            };
            let token = match c {
                ';' if self.reader.is_next(';') => {
                    // Comment, read until end of line
                    self.read_until_new_line();
                    None
                } 
                '(' if self.reader.is_next(';') => {
                    // Comment thats large, read until closing comment
                    let _ = self.reader.next(); // TODO(Alec): .expect(';')
                    self.read_until_closing_comment(); // read until closing ';)'
                    None
                }
                '(' | ')' => Some(Token {
                    ty: if c == '(' { TokenType::LeftParen } else { TokenType::RightParen },
                    line: self.reader.line,
                    column: self.reader.column,
                    index: self.reader.index,
                    source: String::from(c),
                }),
                c if c.is_ascii_lowercase() => Some(self.read_keyword(c)),
                _ => None
            };
            if let Some(token) = token {
                tokens.push(token);
            }
        }
        tokens
    }

    fn read_keyword(&mut self, char: char) -> Token {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![char];
        while let Some(c) = self.reader.next() {
            match c {
                SoftToken::Char(c) if c.is_ascii_lowercase() => {
                    chars.push(c)
                },
                _ => break
            }
        }
        let source = chars.iter().collect::<String>();
        Token {
            line,
            column,
            index,
            source,
            ty: TokenType::Keyword,
        }
    }

    fn read_until_new_line(&mut self) {
        while let Some(c) = self.reader.next() {
            match c {
                SoftToken::NewLine => break,
                _ => continue
            }
        }
    }
    
    fn read_until_closing_comment(&mut self) {
        while let Some(c) = self.reader.next() {
            match c {
                SoftToken::Char(';') if self.reader.is_next(')') => {
                    let _ = self.reader.next(); // Todo(Alec): .expect(')')
                    break
                },
                _ => continue
            }
        }
    }
}