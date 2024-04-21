use std::iter::Peekable;

#[derive(Debug)]
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
    Keyword,
}

pub enum Whitespace {
    Space,
    NewLine,
}

pub enum Comment {
    LineComment,
    BlockComment,
}

pub struct BufferedReader<I: Iterator<Item = char>> {
    source: Peekable<I>,

    index: usize,
    line: usize,
    column: usize,
}

pub fn parse(wasm: &str) -> Result<Vec<Token>, ParseError> {
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
const ID_CHAR: [char; 23] = [
    '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '_',
    '`', '|', '~', '\'',
];
const ESCAPE_CHARACTERS: [char; 6] = ['n', 'r', 't', '\\', '\'', '\"'];
const DIGIT: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const HEX_DIGIT: [char; 22] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9',
];
// let hexnum = hexdigit ('_'? hexdigit)*

enum NumberString {
    Hex(String),
    Int(String),
    Float(String),
}

/*
let sign = '+' | '-'
let digit = ['0'-'9']
let hexdigit = ['0'-'9''a'-'f''A'-'F']
let num = digit ('_'? digit)*
let hexnum = hexdigit ('_'? hexdigit)*

let letter = ['a'-'z''A'-'Z']
let symbol =
  ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`''.''\'']

let ascii_newline = ['\x0a''\x0d']
let newline = ascii_newline | "\x0a\x0d"
let space = [' ''\x09''\x0a''\x0d']
let control = ['\x00'-'\x1f'] # space
let ascii = ['\x00'-'\x7f']
let ascii_no_nl = ascii # ascii_newline
let utf8cont = ['\x80'-'\xbf']
let utf8enc =
    ['\xc2'-'\xdf'] utf8cont
  | ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xed'] ['\x80'-'\x9f'] utf8cont
  | ['\xe1'-'\xec''\xee'-'\xef'] utf8cont utf8cont
  | ['\xf0'] ['\x90'-'\xbf'] utf8cont utf8cont
  | ['\xf4'] ['\x80'-'\x8f'] utf8cont utf8cont
  | ['\xf1'-'\xf3'] utf8cont utf8cont utf8cont
let utf8 = ascii | utf8enc
let utf8_no_nl = ascii_no_nl | utf8enc

let escape = ['n''r''t''\\''\'''\"']
let character =
    [^'"''\\''\x00'-'\x1f''\x7f'-'\xff']
  | utf8enc
  | '\\'escape
  | '\\'hexdigit hexdigit
  | "\\u{" hexnum '}'

let nat = num | "0x" hexnum
let int = sign nat
let frac = num
let hexfrac = hexnum
let float =
    sign? num '.' frac?
  | sign? num ('.' frac?)? ('e' | 'E') sign? num
  | sign? "0x" hexnum '.' hexfrac?
  | sign? "0x" hexnum ('.' hexfrac?)? ('p' | 'P') sign? num
  | sign? "inf"
  | sign? "nan"
  | sign? "nan:" "0x" hexnum
let string = '"' character* '"'

let idchar = letter | digit | '_' | symbol
let name = idchar+
let id = '$' name

let keyword = ['a'-'z'] (letter | digit | '_' | '.' | ':')+
let reserved = (idchar | string)+ | ',' | ';' | '[' | ']' | '{' | '}'

let ixx = "i" ("32" | "64")
let fxx = "f" ("32" | "64")
let nxx = ixx | fxx
let vxxx = "v128"
let mixx = "i" ("8" | "16" | "32" | "64")
let mfxx = "f" ("32" | "64")
let sign = "s" | "u"
let mem_size = "8" | "16" | "32"
let v128_int_shape = "i8x16" | "i16x8" | "i32x4" | "i64x2"
let v128_float_shape = "f32x4" | "f64x2"
let v128_shape = v128_int_shape | v128_float_shape
*/

impl<I> BufferedReader<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(source: I) -> Self {
        let source = source.peekable();
        Self {
            source,
            index: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.source.peek()
    }

    pub fn pop(&mut self) -> Option<SoftToken> {
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
                return true;
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
    ty: TokenType,
}

pub struct Tokenizer<I: Iterator<Item = char>> {
    reader: BufferedReader<I>,
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(reader: BufferedReader<I>) -> Self {
        Self { reader }
    }

    pub fn parse(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut tokens = vec![];
        while let Some(character) = self.reader.pop() {
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
                    let _ = self.reader.pop(); // TODO(Alec): .expect(';')
                    self.read_until_closing_comment(); // read until closing ';)'
                    None
                }
                '(' | ')' => Some(Token {
                    ty: if c == '(' {
                        TokenType::LeftParen
                    } else {
                        TokenType::RightParen
                    },
                    line: self.reader.line,
                    column: self.reader.column,
                    index: self.reader.index,
                    source: String::from(c),
                }),
                '$' => Some(self.read_id()),
                '"' => Some(self.read_string()?),
                c if c.is_ascii_lowercase() => Some(self.read_keyword(c)),

                _ => None,
            };
            if let Some(token) = token {
                tokens.push(token);
            }
        }
        Ok(tokens)
    }

    fn read_hex_number(&mut self, is_hex: bool) -> Result<NumberString, ParseError> {
        let mut chars = vec![];
        while let Some(character) = self.reader.peek() {
            // First character must be a hex digit
            if !HEX_DIGIT.contains(character) {
                return Err(ParseError::new());
            }
            match self.reader.pop() {
                Some(SoftToken::Char('_')) => continue,
                Some(SoftToken::Char(c)) if HEX_DIGIT.contains(&c) => chars.push(c),
                _ => return Err(ParseError::new()),
            }
        }
        let string = chars.iter().collect::<String>();
        if is_hex {
            Ok(NumberString::Hex(string))
        } else if string.chars().all(|c| DIGIT.contains(&c)) {
            Ok(NumberString::Int(string))
        } else {
            Ok(NumberString::Hex(string))
        }
    }

    fn read_string(&mut self) -> Result<Token, ParseError> {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;

        let mut chars = vec![];
        // Strings represent both textual and binary data. They are enclosed in
        // quotations ("), and can contain anything expect control characters or
        // backslashed (unless used for escape character)
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::Char(c) if c == '"' => break,
                SoftToken::Char(c) if c.is_ascii_control() => return Err(ParseError::new()),
                SoftToken::Char(c) if c == '\\' => match self.reader.pop() {
                    Some(SoftToken::Char('n')) => chars.push('\x0a'),
                    Some(SoftToken::Char('r')) => chars.push('\x0d'),
                    Some(SoftToken::Char('\\')) => chars.push('\\'),
                    Some(SoftToken::Char('\'')) => chars.push('\''),
                    Some(SoftToken::Char('\"')) => chars.push('\"'),
                    Some(SoftToken::Char('u')) => {
                        let number = self.read_hex_number(false)?;
                        panic!("hi, please give me the code that errored here, thanks.")
                    }
                    Some(SoftToken::Char(c)) => {
                        let number =
                            if c == '0' && self.reader.peek().map(|c| *c == 'x').unwrap_or(false) {
                                self.read_hex_number(true)?
                            } else {
                                self.read_hex_number(false)?
                            };
                        panic!("Hi, please tell me where i failed :) ")
                    }
                    _ => return Err(ParseError::new()),
                },
                SoftToken::Char(c) => chars.push(c),
                _ => break,
            }
        }
        let source = chars.iter().collect::<String>();
        Ok(Token {
            line,
            column,
            index,
            source,
            ty: TokenType::Id,
        })
    }

    fn read_id(&mut self) -> Token {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![];
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::Char(c) if c.is_ascii_alphanumeric() || ID_CHAR.contains(&c) => {
                    chars.push(c)
                }
                _ => break,
            }
        }
        let source = chars.iter().collect::<String>();
        Token {
            line,
            column,
            index,
            source,
            ty: TokenType::Id,
        }
    }

    fn read_keyword(&mut self, char: char) -> Token {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![char];
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::Char(c) if c.is_ascii_lowercase() => chars.push(c),
                _ => break,
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
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::NewLine => break,
                _ => continue,
            }
        }
    }

    fn read_until_closing_comment(&mut self) {
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::Char(';') if self.reader.is_next(')') => {
                    let _ = self.reader.pop(); // Todo(Alec): .expect(')')
                    break;
                }
                _ => continue,
            }
        }
    }
}
