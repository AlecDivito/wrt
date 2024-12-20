use std::iter::Peekable;

use super::constants::*;
use super::Location;

#[derive(Debug)]
pub enum SoftToken {
    Char(char),
    Whitespace,
    NewLine,
}

impl SoftToken {
    pub fn as_char(&self) -> Option<&char> {
        if let Self::Char(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

// let hexnum = hexdigit ('_'? hexdigit)*

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
  ['\xe0'] ['\xa0'-'\xbf'] utf8cont
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
  utf8enc
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

pub enum NumberString {
    Hex(String),
    Int(String),
    Float(String),
}

pub struct BufferedReader<I: Iterator<Item = char>> {
    source: Peekable<I>,

    pub index: usize,
    pub line: usize,
    pub column: usize,
}

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

    pub fn peek_token(&mut self) -> Option<SoftToken> {
        match self.source.peek() {
            Some(character) => {
                if [NEW_LINE, NEW_LINE_2, CLCR].contains(character) {
                    Some(SoftToken::NewLine)
                } else if [SPACE, TAB].contains(character) {
                    Some(SoftToken::Whitespace)
                } else {
                    Some(SoftToken::Char(*character))
                }
            }
            None => None,
        }
    }

    pub fn read(&mut self) -> Option<char> {
        if let Some(char) = self.source.next() {
            self.index += 1;
            // If new line: https://doc.rust-lang.org/stable/std/io/trait.BufRead.html#method.read_line
            if char == NEW_LINE || char == NEW_LINE_2 || char == CLCR {
                self.line += 1;
                self.column = 0;
            } else if char == SPACE || char == TAB {
                self.column += 1;
            } else {
                self.column += 1;
            }
            Some(char)
        } else {
            None
        }
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

    pub fn location(&self) -> Location {
        Location {
            index: self.index,
            line: self.line,
            column: self.column,
        }
    }
}
