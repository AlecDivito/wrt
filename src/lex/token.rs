use std::str::FromStr;

use super::{
    constants::*,
    error::LexError,
    reader::{BufferedReader, NumberString, SoftToken},
    Keyword,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub source: String,
    pub ty: TokenType,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}\"{}\"", self.ty, self.source)
    }
}

impl Token {
    pub fn ty(&self) -> &TokenType {
        &self.ty
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Number,
    String,
    Id,
    Reserved,
    Keyword(Keyword),
}

impl TokenType {
    /// Returns `true` if the token type is [`LeftParen`].
    ///
    /// [`LeftParen`]: TokenType::LeftParen
    #[must_use]
    pub fn is_left_paren(&self) -> bool {
        matches!(self, Self::LeftParen)
    }

    /// Returns `true` if the token type is [`RightParen`].
    ///
    /// [`RightParen`]: TokenType::RightParen
    #[must_use]
    pub fn is_right_paren(&self) -> bool {
        matches!(self, Self::RightParen)
    }
}

pub struct Tokenizer<I: Iterator<Item = char>> {
    reader: BufferedReader<I>,
    tokens: Vec<Token>,
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(reader: BufferedReader<I>) -> Self {
        Self {
            reader,
            tokens: vec![],
        }
    }

    pub fn take(self) -> Vec<Token> {
        self.tokens
    }

    pub fn lex(&mut self) -> Result<(), LexError> {
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
                c if c.is_ascii_digit() || c == '+' || c == '-' => {
                    let line = self.reader.line;
                    let column = self.reader.column;
                    let index = self.reader.index;

                    let ty = TokenType::Number;
                    let source = self.read_until_delimiter(c);

                    Some(Token {
                        ty,
                        line,
                        column,
                        index,
                        source,
                    })
                }
                c if c.is_ascii_lowercase() => Some(self.read_keyword(c)?),

                _ => None,
            };
            if let Some(token) = token {
                self.tokens.push(token);
            }
        }
        Ok(())
    }

    fn read_hex_digit(
        &mut self,
        starting_char: Option<char>,
        is_hex: bool,
    ) -> Result<String, LexError> {
        let mut chars = if let Some(c) = starting_char {
            vec![c]
        } else {
            vec![]
        };
        while let Some(character) = self.reader.peek() {
            // First character must be a hex digit
            if !(HEX_DIGIT.contains(character) || *character == '_') {
                if chars.is_empty() {
                    return Err(LexError::new(format!(
                        "character {} is not valid. List of chars {:?}",
                        character, chars
                    )));
                } else {
                    break;
                }
            }
            match self.reader.pop() {
                Some(SoftToken::Char('_')) => chars.push('_'),
                Some(SoftToken::Char(c)) if HEX_DIGIT.contains(&c) && is_hex => chars.push(c),
                Some(SoftToken::Char(c)) if DIGIT.contains(&c) && !is_hex => chars.push(c),
                char => {
                    let str = chars.iter().collect::<String>();
                    let err = format!(
                        "Parsing Hex Number {} failed when encountered {:?}",
                        str, char
                    );
                    return Err(LexError::new(err));
                }
            }
        }
        Ok(chars.iter().collect::<String>())
    }

    fn read_hex_number(&mut self, is_hex: bool) -> Result<NumberString, LexError> {
        let mut chars = vec![];
        while let Some(character) = self.reader.peek() {
            // First character must be a hex digit
            if !HEX_DIGIT.contains(character) {
                if chars.is_empty() {
                    return Err(LexError::new(format!(
                        "character {} is not valid. List of chars {:?}",
                        character, chars
                    )));
                } else {
                    break;
                }
            }
            match self.reader.pop() {
                Some(SoftToken::Char('_')) => continue,
                Some(SoftToken::Char(c)) if HEX_DIGIT.contains(&c) => chars.push(c),
                char => {
                    let str = chars.iter().collect::<String>();
                    let err = format!(
                        "Parsing Hex Number {} failed when encountered {:?}",
                        str, char
                    );
                    return Err(LexError::new(err));
                }
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

    fn read_string(&mut self) -> Result<Token, LexError> {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;

        let mut chars = vec![];
        // Strings represent both textual and binary data. They are enclosed in
        // quotations ("), and can contain anything expect control characters or
        // backslashed (unless used for escape character)
        while let Some(c) = self.reader.read() {
            if c == '"' {
                break;
            } else if c.is_ascii_control() {
                return Err(LexError::new(
                    "Failed to read string because it contains a control character",
                ));
            } else if c == '\\' {
                match self.reader.read() {
                    Some('n') => chars.push('\x0a'),
                    Some('r') => chars.push('\x0d'),
                    Some('\\') => chars.push('\\'),
                    Some('\'') => chars.push('\''),
                    Some('\"') => chars.push('\"'),
                    Some('u') => {
                        let _ = self.read_hex_number(false)?;
                        panic!("hi, please give me the code that errored here, thanks.")
                    }
                    Some(num1) if num1.is_ascii_hexdigit() => {
                        use std::convert::TryFrom;
                        let num1 = num1.to_digit(16).unwrap();
                        let _ = self.reader.read().unwrap().to_digit(16).unwrap();
                        let number = u8::try_from(16 * num1 + num1).unwrap();
                        chars.push(number as char);
                    }
                    Some(char) => {
                        let string = chars.iter().collect::<String>();
                        format!(
                            "Was reading string {} and encountered invalid escape character \\{:?}",
                            string, char
                        );
                        return Err(LexError::new(string));
                    }
                    _ => panic!(),
                }
            } else {
                chars.push(c)
            }
        }
        let source = chars.iter().collect::<String>();
        Ok(Token {
            line,
            column,
            index,
            source,
            ty: TokenType::String,
        })
    }

    fn read_id(&mut self) -> Token {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![];
        while let Some(c) = self.reader.peek_token() {
            match c {
                SoftToken::Char(c) if c.is_ascii_alphanumeric() || ID_CHAR.contains(&c) => {
                    let _ = self.reader.pop();
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

    fn read_keyword(&mut self, char: char) -> Result<Token, LexError> {
        let line = self.reader.line;
        let index = self.reader.index;
        let column = self.reader.column;
        let mut chars = vec![char];
        while let Some(peek) = self.reader.peek() {
            if peek.is_ascii_alphanumeric() || ID_CHAR.contains(peek) {
                chars.push(*self.reader.pop().unwrap().as_char().unwrap())
            } else {
                break;
            }
        }
        let source = chars.iter().collect::<String>();
        let ty = Keyword::from_str(&source)?;
        Ok(Token {
            line,
            column,
            index,
            source,
            ty: TokenType::Keyword(ty),
        })
    }

    fn read_until_delimiter(&mut self, first: char) -> String {
        let mut tokens = vec![first];
        while let Some(c) = self.reader.peek() {
            if c.is_ascii_hexdigit() || ALL_NUMBER.contains(c) {
                tokens.push(self.reader.read().unwrap());
            } else {
                break;
            }
        }
        tokens.iter().collect::<String>()
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
        let mut counter = 0;
        while let Some(c) = self.reader.pop() {
            match c {
                SoftToken::Char('(') if self.reader.is_next(';') => {
                    counter += 1;
                    self.reader.pop();
                }
                SoftToken::Char(';') if self.reader.is_next(')') => {
                    let _ = self.reader.pop(); // Todo(Alec): .expect(')')
                    if counter == 0 {
                        break;
                    } else {
                        counter -= 1;
                    }
                }
                _ => continue,
            }
        }
    }

    pub fn reader(&self) -> &BufferedReader<I> {
        &self.reader
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }
}
