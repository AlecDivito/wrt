use super::{ast::Error, Location, Token};

#[derive(Debug)]
pub struct LexError {
    // spec_error
    pub location: Option<Location>,
    pub error: String,
    tokens: Option<Vec<Token>>,
}

impl std::error::Error for LexError {}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Scan Error: {:?} at location {:?}",
            self.error, self.location
        )
    }
}

impl From<LexError> for Error {
    fn from(value: LexError) -> Self {
        LexError::into(value)
    }
}

impl LexError {
    pub fn new(str: impl Into<String>) -> Self {
        Self {
            error: str.into(),
            location: None,
            tokens: None,
        }
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = Some(location);
        self
    }

    pub fn with_tokens(mut self, token: Vec<Token>) -> Self {
        self.tokens = Some(token);
        self
    }
}
