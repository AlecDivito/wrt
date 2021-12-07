use std::convert::TryFrom;
use std::fmt::Display;
use std::str::FromStr;

use crate::block::Block;
use crate::block::BlockType;
use crate::error::Result;
use crate::error::WasmError;

use super::value::ValueType;

#[derive(Debug, Clone, PartialEq)]
pub enum Mutibility {
    Const(ValueType),
    Mut(ValueType),
}

impl Mutibility {
    pub fn set_value(self, value: &str) -> Result<Mutibility> {
        let new_value = ValueType::from_str(value)?;
        let old_value = match self {
            Mutibility::Const(v) => v,
            Mutibility::Mut(v) => v,
        };
        if old_value.type_equality(&new_value) {
            Ok(Mutibility::Const(new_value))
        } else {
            Err(WasmError::expected_type(old_value, new_value))
        }
    }
}

impl Display for Mutibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mutibility::Const(i) => write!(f, "{}", i),
            Mutibility::Mut(i) => write!(f, "(mut {})", i),
        }
    }
}

impl<'a> TryFrom<&Block<'a>> for Mutibility {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Mut)?;
        match block.content() {
            Some(content) => Ok(Mutibility::Mut(ValueType::from_str(content)?)),
            None => Err(WasmError::err(
                "global type expected to be mutable, but found no type",
            )),
        }
    }
}

impl FromStr for Mutibility {
    type Err = WasmError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut splits = s.trim().split(" ");
        let value_type = splits.next().ok_or(WasmError::err("expected type"))?;

        if let Some(split) = splits.next() {
            Ok(Mutibility::Const(
                ValueType::from_str(value_type)?.set_value(split)?,
            ))
        } else {
            Ok(Mutibility::Const(ValueType::from_str(value_type)?))
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    fn parse_block(string: &str) -> Result<Mutibility> {
        let mut source = crate::block::SubString::new(string);
        let block = Block::parse(&mut source)?;
        let mutibility = Mutibility::try_from(&block)?;
        Ok(mutibility)
    }

    fn parse_content(string: &str) -> Result<Mutibility> {
        Mutibility::from_str(string)
    }

    #[test]
    fn parse_mutibility_mut_i32() {
        let m = parse_block("(mut i32)").unwrap();
        assert_eq!(m, Mutibility::Mut(ValueType::I32(0)));
    }

    #[test]
    fn parse_mutibility_const_i32() {
        let m = parse_content("i32 100").unwrap();
        assert_eq!(m, Mutibility::Const(ValueType::I32(100)));
    }

    #[test]
    fn parse_mutibility_const_i32_without_value() {
        let m = parse_content("i32").unwrap();
        assert_eq!(m, Mutibility::Const(ValueType::I32(0)));
    }
}
