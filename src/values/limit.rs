use std::{convert::TryFrom, str::FromStr};

use crate::{block::Block, error::WasmError};

#[derive(Debug, Clone, PartialEq)]
pub struct Limit {
    min: u32,
    max: Option<u32>,
}

impl Limit {
    pub fn new(min: u32, max: Option<u32>) -> Self {
        Self { min, max }
    }

    pub fn max(min: u32, max: u32) -> Self {
        Self {
            min,
            max: Some(max),
        }
    }

    pub fn min(min: u32) -> Self {
        Self { min, max: None }
    }
}

impl<'a> TryFrom<&mut &mut Block<'a>> for Limit {
    type Error = WasmError;

    fn try_from(block: &mut &mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        if let Ok(first) = block.pop_attribute() {
            let max = first.as_num()?.parse::<u32>()?;
            if let Ok(attr) = block.pop_attribute() {
                let min = attr.as_num()?.parse::<u32>()?;
                Ok(Self::max(min, max))
            } else {
                Ok(Self::min(max))
            }
        } else if let Some(c) = block.take_content() {
            Limit::from_str(&c)
        } else {
            Err(WasmError::err("limits not found in content or attributes"))
        }
    }
}

impl FromStr for Limit {
    type Err = WasmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(" ");

        let min = parts
            .next()
            .ok_or(WasmError::err("failed to parse limit as no number exists"))?
            .parse::<u32>()
            .map_err(|_| {
                WasmError::err(format!(
                    "expected number, recieved {}",
                    parts.nth(0).unwrap()
                ))
            })?;

        let max = match parts.next() {
            Some(str) => Some(str.parse::<u32>().map_err(|_| {
                WasmError::err(format!(
                    "expected number, recieved {}",
                    parts.nth(0).unwrap()
                ))
            })?),
            None => None,
        };

        Ok(Self { min, max })
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn limit_without_max() {
        let limit: Limit = "0".parse().unwrap();
        assert_eq!(limit.min, 0);
        assert!(limit.max.is_none())
    }

    #[test]
    fn limit_with_max() {
        let limit: Limit = "0 10".parse().unwrap();
        assert_eq!(limit.min, 0);
        assert_eq!(limit.max, Some(10));
    }

    // fn limit_with_min_as_characters() {
    //     assert!("limit?".parse::<Limit>().is_err())
    // }
}
