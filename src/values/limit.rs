use std::str::FromStr;

use crate::error::WasmError;

pub struct Limit {
    min: u32,
    max: Option<u32>,
}

impl FromStr for Limit {
    type Err = WasmError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split(" ");

        let min = parts
            .next()
            .ok_or(WasmError::err("failed to parse limit as no number exists"))?
            .parse::<u32>()
            .map_err(|e| {
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

    fn limit_with_min_as_characters() {
        assert!("limit?".parse::<Limit>().is_err())
    }
}
