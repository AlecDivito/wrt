use std::convert::TryFrom;

use crate::{
    block::Block, error::WasmError, values::limit::Limit
};

struct Table {
    limit: Limit,
    id: Option<String>,
}

impl<'a> TryFrom<&Block<'a>> for Table {
    type Error = WasmError;

    fn try_from(value: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        todo!()
    }
}

#[cfg(test)]
mod test {

    use crate::{block::Block, error::WasmError};

    use super::*;

    fn parse(string: &str) -> Result<Table, WasmError> {
        let mut source = crate::block::SubString::new(string);
        let block = Block::parse(&mut source)?;
        let func = Table::try_from(&block)?;
        Ok(func)
    }

    #[test]
    fn table() {}
}
