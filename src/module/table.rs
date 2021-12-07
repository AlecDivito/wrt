use std::convert::TryFrom;

use crate::{
    block::Block,
    error::{Result, WasmError},
    values::limit::Limit,
};

struct Table {
    id: Option<String>,
    limit: Limit,
    ref_type: 
}

impl<'a> TryFrom<&Block<'a>> for Table {
    type Error = WasmError;

    fn try_from(value: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        todo!()
    }
}

#[cfg(test)]
mod test {

    use crate::block::Block;

    use super::*;

    fn parse(string: &str) -> Result<Table> {
        let mut source = crate::block::SubString::new(string);
        let block = Block::parse(&mut source)?;
        let func = Table::try_from(&block)?;
        Ok(func)
    }

    #[test]
    fn table() {}
}
