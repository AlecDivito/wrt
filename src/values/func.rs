use std::{convert::TryFrom, fmt::Display};

use crate::{
    block::{Block, BlockType},
    error::WasmError,
};

use super::value::ValueType;

#[derive(Debug)]
pub struct Param {
    id: Option<String>,
    value_type: Vec<ValueType>,
}

impl<'a> TryFrom<&Block<'a>> for Param {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Result)?;
        let id = block.try_identity()?;
        let value_type = match id {
            Some(_) => vec![block.value_type()?],
            None => block.value_types()?,
        };
        Ok(Self { id, value_type })
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = if self.value_type.len() == 1 {
            let value_type = self.value_type.pop().unwrap().type_id_string();
            if let Some(id) = self.id {
                format!("{} {}", id, value_type)
            } else {
                value_type
            }
        } else {
            self.value_type
                .iter()
                .map(|v| v.type_id_string())
                .collect::<Vec<String>>()
                .join(" ")
        };
        write!(f, "(result {})", content)
    }
}

#[derive(Debug)]
pub struct Result {
    value_type: Vec<ValueType>,
}

impl<'a> TryFrom<&Block<'a>> for Result {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Result)?;
        let value_type = block.value_types()?;
        Ok(Self { value_type })
    }
}

impl Display for Result {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let types = self
            .value_type
            .iter()
            .map(|v| v.type_id_string())
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "(result {})", types)
    }
}

#[derive(Debug)]
pub struct FunctionType {
    parameters: Vec<Param>,
    results: Vec<Result>,
}

impl FunctionType {
    /// Get a reference to the function type's results.
    pub fn results(&self) -> &[Result] {
        self.results.as_ref()
    }

    /// Get a reference to the function type's parameters.
    pub fn parameters(&self) -> &[Param] {
        self.parameters.as_ref()
    }
}

impl<'a> TryFrom<&Block<'a>> for FunctionType {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Function)?;

        let mut parameters = Vec::new();
        let mut results = Vec::new();

        for child in block.children() {
            match child.type_id() {
                BlockType::Parameter => parameters.push(Param::try_from(child)?),
                BlockType::Result => results.push(Result::try_from(child)?),
                _ => {
                    return Err(WasmError::expected(
                        &[BlockType::Parameter, BlockType::Result],
                        child.type_id(),
                    ))
                }
            }
        }

        Ok(Self {
            parameters,
            results,
        })
    }
}

fn to_string<F: Display>(m: &Vec<F>) -> String {
    m.iter()
        .map(|b| b.to_string())
        .collect::<Vec<String>>()
        .join(" ")
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(func {} {})",
            to_string(&self.parameters),
            to_string(&self.results)
        )
    }
}

#[cfg(test)]
mod test {

    use crate::block::SubString;
    use crate::error::Result;

    use super::*;

    fn parse(program: &str) -> Result<Block> {
        let mut source = SubString::new(program);
        Block::parse(&mut source)
    }

    #[test]
    fn parse_function_type() {
        let block = parse("(func)").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        assert!(func.parameters.is_empty());
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_param() {
        let block = parse("(func (param i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(param.value_type.len(), 1);
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_param_and_id() {
        let block = parse("(func (param $id i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(param.value_type.len(), 1);
        assert_eq!(param.id.unwrap(), "$id");
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_multiple_param_and_ids() {
        let block = parse("(func (param $id0 i32) (param $id1 i32) (param $id2 i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        for (i, param) in func.parameters.iter().enumerate() {
            assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
            assert_eq!(param.value_type.len(), 1);
            assert_eq!(param.id.unwrap(), format!("$id{}", i));
        }
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_multiple_params_in_one_block() {
        let block = parse("(func (param i32 i32 i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert!(param.id.is_none());
        assert_eq!(param.value_type.len(), 3);
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_result() {
        let block = parse("(func (result i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        let result = func.results.get(0).unwrap();
        assert_eq!(*result.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(result.value_type.len(), 1);
        assert!(func.parameters.is_empty());
    }

    #[test]
    fn parse_function_type_with_multiple_results_in_one_block() {
        let block = parse("(func (result i32 i32 i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        let result = func.results.get(0).unwrap();
        assert_eq!(result.value_type.len(), 3);
        assert!(func.parameters.is_empty());
    }

    #[test]
    fn parse_function_type_with_param_and_result() {
        let block = parse("(func (param i32) (result i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        assert_eq!(func.parameters.len(), 1);
        assert_eq!(func.results.len(), 1);
        let param = func.parameters.get(0).unwrap();
        let result = func.results.get(0).unwrap();
        assert_eq!(*result.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
    }

    #[test]
    fn parse_function_type_with_params_and_results() {
        let block = parse("(func (param i32) (param i32) (result i32) (result i32))").unwrap();
        let func = FunctionType::try_from(&block).unwrap();
        assert_eq!(func.parameters.len(), 2);
        assert_eq!(func.results.len(), 2);
        for (param, result) in func.parameters.iter().zip(&func.results) {
            assert_eq!(*result.value_type.get(0).unwrap(), ValueType::I32(0));
            assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        }
    }
}
