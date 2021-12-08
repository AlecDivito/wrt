use std::{convert::TryFrom, fmt::Display, str::FromStr};

use crate::{
    block::{Attribute, Block, BlockType, Identifier},
    error::{Result, WasmError},
};

use super::value::ValueType;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    id: Option<Identifier>,
    value_type: Vec<ValueType>,
}

impl FuncParam {
    /// Get a reference to the func param's value type.
    pub fn value_type(&self) -> &[ValueType] {
        self.value_type.as_ref()
    }

    /// Get a reference to the func param's id.
    pub fn id(&self) -> Option<&Identifier> {
        self.id.as_ref()
    }
}

impl<'a> TryFrom<&'a mut Block<'a>> for FuncParam {
    type Error = WasmError;

    fn try_from(block: &'a mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Parameter)?;

        let mut id = block.take_id();

        let param = match block.attribute_length() {
            0 => Err(WasmError::err("param expected at least 1 attirbute")),
            1 => Ok(Self {
                id,
                value_type: block.all_attributes_to_value_type()?,
            }),
            2 => {
                let mut value_type = vec![block.pop_attribute_as_value_type()?];
                match block.pop_attribute()? {
                    Attribute::Str(s) => {
                        value_type.push(ValueType::from_str(s)?);
                    }
                    Attribute::Num(n) => {
                        id.insert(Identifier::Number(n.parse::<usize>()?));
                    }
                };
                Ok(Self { id, value_type })
            }
            _ => {
                if id.is_none() {
                    Ok(Self {
                        id,
                        value_type: block.all_attributes_to_value_type()?,
                    })
                } else {
                    Err(WasmError::err(
                        "can not have multiple attributes and declare an id",
                    ))
                }
            }
        }?;

        block.should_be_empty()?;
        Ok(param)
    }
}

impl Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = if let Some(id) = &self.id {
            format!("{} {}", id, self.value_type[0])
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

#[derive(Debug, Clone, PartialEq)]
pub struct FuncResult {
    value_type: Vec<ValueType>,
}

impl<'a> TryFrom<&'a mut Block<'a>> for FuncResult {
    type Error = WasmError;

    fn try_from(block: &'a mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Result)?;
        let value_type = block.all_attributes_to_value_type()?;
        block.should_be_empty()?;
        Ok(Self { value_type })
    }
}

impl Display for FuncResult {
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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    parameters: Vec<FuncParam>,
    results: Vec<FuncResult>,
}

impl FunctionType {
    /// Get a reference to the function type's results.
    pub fn results(&self) -> &[FuncResult] {
        self.results.as_ref()
    }

    /// Get a reference to the function type's parameters.
    pub fn parameters(&self) -> &[FuncParam] {
        self.parameters.as_ref()
    }

    pub fn empty() -> Self {
        Self {
            parameters: Vec::new(),
            results: Vec::new(),
        }
    }

    pub fn try_from_block_allowing_other_children<'a>(block: &'a mut Block<'a>) -> Result<Self> {
        block.expect(BlockType::Function)?;
        let parameters = FunctionType::get_parameters(block)?;
        let results = FunctionType::get_results(block)?;

        Ok(Self {
            parameters,
            results,
        })
    }

    fn get_parameters(block: &mut Block) -> Result<Vec<FuncParam>> {
        block
            .take_children_that_are(BlockType::Parameter)
            .iter_mut()
            .map(|b| FuncParam::try_from(b))
            .collect::<Result<Vec<FuncParam>>>()
    }

    fn get_results(block: &mut Block) -> Result<Vec<FuncResult>> {
        block
            .take_children_that_are(BlockType::Parameter)
            .iter_mut()
            .map(|b| FuncResult::try_from(b))
            .collect::<Result<Vec<FuncResult>>>()
    }
}

impl<'a> TryFrom<&'a mut Block<'a>> for FunctionType {
    type Error = WasmError;

    fn try_from(block: &'a mut Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Function)?;
        let parameters = FunctionType::get_parameters(block)?;
        let results = FunctionType::get_results(block)?;
        block.should_be_empty()?;

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
        let mut block = parse("(func)").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        assert!(func.parameters.is_empty());
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_param() {
        let mut block = parse("(func (param i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(param.value_type.len(), 1);
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_multiple_params() {
        let mut block = parse("(func (param i32 i32 i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert!(param.id.is_none());
        assert_eq!(param.value_type.len(), 3);
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_param_and_string_id() {
        let mut block = parse("(func (param $id i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(param.value_type.len(), 1);
        assert_eq!(
            param.id.as_ref().unwrap(),
            &Identifier::String("$id".into())
        );
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_param_and_int_id() {
        let mut block = parse("(func (param 0 i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        let param = func.parameters.get(0).unwrap();
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(param.value_type.len(), 1);
        assert_eq!(param.id.as_ref().unwrap(), &Identifier::Number(0));
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_multiple_param_and_string_ids() {
        let mut block = parse("(func (param $id0 i32) (param $id1 i32) (param $id2 i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        for (i, param) in func.parameters.iter().enumerate() {
            assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
            assert_eq!(param.value_type.len(), 1);
            assert_eq!(
                param.id.as_ref().unwrap(),
                &Identifier::String(format!("$id{}", i))
            );
        }
        assert!(func.results.is_empty());
    }

    #[test]
    fn parse_function_type_with_result() {
        let mut block = parse("(func (result i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        let result = func.results.get(0).unwrap();
        assert_eq!(*result.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(result.value_type.len(), 1);
        assert!(func.parameters.is_empty());
    }

    #[test]
    fn parse_function_type_with_multiple_results_in_one_block() {
        let mut block = parse("(func (result i32 i32 i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        let result = func.results.get(0).unwrap();
        assert_eq!(result.value_type.len(), 3);
        assert!(func.parameters.is_empty());
    }

    #[test]
    fn parse_function_type_with_param_and_result() {
        let mut block = parse("(func (param i32) (result i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        assert_eq!(func.parameters.len(), 1);
        assert_eq!(func.results.len(), 1);
        let param = func.parameters.get(0).unwrap();
        let result = func.results.get(0).unwrap();
        assert_eq!(*result.value_type.get(0).unwrap(), ValueType::I32(0));
        assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        assert!(param.id.is_none());
    }

    #[test]
    fn parse_function_type_with_params_and_results() {
        let mut block = parse("(func (param i32) (param i32) (result i32) (result i32))").unwrap();
        let func = FunctionType::try_from(&mut block).unwrap();
        assert_eq!(func.parameters.len(), 2);
        assert_eq!(func.results.len(), 2);
        for (param, result) in func.parameters.iter().zip(&func.results) {
            assert_eq!(*result.value_type.get(0).unwrap(), ValueType::I32(0));
            assert_eq!(*param.value_type.get(0).unwrap(), ValueType::I32(0));
        }
    }
}
