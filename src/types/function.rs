use std::str::FromStr;

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

use super::{instruction::Instruction, parameter::Parameter, value::ValueType, Identifier};

#[derive(Debug)]
pub struct Frame<'a> {
    pub opcodes: &'a [Instruction],
    values: Vec<ValueType>,
    params: &'a [ValueType],
    results: &'a [ValueType],
    locals: &'a [ValueType],
}

impl<'a> Frame<'a> {
    pub fn set_result(&mut self) -> Result<Vec<ValueType>> {
        let mut vec = Vec::new();
        for result in self.results {
            let value = self
                .pop()
                .ok_or(WasmError::err("Stack is empty when getting results"))?;
            if result.type_equality(&value) {
                vec.push(value);
            } else {
                return Err(WasmError::err("Stack contained incorrect value"));
            }
        }
        Ok(vec)
    }

    pub fn push_param(&mut self, index: &usize) -> Result<()> {
        self.values.push(
            self.params
                .get(*index)
                .ok_or(WasmError::err(format!("Index out of bounds: {}", index)))?
                .clone(),
        );
        Ok(())
    }

    pub fn pop(&mut self) -> Option<ValueType> {
        self.values.pop()
    }

    pub(crate) fn push(&mut self, v: ValueType) {
        self.values.push(v)
    }
}

#[derive(Debug)]
pub struct Function {
    // id: u32
    // locals: Vec<ValueType>
    // body: Vec<Instruction>
    id: Identifier,
    parameters: Vec<ValueType>,
    locals: Vec<ValueType>,
    results: Vec<ValueType>,
    instructions: Vec<Instruction>,
}

impl Function {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Identifier::String(name.into()),
            parameters: Vec::new(),
            locals: Vec::new(),
            results: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn build(block: &Block) -> Result<Function> {
        if !block.type_id().is_function() {
            return Err(WasmError::err(format!(
                "expected function, found {}",
                block.type_id()
            )));
        }
        //TODO(Alec): I believe that a function can have no content. will need to double check that
        let mut content = block.content().unwrap_or("").to_string();

        //TODO(Alec): If there is no function name, then we still need to identify it in some way...
        let name = block.variable_name().unwrap_or("");
        let mut func = Self::new(name);

        for child in block.children() {
            let param = Parameter::build(child)?;
            match param.type_id() {
                BlockType::Parameter => {
                    if let Some(old) = param.name() {
                        let new = func.parameters.len().to_string();
                        content = content.replace(&old, &new);
                    }
                    func.parameters.push(param.value());
                }
                BlockType::Result => {
                    if let Some(old) = param.name() {
                        let new = func.results.len().to_string();
                        content = content.replace(&old, &new);
                    }
                    func.results.push(param.value());
                }
                BlockType::Local => {
                    if let Some(old) = param.name() {
                        let new = func.locals.len().to_string();
                        content = content.replace(&old, &new);
                    }
                    func.locals.push(param.value());
                }
                _ => {
                    panic!("This should neven execute as we already check the type when we build the parameter");
                }
            };
        }
        for line in content.lines() {
            func.instructions.push(Instruction::from_str(line)?)
        }
        Ok(func)
    }

    pub fn frame<'a>(&'a self, parameters: &'a [ValueType]) -> Result<Frame<'a>> {
        // 1. Check if the function has the same parameters
        if !ValueType::same_stack_types(&self.parameters, parameters) {
            return Err(WasmError::err(format!(
                "can't build frame with function because improper parameters were shared"
            )));
        }
        Ok(Frame {
            values: vec![],
            opcodes: &self.instructions,
            results: &self.results,
            params: parameters,
            locals: &self.locals,
        })
    }

    pub fn identifier(&self) -> Identifier {
        self.id.clone()
    }
}