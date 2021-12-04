use std::str::FromStr;

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

use super::{
    export::Export, instruction::Instruction, parameter::Parameter, value::ValueType, Identifier,
};

#[derive(Debug)]
pub struct Frame<'a> {
    pub opcodes: &'a [Instruction],
    values: Vec<ValueType>,
    params: Vec<ValueType>,
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
pub struct FunctionType {
    parameters: Vec<ValueType>,
    results: Vec<ValueType>,
}

impl FunctionType {
    pub fn new() -> Self {
        Self {
            parameters: Vec::new(),
            results: Vec::new(),
        }
    }

    pub fn add_param_type(&mut self, param: Parameter) {
        self.parameters.push(param.value().unwrap());
    }

    pub fn add_result_type(&mut self, param: Parameter) {
        self.results.push(param.value().unwrap());
    }

    pub fn add_param(&mut self, content: String, param: Parameter) -> Result<String> {
        let mut temp = content;
        if let Some(old) = param.id() {
            let new = self.parameters.len().to_string();
            temp = temp.replace(&old, &new);
        }
        self.parameters.push(param.value().unwrap());
        Ok(temp)
    }

    pub fn add_result(&mut self, content: String, param: Parameter) -> Result<String> {
        let mut temp = content;
        if let Some(old) = param.id() {
            let new = self.results.len().to_string();
            temp = temp.replace(&old, &new);
        }
        self.results.push(param.value().unwrap());
        Ok(temp)
    }

    fn compare_params(&self, parameters: &[ValueType]) -> bool {
        !ValueType::same_stack_types(&self.parameters, parameters)
    }
}

#[derive(Debug)]
pub struct Function {
    id: Identifier,
    types: FunctionType,
    locals: Vec<ValueType>,
    instructions: Vec<Instruction>,
    export: Option<Export>,
}

impl Function {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Identifier::String(name.into()),
            types: FunctionType::new(),
            instructions: Vec::new(),
            locals: Vec::new(),
            export: None,
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
        let name = *block.variable_name().get(0).unwrap_or(&"");
        let mut func = Self::new(name);

        for child in block.children() {
            let param = Parameter::build(child)?;
            match param.type_id() {
                BlockType::Parameter => content = func.types.add_param(content, param)?,
                BlockType::Result => content = func.types.add_result(content, param)?,

                BlockType::Local => {
                    if let Some(old) = param.id() {
                        let new = func.locals.len().to_string();
                        content = content.replace(&old, &new);
                    }
                    func.locals.push(param.value().unwrap());
                }
                BlockType::Export => {
                    if func.export.is_some() {
                        return Err(WasmError::err("can not export function more then once"));
                    }
                    func.export = Some(Export::make_function_export(param, func.id.clone()))
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
        if self.types.compare_params(parameters) {
            return Err(WasmError::err(format!(
                "can't build frame with function because improper parameters were shared"
            )));
        }
        Ok(Frame {
            values: vec![],
            opcodes: &self.instructions,
            results: &self.types.results,
            params: parameters.to_vec(),
            locals: &self.locals,
        })
    }

    pub fn from_frame<'a>(&'a self, frame: &'a Frame) -> Result<Frame<'a>> {
        // 1. Get the parameters that needs to be passed to the function
        let mut params = vec![];
        let mut values = frame.values.iter().rev();
        for _ in 0..self.types.parameters.len() {
            params.push(
                values
                    .next()
                    .ok_or(WasmError::err("Expected more items to fit on stack"))?
                    .clone(),
            );
        }
        params.reverse();

        // 2. Check if the call stack matches the parameters
        if self.types.compare_params(&params) {
            return Err(WasmError::err(format!(
                "can't build frame with function because improper parameters were shared"
            )));
        }

        // 3. Create the frame
        Ok(Frame {
            values: vec![],
            opcodes: &self.instructions,
            results: &self.types.results,
            params,
            locals: &self.locals,
        })
    }

    pub fn identifier(&self) -> Identifier {
        self.id.clone()
    }

    pub fn take_export(&mut self) -> Option<Export> {
        self.export.take()
    }
}
