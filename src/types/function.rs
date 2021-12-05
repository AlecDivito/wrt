use std::{collections::HashMap, str::FromStr};

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

use super::{
    export::Export, import::Import, instruction::Instruction, module::Module, parameter::Parameter,
    value::ValueType, Identifier,
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
pub struct FuncImpl {
    locals: Vec<ValueType>,
    instructions: Vec<Instruction>,
}

impl FuncImpl {
    pub fn new(locals: Vec<ValueType>, content: String) -> Result<Self> {
        let instructions = content
            .lines()
            .map(Instruction::from_str)
            .collect::<Result<Vec<Instruction>>>()?;
        Ok(Self {
            locals,
            instructions,
        })
    }
}

#[derive(Debug)]
pub enum FunctionPointer {
    Impl(FuncImpl),
    Import(Import),
}

#[derive(Debug)]
pub struct Function {
    id: Identifier,
    parameters: Vec<ValueType>,
    results: Vec<ValueType>,
    function: FunctionPointer,
    export: Option<Export>,
}

impl Function {
    fn block_check(block: &Block) -> Result<()> {
        if !block.type_id().is_function() {
            return Err(WasmError::err(format!(
                "expected function, found {}",
                block.type_id()
            )));
        }
        Ok(())
    }

    fn block_id(block: &Block) -> Identifier {
        //TODO(Alec): If there is no function name, then we still need to identify it in some way...
        // as a solution, we could pass the identifier into the function...
        Identifier::String(block.variable_name().get(0).unwrap_or(&"").to_string())
    }

    fn block_params(block: &Block) -> Result<Vec<Parameter>> {
        block
            .children()
            .iter()
            .map(Parameter::build)
            .collect::<Result<Vec<Parameter>>>()
    }

    pub fn import_block(block: &Block, import: Import) -> Result<Self> {
        Function::block_check(block)?;
        let id = Function::block_id(block);
        let params = Function::block_params(block)?;
        // from my understanding, for the import statement, we can only accept
        // parameter and result statements which means no re-export
        let mut parameters = vec![];
        let mut results = vec![];
        for mut param in params {
            match param.type_id() {
                BlockType::Parameter => parameters.append(param.value()),
                BlockType::Result => results.append(param.value()),
                _ => {
                    return Err(WasmError::err(format!(
                        "import func doesn't support parameter {:?}",
                        param
                    )))
                }
            };
        }
        Ok(Self {
            id,
            parameters,
            results,
            function: FunctionPointer::Import(import),
            export: None, // TODO(Alec): Can we re-export an import???
        })
    }

    pub fn impl_block(block: &Block) -> Result<Function> {
        Function::block_check(block)?;
        let id = Function::block_id(block);
        let params = Function::block_params(block)?;

        let mut parameters = vec![];
        let mut results = vec![];
        let mut locals = vec![];
        let mut export = None;

        //TODO(Alec): I believe that a function can have no content. will need to double check that
        let mut content = block.content().unwrap_or("").to_string();

        for mut param in params {
            match param.type_id() {
                BlockType::Parameter => {
                    if let Some(old) = param.id() {
                        content = content.replace(&old, &parameters.len().to_string());
                    }
                    parameters.append(param.value());
                }
                BlockType::Result => {
                    if let Some(old) = param.id() {
                        content = content.replace(&old, &results.len().to_string());
                    }
                    results.append(param.value());
                }
                BlockType::Local => {
                    if let Some(old) = param.id() {
                        content = content.replace(&old, &locals.len().to_string());
                    }
                    locals.append(param.value());
                }
                BlockType::Export => {
                    if export.is_some() {
                        return Err(WasmError::err("can not export function more then once"));
                    }
                    export = Some(Export::make_function_export(param, id.clone()))
                }
                _ => {
                    panic!("This should neven execute as we already check the type when we build the parameter");
                }
            };
        }

        Ok(Self {
            id,
            parameters,
            results,
            function: FunctionPointer::Impl(FuncImpl::new(locals, content)?),
            export,
        })
    }

    fn compare_params(&self, parameters: &[ValueType]) -> bool {
        !ValueType::same_stack_types(&self.parameters, parameters)
    }

    fn function_impl<'a>(
        &'a self,
        modules: &'a HashMap<String, Module>,
    ) -> Result<(&'a [ValueType], &'a [Instruction])> {
        match &self.function {
            FunctionPointer::Impl(i) => Ok((&i.locals, &i.instructions)),
            FunctionPointer::Import(i) => {
                let module = modules
                    .get(i.module())
                    .ok_or(WasmError::err("module that was required wasn't imported"))?;
                let export = module.export(i.name()).ok_or(WasmError::err(
                    "module was imported but required function wasn't",
                ))?;
                let func_id = export
                    .export_type()
                    .function_id()
                    .ok_or(WasmError::err(format!(
                        "failed to find exported function {} in program",
                        i.name()
                    )))?;
                let function = module.function(func_id).ok_or(WasmError::err(format!(
                    "failed to find exported function with identifier {:?}",
                    func_id
                )))?;
                function.function_impl(modules)
            }
        }
    }

    /// Assuming that this function can not be an imported function, we create
    /// a frame using our current knowledge
    pub fn frame<'a>(&'a self, parameters: &'a [ValueType]) -> Result<Frame<'a>> {
        // 1. Check if the function has the same parameters
        if self.compare_params(parameters) {
            return Err(WasmError::err(format!(
                "can't build frame with function because improper parameters were shared"
            )));
        }
        // 2. Get the instructions for the function
        let (locals, instructions) = match &self.function {
            FunctionPointer::Impl(i) => Ok((&i.locals, &i.instructions)),
            FunctionPointer::Import(_) => {Err(WasmError::err("We can't import a function when we are creating a frame. This shouldn't ever happen and if it does, we may need to change the implmentation again..."))},
        }?;

        // 3. Create the frame
        Ok(Frame {
            values: vec![],
            opcodes: &instructions,
            results: &self.results,
            params: parameters.to_vec(),
            locals: &locals,
        })
    }

    pub fn from_frame<'a>(
        &'a self,
        frame: &'a Frame,
        modules: &'a HashMap<String, Module>,
    ) -> Result<Frame<'a>> {
        // 1. Get the parameters that needs to be passed to the function
        let mut params = vec![];
        let mut values = frame.values.iter().rev();
        for _ in 0..self.parameters.len() {
            params.push(
                values
                    .next()
                    .ok_or(WasmError::err("Expected more items to fit on stack"))?
                    .clone(),
            );
        }
        params.reverse();

        // 2. Check if the call stack matches the parameters
        if self.compare_params(&params) {
            return Err(WasmError::err(format!(
                "can't build frame with function because improper parameters were shared"
            )));
        }

        // 3. Get the instructions for the function
        let (locals, instructions) = self.function_impl(modules)?;

        // 4. Create the frame
        Ok(Frame {
            values: vec![],
            opcodes: &instructions,
            results: &self.results,
            params: self.parameters.to_vec(),
            locals: &locals,
        })
    }

    pub fn identifier(&self) -> Identifier {
        self.id.clone()
    }

    pub fn take_export(&mut self) -> Option<Export> {
        self.export.take()
    }
}
