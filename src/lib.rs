use block::{Block, SubString};
use types::{
    export::ExportType,
    instruction::{self, Instruction},
    module::Module,
};

use crate::{
    error::{Result, WasmError},
    types::value::ValueType,
};

pub struct Instance {
    module: Module,
}

impl Instance {
    pub fn new(module: Module) -> Self {
        Self { module }
    }

    pub fn execute(
        &self,
        name: impl Into<String>,
        arguments: &[ValueType],
    ) -> Result<Vec<ValueType>> {
        let name = name.into();
        let export = self.module.export(&name).ok_or(WasmError::err(format!(
            "function name ('{}') does not exist in module",
            name
        )))?;

        let function = if let ExportType::Function(func_id) = export.export_type() {
            //TODO(Alec): This .ok_or() probably won't ever be triggered. The
            // reason is because when we build the export class, we already do
            // this check during compilation.
            self.module.function(func_id).ok_or(WasmError::err(format!(
                "failed to find exported function with identifier {:?}",
                func_id
            )))?
        } else {
            return Err(WasmError::err(format!(
                "failed to find exported function {} in program",
                name
            )));
        };

        let mut frame = function.frame(arguments)?;

        let mut i = 0;
        while let Some(instruction) = frame.opcodes.get(i) {
            match instruction {
                Instruction::LocalGet(index) => {
                    frame.push_param(index)?;
                }
                Instruction::I32Add => {
                    let v1 = frame.pop().ok_or(WasmError::err("Stack overflow"))?;
                    let v2 = frame.pop().ok_or(WasmError::err("Stack overflow"))?;
                    frame.push(v1.add(v2)?);
                }
            }
            i = i + 1;
        }

        frame.set_result()
    }
}

pub struct Engine {}

impl Engine {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, text: &str) -> Result<Module> {
        let mut program_str = SubString::new(text);
        let program = Block::parse(&mut program_str)?;
        program.walk(None);
        Module::build(program)
    }

    pub fn compile_and_run(text: &str, func: &str, args: &[ValueType]) -> Result<Vec<ValueType>> {
        let engine = Engine::new();
        let module = engine.compile(text)?;
        let instance = engine.instantiate(module);
        instance.execute(func, args)
    }

    pub fn instantiate(&self, module: Module) -> Instance {
        Instance::new(module)
    }

    pub fn execute(&self, module: Module, func: impl Into<String>) -> Vec<ValueType> {
        vec![ValueType::I32(0)]
    }
}

mod block;
pub mod error;
pub mod types;
