use std::collections::HashMap;

use block::{Block, SubString};
use types::{function::Frame, instruction::Instruction, module::Module, Identifier};

use crate::{
    error::{Result, WasmError},
    types::value::ValueType,
};

pub struct Instance {
    main: Module,
    modules: HashMap<String, Module>,
}

impl Instance {
    pub fn new(module: Module) -> Self {
        Self {
            main: module,
            modules: HashMap::new(),
        }
    }

    pub fn link(&mut self, module: Module, name: impl Into<String>) {
        //TODO(Alec): maybe do some error checking??? but then again, i'm not
        // sure if the module should be added right now or later but this is the
        // implementation i've choosen so far.
        self.modules.insert(name.into(), module);
    }

    pub fn execute(
        &self,
        name: impl Into<String>,
        parameters: &[ValueType],
    ) -> Result<Vec<ValueType>> {
        let name = name.into();
        let export = self.main.export(&name).ok_or(WasmError::err(format!(
            "function name ('{}') does not exist in module",
            name
        )))?;

        let func_id = export
            .export_type()
            .function_id()
            .ok_or(WasmError::err(format!(
                "failed to find exported function {} in program",
                name
            )))?;

        let function = self.main.function(func_id).ok_or(WasmError::err(format!(
            "failed to find exported function with identifier {:?}",
            func_id
        )))?;

        let frame = function.frame(parameters)?;

        self.run_frame(frame)
    }

    fn run_frame(&self, mut frame: Frame) -> Result<Vec<ValueType>> {
        let mut i = 0;
        while let Some(instruction) = frame.opcodes.get(i) {
            match instruction {
                &Instruction::Return => break,
                Instruction::Call(id) => {
                    let func_call = self
                        .main
                        .function(&Identifier::String(id.to_owned()))
                        .ok_or(WasmError::err(format!(
                            "failed to find exported function with identifier {:?}",
                            id
                        )))?;
                    let new_frame = func_call.from_frame(&frame)?;
                    let call_result = self.run_frame(new_frame)?;
                    for value in call_result {
                        frame.push(value)
                    }
                }

                Instruction::LocalGet(index) => frame.push_param(index)?,

                Instruction::I32Add => {
                    let v1 = frame.pop().ok_or(WasmError::err("Stack overflow"))?;
                    let v2 = frame.pop().ok_or(WasmError::err("Stack overflow"))?;
                    frame.push(v1.add(v2)?);
                }
                Instruction::I32Const(int) => frame.push(ValueType::I32(*int)),
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
}

mod block;
pub mod error;
pub mod types;
