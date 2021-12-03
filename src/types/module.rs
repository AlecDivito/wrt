use std::collections::HashMap;

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    Block,
};

use super::{export::Export, function::Function, Identifier};

#[derive(Debug)]
pub struct Module {
    // types: Vec<FuncType>
    // funcs: Vec<Func>
    // tables: Vec<Table>
    // memories: Vec<Memory>
    // globals: Vec<Global>
    // elements: Vec<Element>
    // datas: Vec<Data>
    // start: Option<Start>
    // imports: Vec<Import>
    // exports: Vec<Export>
    functions: Vec<Function>,
    exports: HashMap<String, Export>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            exports: HashMap::new(),
        }
    }

    pub fn build(block: Block) -> Result<Module> {
        let mut module = Module::new();
        if let BlockType::Module = block.type_id() {
            for child in block.children() {
                match child.type_id() {
                    BlockType::Function => {
                        module.functions.push(Function::build(child)?);
                    }
                    BlockType::Export => {
                        let export = Export::build(&module, child)?;
                        module
                            .exports
                            .insert(child.variable_name().as_ref().unwrap().to_string(), export);
                    }
                    _ => {
                        return Err(WasmError::new(
                            0,
                            0,
                            format!("expected child block, found {}", child.type_id()),
                        ))
                    }
                };
            }
            Ok(module)
        } else {
            Err(WasmError::new(
                0,
                0,
                format!("expected module, found {}", block.type_id()),
            ))
        }
    }

    pub fn function(&self, id: &Identifier) -> Option<&Function> {
        for func in &self.functions {
            if func.identifier() == *id {
                return Some(func);
            }
        }
        None
    }

    pub fn export(&self, name: &str) -> Option<&Export> {
        self.exports.get(name)
    }
}
