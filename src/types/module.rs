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
    exports: HashMap<Identifier, Export>,
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
                        let mut func = Function::build(child)?;
                        if let Some(export) = func.take_export() {
                            module.exports.insert(export.id(), export);
                        }
                        module.functions.push(func);
                    }
                    BlockType::Export => {
                        let export = Export::build(&module, child)?;
                        module.exports.insert(export.id(), export);
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

    pub fn export(&self, name: impl Into<String>) -> Option<&Export> {
        self.exports.get(&Identifier::String(name.into()))
    }
}
