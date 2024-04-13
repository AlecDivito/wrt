use crate::{
    block::BlockType,
    error::{Result, WasmError},
    values::func::FunctionType,
    Block,
};

use super::{export::{Export, Identifier}, function::Function, global::Global, import::Import};

#[derive(Debug)]
pub struct Module {
    types: Vec<FunctionType>,
    // funcs: Vec<Func>
    // tables: Vec<Table>
    // memories: Vec<Memory>
    globals: Vec<Global>,
    // elements: Vec<Element>
    // datas: Vec<Data>
    // start: Option<Start>
    exports: Vec<Export>,
    functions: Vec<Function>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            exports: Vec::new(),
        }
    }

    pub fn build(block: Block) -> Result<Module> {
        let mut module = Module::new();
        if let BlockType::Module = block.type_id() {
            for child in block.children() {
                match child.type_id() {
                    BlockType::Function => {
                        let mut func = Function::impl_block(child)?;
                        if let Some(export) = func.take_export() {
                            module.exports.insert(export.id(), export);
                        }
                        module.functions.push(func);
                    }
                    BlockType::Export => {
                        let export = Export::build(&module, child)?;
                        module.exports.insert(export.id(), export);
                    }
                    BlockType::Import => match Import::build(child)? {
                        // ImportDefinition::Function(f) => module.functions.push(f),
                    },
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
