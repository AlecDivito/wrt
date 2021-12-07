use std::{convert::TryFrom, str::FromStr};

use crate::{
    block::BlockType,
    error::{Result, WasmError},
    values::{
        func::{FuncParam, FunctionType},
        value::ValueType,
    },
    Block,
};

use super::{export::Export, import::Import, instruction::Instruction};

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

// #[derive(Debug)]
// pub struct FuncImpl {
//     locals: Vec<ValueType>,
//     instructions: Vec<Instruction>,
// }

// impl FuncImpl {
//     pub fn new(locals: Vec<ValueType>, content: String) -> Result<Self> {
//         let instructions = content
//             .lines()
//             .map(Instruction::from_str)
//             .collect::<Result<Vec<Instruction>>>()?;
//         Ok(Self {
//             locals,
//             instructions,
//         })
//     }
// }

// #[derive(Debug)]
// pub enum FunctionPointer {
//     Impl(FuncImpl),
//     Import(Import),
// }

// #[derive(Debug)]
// pub struct Function {
//     id: Option<String>,
//     parameters: Vec<ValueType>,
//     results: Vec<ValueType>,
//     function: FunctionPointer,
//     export: Option<Export>,
// }

// impl Function {
//     fn block_check(block: &Block) -> Result<()> {
//         if !block.type_id().is_function() {
//             return Err(WasmError::err(format!(
//                 "expected function, found {}",
//                 block.type_id()
//             )));
//         }
//         Ok(())
//     }

//     fn block_id(block: &Block) -> Identifier {
//         //TODO(Alec): If there is no function name, then we still need to identify it in some way...
//         // as a solution, we could pass the identifier into the function...
//         Identifier::String(block.variable_name().get(0).unwrap_or(&"").to_string())
//     }

//     fn block_params(block: &Block) -> Result<Vec<Parameter>> {
//         block
//             .children()
//             .iter()
//             .map(Parameter::build)
//             .collect::<Result<Vec<Parameter>>>()
//     }

//     pub fn import_block(block: &Block, import: Import) -> Result<Self> {
//         Function::block_check(block)?;
//         let id = Function::block_id(block);
//         let params = Function::block_params(block)?;
//         // from my understanding, for the import statement, we can only accept
//         // parameter and result statements which means no re-export
//         let mut parameters = vec![];
//         let mut results = vec![];
//         for mut param in params {
//             match param.type_id() {
//                 BlockType::Parameter => parameters.append(param.value()),
//                 BlockType::Result => results.append(param.value()),
//                 _ => {
//                     return Err(WasmError::err(format!(
//                         "import func doesn't support parameter {:?}",
//                         param
//                     )))
//                 }
//             };
//         }
//         Ok(Self {
//             id,
//             parameters,
//             results,
//             function: FunctionPointer::Import(import),
//             export: None, // TODO(Alec): Can we re-export an import???
//         })
//     }

//     pub fn impl_block(block: &Block) -> Result<Function> {
//         Function::block_check(block)?;
//         let id = Function::block_id(block);
//         let params = Function::block_params(block)?;

//         let mut parameters = vec![];
//         let mut results = vec![];
//         let mut locals = vec![];
//         let mut export = None;

//         //TODO(Alec): I believe that a function can have no content. will need to double check that
//         let mut content = block.content().unwrap_or("").to_string();

//         for mut param in params {
//             match param.type_id() {
//                 BlockType::Parameter => {
//                     if let Some(old) = param.id() {
//                         content = content.replace(&old, &parameters.len().to_string());
//                     }
//                     parameters.append(param.value());
//                 }
//                 BlockType::Result => {
//                     if let Some(old) = param.id() {
//                         content = content.replace(&old, &results.len().to_string());
//                     }
//                     results.append(param.value());
//                 }
//                 BlockType::Local => {
//                     if let Some(old) = param.id() {
//                         content = content.replace(&old, &locals.len().to_string());
//                     }
//                     locals.append(param.value());
//                 }
//                 BlockType::Export => {
//                     if export.is_some() {
//                         return Err(WasmError::err("can not export function more then once"));
//                     }
//                     export = Some(Export::make_function_export(param, id.clone()))
//                 }
//                 _ => {
//                     panic!("This should neven execute as we already check the type when we build the parameter");
//                 }
//             };
//         }

//         Ok(Self {
//             id,
//             parameters,
//             results,
//             function: FunctionPointer::Impl(FuncImpl::new(locals, content)?),
//             export,
//         })
//     }

//     fn compare_params(&self, parameters: &[ValueType]) -> bool {
//         !ValueType::same_stack_types(&self.parameters, parameters)
//     }

//     fn function_impl<'a>(
//         &'a self,
//         modules: &'a HashMap<String, Module>,
//     ) -> Result<(&'a [ValueType], &'a [Instruction])> {
//         match &self.function {
//             FunctionPointer::Impl(i) => Ok((&i.locals, &i.instructions)),
//             FunctionPointer::Import(i) => {
//                 let module = modules
//                     .get(i.module())
//                     .ok_or(WasmError::err("module that was required wasn't imported"))?;
//                 let export = module.export(i.name()).ok_or(WasmError::err(
//                     "module was imported but required function wasn't",
//                 ))?;
//                 let func_id = export
//                     .export_type()
//                     .function_id()
//                     .ok_or(WasmError::err(format!(
//                         "failed to find exported function {} in program",
//                         i.name()
//                     )))?;
//                 let function = module.function(func_id).ok_or(WasmError::err(format!(
//                     "failed to find exported function with identifier {:?}",
//                     func_id
//                 )))?;
//                 function.function_impl(modules)
//             }
//         }
//     }

//     /// Assuming that this function can not be an imported function, we create
//     /// a frame using our current knowledge
//     pub fn frame<'a>(&'a self, parameters: &'a [ValueType]) -> Result<Frame<'a>> {
//         // 1. Check if the function has the same parameters
//         if self.compare_params(parameters) {
//             return Err(WasmError::err(format!(
//                 "can't build frame with function because improper parameters were shared"
//             )));
//         }
//         // 2. Get the instructions for the function
//         let (locals, instructions) = match &self.function {
//             FunctionPointer::Impl(i) => Ok((&i.locals, &i.instructions)),
//             FunctionPointer::Import(_) => {Err(WasmError::err("We can't import a function when we are creating a frame. This shouldn't ever happen and if it does, we may need to change the implmentation again..."))},
//         }?;

//         // 3. Create the frame
//         Ok(Frame {
//             values: vec![],
//             opcodes: &instructions,
//             results: &self.results,
//             params: parameters.to_vec(),
//             locals: &locals,
//         })
//     }

//     pub fn from_frame<'a>(
//         &'a self,
//         frame: &'a Frame,
//         modules: &'a HashMap<String, Module>,
//     ) -> Result<Frame<'a>> {
//         // 1. Get the parameters that needs to be passed to the function
//         let mut params = vec![];
//         let mut values = frame.values.iter().rev();
//         for _ in 0..self.parameters.len() {
//             params.push(
//                 values
//                     .next()
//                     .ok_or(WasmError::err("Expected more items to fit on stack"))?
//                     .clone(),
//             );
//         }
//         params.reverse();

//         // 2. Check if the call stack matches the parameters
//         if self.compare_params(&params) {
//             return Err(WasmError::err(format!(
//                 "can't build frame with function because improper parameters were shared"
//             )));
//         }

//         // 3. Get the instructions for the function
//         let (locals, instructions) = self.function_impl(modules)?;

//         // 4. Create the frame
//         Ok(Frame {
//             values: vec![],
//             opcodes: &instructions,
//             results: &self.results,
//             params: self.parameters.to_vec(),
//             locals: &locals,
//         })
//     }

//     pub fn identifier(&self) -> Identifier {
//         self.id.clone()
//     }

//     pub fn take_export(&mut self) -> Option<Export> {
//         self.export.take()
//     }
// }

#[derive(Debug)]
pub struct Function {
    id: Option<String>,
    // TODO(Alec): Locals and function parameters can not have the same ids
    locals: Vec<FuncParam>,
    type_id: FunctionType, // can be anonomy or typed

    // other things
    import: Option<Import>,
    exports: Option<Vec<Export>>,
    instructions: Vec<Instruction>,
}

impl<'a> TryFrom<&Block<'a>> for Function {
    type Error = WasmError;

    fn try_from(block: &Block<'a>) -> std::result::Result<Self, Self::Error> {
        block.expect(BlockType::Function)?;
        let id = block.try_identity()?;
        let type_id = FunctionType::try_from_block_allowing_other_children(block)?;

        let mut exports: Option<Vec<Export>> = None;
        let mut import = None;
        let mut locals = Vec::new();

        for child in block.children() {
            use BlockType::*;
            match child.type_id() {
                Import => {
                    if import.is_some() {
                        return Err(WasmError::err(
                            "global blocks can only contain one import statement",
                        ));
                    }
                    import.insert(super::import::Import::try_from(child)?);
                }
                Export => {
                    let export = super::export::Export::try_from(child)?;
                    if let Some(e) = exports {
                        e.push(export);
                    } else {
                        exports.insert(vec![export]);
                    }
                }
                Local => locals.push(FuncParam::try_from(child)?),
                Result | Parameter => continue,
                _ => {
                    return Err(WasmError::expected(
                        &[Import, Export, Parameter, Result, Local],
                        child.type_id(),
                    ))
                }
            }
        }

        //TODO(Alec): I believe that a function can have no content. will need to double check that
        let mut content = block.content().unwrap_or("").to_string();
        let instructions = content
            .lines()
            .map(Instruction::from_str)
            .collect::<Result<Vec<Instruction>>>()?;

        if import.is_some() {
            if !instructions.is_empty() || !locals.is_empty() {
                return Err(WasmError::err(
                    "function can not have instructions or locals when it is imported",
                ));
            }
        }

        let fun = |v: &[FuncParam]| {
            v.iter()
                .map(|l| l.id())
                .filter(|p| p.is_some())
                .map(|p| p.unwrap())
                .collect::<Vec<&String>>()
        };

        let local_ids = fun(&locals);
        let param_ids = fun(type_id.parameters());

        for id in local_ids {
            if param_ids.contains(&id) {
                return Err(WasmError::err(
                    "function can not have parameters and local share the same ids",
                ));
            }
        }

        Ok(Self {
            id,
            locals,
            type_id,
            import,
            exports,
            instructions,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse(string: &str) -> Result<Function> {
        let mut source = crate::block::SubString::new(string);
        let block = Block::parse(&mut source)?;
        let func = Function::try_from(&block)?;
        Ok(func)
    }

    #[test]
    fn func() {
        let func = parse("(func)").unwrap();
        assert!(func.id.is_none());
        assert!(func.locals.is_empty());
        assert_eq!(func.type_id, FunctionType::empty());
        assert!(func.instructions.is_empty());
        assert!(func.import.is_none());
        assert!(func.exports.is_none());
    }

    #[test]
    fn func_with_local() {
        let func = parse("(func (local i32))").unwrap();
        assert!(func.id.is_none());
        assert_eq!(func.locals[0].value_type()[0], ValueType::I32(0));
        assert!(func.locals[0].id().is_none());
        assert_eq!(func.type_id, FunctionType::empty());
        assert!(func.instructions.is_empty());
        assert!(func.import.is_none());
        assert!(func.exports.is_none());
    }

    #[test]
    fn func_with_local_and_instructions() {
        let func = parse("(func (local i32) i32.const 42)").unwrap();
        assert!(func.id.is_none());
        assert_eq!(func.locals[0].value_type()[0], ValueType::I32(0));
        assert!(func.locals[0].id().is_none());
        assert_eq!(func.type_id, FunctionType::empty());
        assert_eq!(func.instructions.len(), 1);
        assert!(func.import.is_none());
        assert!(func.exports.is_none());
    }

    #[test]
    fn func_with_local_identifer_and_instructions() {
        let func = parse("(func $id (local i32) i32.const 42)").unwrap();
        assert_eq!(func.id.unwrap(), "$id");
        assert_eq!(func.locals[0].value_type()[0], ValueType::I32(0));
        assert!(func.locals[0].id().is_none());
        assert_eq!(func.type_id, FunctionType::empty());
        assert_eq!(func.instructions.len(), 1);
        assert!(func.import.is_none());
        assert!(func.exports.is_none());
    }

    #[test]
    fn func_with_local_export_identifer_and_instructions() {
        let func = parse("(func $id (export \"test\") (local i32) i32.const 42)").unwrap();
        assert_eq!(func.id.unwrap(), "$id");
        assert_eq!(func.locals[0].value_type()[0], ValueType::I32(0));
        assert!(func.locals[0].id().is_none());
        assert_eq!(func.type_id, FunctionType::empty());
        assert_eq!(func.instructions.len(), 1);
        assert!(func.import.is_none());
        assert_eq!(func.exports.unwrap()[0], Export::new("test"));
    }

    #[test]
    fn func_with_import_and_locals_fails() {
        assert!(parse("(func $id (import \"a\" \"b\") (local i32))").is_err());
    }

    #[test]
    fn func_with_import_and_instructions_fails() {
        assert!(parse("(func $id (import \"a\" \"b\") i32.const 42)").is_err());
    }

    #[test]
    fn func_with_local_and_params_share_ids_fails() {
        assert!(parse("(func $id (local $i i32) (param $i i32) i32.const 42)").is_err());
    }

    #[test]
    fn func_with_id_export_and_import() {
        let func = parse("(func $id (export \"test\") (import \"a\" \"b\") )").unwrap();
        assert_eq!(func.id.unwrap(), "$id");
        assert_eq!(func.type_id, FunctionType::empty());
        assert!(func.instructions.is_empty());
        assert!(func.locals.is_empty());
        assert_eq!(func.import.unwrap(), Import::new("a", "b"));
        assert_eq!(func.exports.unwrap()[0], Export::new("test"));
    }

    #[test]
    fn func_with_local_exports_identifer_and_instructions() {
        let func =
            parse("(func $id (export \"test\") (export \"test2\") (local i32) i32.const 42)")
                .unwrap();
        assert_eq!(func.id.unwrap(), "$id");
        assert_eq!(func.locals[0].value_type()[0], ValueType::I32(0));
        assert!(func.locals[0].id().is_none());
        assert_eq!(func.type_id, FunctionType::empty());
        assert_eq!(func.instructions.len(), 1);
        assert!(func.import.is_none());
        assert_eq!(func.exports.unwrap()[0], Export::new("test"));
        assert_eq!(func.exports.unwrap()[1], Export::new("test2"));
    }
}
