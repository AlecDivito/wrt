use std::{collections::HashMap, convert::TryInto, str::FromStr};

use crate::{
    ast::{
        model::{ExportType, Frame, Function, Module},
        types::ValueType,
    },
    structure::types::Index,
};

use super::Number;

pub struct FunctionInstance {
    // instructions:
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(Number),
}

impl Value {
    pub fn as_number(&self) -> Option<&Number> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug)]
enum ExportPointer {
    Func(usize),
}

#[derive(Debug)]
pub struct Instance {
    funcs: Vec<Function>,
    exports: HashMap<String, ExportPointer>,
}

impl Instance {
    pub fn new(module: &Module) -> Instance {
        let mut exports = HashMap::new();
        for export in module.exports.iter() {
            // TODO: There really should be validation here loool
            match export.ty().unwrap() {
                ExportType::Func(Index::Id(id)) => {
                    if let Some(index) = module.functions.iter().position(|p| p.id() == Some(&id)) {
                        exports.insert(export.name().unwrap().clone(), ExportPointer::Func(index))
                    } else {
                        panic!("I don't plan to spend more time on this project anymore.")
                    }
                }
                ExportType::Func(Index::Index(index)) => {
                    exports.insert(format!("{}", index), ExportPointer::Func(*index as usize))
                }
            };
        }

        Self {
            funcs: module.functions.clone(),
            exports,
        }
    }

    pub fn call(&self, name: impl ToString, parameters: &[&str]) -> Vec<Value> {
        if let Some(ExportPointer::Func(id)) = self.exports.get(&name.to_string()) {
            if let Some(func) = self.funcs.get(*id) {
                let frame = Frame::from_strings(func, parameters);
                return func.call(frame);
            }
        }
        panic!("Can't do this :( {:?}", self);
    }

    // pub fn execute(&self, name: impl ToString, variables: impl AsRef<[Value]>) -> Vec<Value> {
    //     if let Some(ExportPointer::Func(id)) = self.exports.get(&name.to_string()) {
    //         if let Some(func) = self.funcs.get(*id) {
    //             return func.call(variables.as_ref());
    //         }
    //     }
    //     panic!("Can't do this :(")
    // }
}
