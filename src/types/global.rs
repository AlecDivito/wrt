use std::{convert::TryFrom, str::FromStr};

use crate::{
    block::{Block, BlockType},
    error::WasmError,
    values::mutibility::Mutibility,
};

use super::{export::Export, import::Import};

#[derive(Debug)]
pub struct Global {
    id: Option<String>,
    value: Mutibility,

    // other things that can be decleared
    import: Option<Import>,
    exports: Option<Vec<Export>>,
}

impl<'a> TryFrom<Block<'a>> for Global {
    type Error = WasmError;

    fn try_from(block: Block<'a>) -> Result<Self, Self::Error> {
        block.expect(BlockType::Global)?;
        let id = block.try_identity()?;
        // let value = block.global_type()?;

        let mut exports: Option<Vec<Export>> = None;
        let mut import = None;
        let mut value = None;

        for child in block.children() {
            match child.type_id() {
                BlockType::Import => {
                    if import.is_some() {
                        return Err(WasmError::err(
                            "global blocks can only contain one import statement",
                        ));
                    }
                    import.insert(Import::try_from(child)?);
                }
                BlockType::Export => {
                    let export = Export::try_from(child)?;
                    if let Some(e) = exports {
                        e.push(export);
                    } else {
                        exports.insert(vec![export]);
                    }
                }
                BlockType::Mut => {
                    if value.is_some() {
                        return Err(WasmError::err(
                            "global blocks can only contain one type declaration. Found more then 1",
                        ));
                    }
                    value.insert(Mutibility::try_from(child)?);
                }
                _ => {
                    return Err(WasmError::expected(
                        &[BlockType::Import, BlockType::Export, BlockType::Mut],
                        child.type_id(),
                    ))
                }
            }
        }

        // 1. if this is imported:
        //  1.1 if we know the type: exit
        //  1.2 if we dont know the type: parse content

        // 2. if this is not imported:
        //  2.1 if we know the type: parse content as value
        //  2.2 if we don't know the type: parse content for type and value

        let err = WasmError::err("type or value not provided for global");

        if import.is_some() {
            if value.is_none() {
                value.insert(Mutibility::from_str(block.content().ok_or(err)?)?);
            }
        } else {
            if value.is_none() {
                value.insert(Mutibility::from_str(block.content().ok_or(err)?)?);
            } else {
                value
                    .as_ref()
                    .unwrap()
                    .set_value(block.content().ok_or(err)?);
            }
        }

        Ok(Self {
            id,
            import,
            exports,
            value: value.expect("globals value was none, this should never happen! add more tests"),
        })
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryInto;

    use crate::{
        block::Block,
        error::Result,
        types::{export::Export, global::Global, import::Import},
        values::{mutibility::Mutibility, value::ValueType},
    };

    fn parse(string: &str) -> Result<Global> {
        let mut source = crate::block::SubString::new(string);
        let block = Block::parse(&mut source)?;
        let global: Global = block.try_into()?;
        Ok(global)
    }

    #[test]
    fn const_global() {
        let global = parse("(global $id i32 42)").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_none());
        assert!(global.import.is_none());
        assert_eq!(global.value, Mutibility::Const(ValueType::I32(42)));
    }

    #[test]
    fn mut_global() {
        let global = parse("(global $id (mut i32) 42)").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_none());
        assert!(global.import.is_none());
        assert_eq!(global.value, Mutibility::Mut(ValueType::I32(42)));
    }

    #[test]
    fn exported_const_global() {
        let global = parse("(global $id (export \"ex\") i32 42)").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_some());
        assert_eq!(global.exports.unwrap().len(), 1);
        assert_eq!(global.exports.unwrap()[0], Export::new("ex"));
        assert!(global.import.is_none());
        assert_eq!(global.value, Mutibility::Const(ValueType::I32(42)));
    }

    #[test]
    fn exported_mut_global() {
        let global = parse("(global $id (export \"ex\") (mut i32) 42)").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.import.is_none());
        assert!(global.exports.is_some());
        assert_eq!(global.exports.unwrap().len(), 1);
        assert_eq!(global.exports.unwrap()[0], Export::new("ex"));
        assert_eq!(global.value, Mutibility::Mut(ValueType::I32(42)));
    }

    #[test]
    fn exported_mut_global_multiple_times() {
        let global =
            parse("(global $id (export \"ex0\") (export \"ex1\") (export \"ex2\") i32 42)")
                .unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.import.is_none());
        assert!(global.exports.is_some());
        assert_eq!(global.exports.unwrap().len(), 3);
        for (i, export) in global.exports.unwrap().iter().enumerate() {
            assert_eq!(*export, Export::new(format!("ex{}", i)));
        }
        assert_eq!(global.value, Mutibility::Const(ValueType::I32(42)));
    }

    #[test]
    fn imported_const_global() {
        let global = parse("(global $id (import \"lib\" \"ex\") i32)").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_none());
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.value, Mutibility::Const(ValueType::I32(0)));
    }

    #[test]
    fn imported_mut_global() {
        let global = parse("(global $id (import \"lib\" \"ex\") (mut i32))").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_none());
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.value, Mutibility::Mut(ValueType::I32(0)));
    }

    #[test]
    fn imported_mut_global_fails_cause_defines_value() {
        assert!(parse("(global $id (import \"lib\" \"ex\") (mut i32) 0)").is_err());
    }

    #[test]
    fn imported_exported_const_global() {
        let global = parse("(global $id (export \"ex\") (import \"lib\" \"ex\") i32)").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_some());
        assert_eq!(global.exports.unwrap().len(), 1);
        assert_eq!(global.exports.unwrap()[0], Export::new("ex"));
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.value, Mutibility::Const(ValueType::I32(0)));
    }

    #[test]
    fn imported_exported_mut_global() {
        let global =
            parse("(global $id (export \"ex\") (import \"lib\" \"ex\") (mut i32))").unwrap();
        assert_eq!(global.id.unwrap(), "$id");
        assert!(global.exports.is_some());
        assert_eq!(global.exports.unwrap().len(), 1);
        assert_eq!(global.exports.unwrap()[0], Export::new("ex"));
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.value, Mutibility::Mut(ValueType::I32(0)));
    }
}
