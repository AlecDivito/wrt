use std::{convert::TryFrom, str::FromStr};

use crate::{
    block::{Block, BlockType},
    error::WasmError,
};

use super::{
    export::Export,
    import::Import,
    value::{Mutibility, ValueType},
    Identifier,
};

#[derive(Debug)]
pub struct Global {
    id: Identifier,
    global: Mutibility,
    import: Option<Import>,
    export: Option<Export>,
}

impl<'a> TryFrom<Block<'a>> for Global {
    type Error = WasmError;

    fn try_from(block: Block<'a>) -> Result<Self, Self::Error> {
        if BlockType::Global != *block.type_id() {
            return Err(WasmError::err(format!(
                "expected block type to be global, found {} instead",
                block.type_id()
            )));
        }

        let id = Identifier::String(block.variable_name().get(0).unwrap_or(&"").to_string());
        let mut value = None;
        let mut import = None;
        let mut export = None;

        for child in block.children() {
            match child.type_id() {
                BlockType::Import => {
                    if import.is_some() {
                        return Err(WasmError::err("for now, global can only have one import"));
                    }
                    import = Some(Import::try_from(child)?)
                }
                BlockType::Export => {
                    if export.is_some() {
                        return Err(WasmError::err("for now, global can only have one export"));
                    }
                    export = Some(Export::make_global_export(child.identity()?, &id));
                }
                BlockType::Mut => {
                    value = Some(
                        child
                            .content()
                            .ok_or(WasmError::err("mut block is required to have a mut block"))?,
                    )
                }
                _ => {
                    return Err(WasmError::err(format!(
                        "block type {} is not a valid child for global",
                        child.type_id()
                    )))
                }
            }
        }

        let global = match value {
            Some(v) => {
                if import.is_none() {
                    let content = block
                        .content()
                        .ok_or(WasmError::err("content is required for global block"))?;
                    Mutibility::Mut(ValueType::from_str(v)?.set_value(content)?)
                } else {
                    Mutibility::Mut(ValueType::from_str(v)?)
                }
            }
            None => {
                let content = block
                    .content()
                    .ok_or(WasmError::err("content is required for global block"))?;

                let mut splits = content.trim().split(" ");
                let value_type = splits.next().ok_or(WasmError::err("expected type"))?;
                if import.is_none() {
                    let value = splits
                        .next()
                        .ok_or(WasmError::err("need the actual value"))?;
                    Mutibility::Const(ValueType::from_str(value_type)?.set_value(value)?)
                } else {
                    Mutibility::Const(ValueType::from_str(value_type)?)
                }
            }
        };

        Ok(Self {
            id,
            global,
            import,
            export,
        })
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryInto;

    use crate::{
        block::Block,
        types::{
            export::Export,
            global::Global,
            import::Import,
            value::{Mutibility, ValueType},
            Identifier,
        },
    };

    fn parse(string: &str) -> Global {
        let mut source = crate::block::SubString::new(string);
        let block = Block::parse(&mut source).unwrap();
        let global: Global = block.try_into().unwrap();
        global
    }

    #[test]
    fn const_global() {
        let global = parse("(global $id i32 42)");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert!(global.export.is_none());
        assert!(global.import.is_none());
        assert_eq!(global.global, Mutibility::Const(ValueType::I32(42)));
    }

    #[test]
    fn mut_global() {
        let global = parse("(global $id (mut i32) 42)");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert!(global.export.is_none());
        assert!(global.import.is_none());
        assert_eq!(global.global, Mutibility::Mut(ValueType::I32(42)));
    }

    #[test]
    fn exported_const_global() {
        let global = parse("(global $id (export \"ex\") i32 42)");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert!(global.import.is_none());
        assert_eq!(
            global.export,
            Some(Export::make_global_export(
                Identifier::String("ex".into()),
                &global.id
            ))
        );
        assert_eq!(global.global, Mutibility::Const(ValueType::I32(42)));
    }

    #[test]
    fn exported_mut_global() {
        let global = parse("(global $id (export \"ex\") (mut i32) 42)");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert!(global.import.is_none());
        assert_eq!(
            global.export,
            Some(Export::make_global_export(
                Identifier::String("ex".into()),
                &global.id
            ))
        );
        assert_eq!(global.global, Mutibility::Mut(ValueType::I32(42)));
    }

    #[test]
    fn imported_const_global() {
        let global = parse("(global $id (import \"lib\" \"ex\") i32)");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert!(global.export.is_none());
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.global, Mutibility::Const(ValueType::I32(0)));
    }

    #[test]
    fn imported_mut_global() {
        let global = parse("(global $id (import \"lib\" \"ex\") (mut i32))");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert!(global.export.is_none());
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.global, Mutibility::Mut(ValueType::I32(0)));
    }

    #[test]
    fn imported_exported_const_global() {
        let global = parse("(global $id (export \"ex\") (import \"lib\" \"ex\") i32)");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert_eq!(
            global.export,
            Some(Export::make_global_export(
                Identifier::String("ex".into()),
                &global.id
            ))
        );
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.global, Mutibility::Const(ValueType::I32(0)));
    }

    #[test]
    fn imported_exported_mut_global() {
        let global = parse("(global $id (export \"ex\") (import \"lib\" \"ex\") (mut i32))");
        assert_eq!(global.id, Identifier::String("$id".to_string()));
        assert_eq!(
            global.export,
            Some(Export::make_global_export(
                Identifier::String("ex".into()),
                &global.id
            ))
        );
        assert_eq!(global.import, Some(Import::new("lib", "ex")));
        assert_eq!(global.global, Mutibility::Mut(ValueType::I32(0)));
    }
}
