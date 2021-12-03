use crate::Block;

/// Buildable takes a block and creates a copy of the self from the block
/// description. Buildable means that a component can be built from a block
trait Buildable {
    fn build(block: &Block) -> Self;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Identifier {
    String(String),
    Number(usize),
}

pub mod export;
pub mod function;
pub mod instruction;
pub mod module;
pub mod parameter;
pub mod value;
