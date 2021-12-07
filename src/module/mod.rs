use crate::Block;

/// Buildable takes a block and creates a copy of the self from the block
/// description. Buildable means that a component can be built from a block
trait Buildable {
    fn build(block: &Block) -> Self;
}

// pub mod data;
// pub mod export;
// pub mod function;
// pub mod global;
// pub mod import;
// pub mod instruction;
// pub mod mem;
pub mod module;
// pub mod table;
