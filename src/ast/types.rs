use std::convert::TryFrom;

use crate::{
    execution::{instance::Value, Number},
    lex::Token,
};

use super::{
    error::Error,
    tree::{Accept, NodeVisitor},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumberType {
    I32,
    I64,
    F32,
    F64,
}

impl NumberType {
    pub fn add(&self, lhs: Number, rhs: Number) -> Number {
        match (self, lhs, rhs) {
            (NumberType::I32, Number::I32(lhs), Number::I32(rhs)) => Number::I32(lhs + rhs),
            (NumberType::I64, Number::I64(lhs), Number::I64(rhs)) => Number::I64(lhs + rhs),
            (NumberType::F32, Number::F32(lhs), Number::F32(rhs)) => Number::F32(lhs + rhs),
            (NumberType::F64, Number::F64(lhs), Number::F64(rhs)) => Number::F64(lhs + rhs),
            _ => panic!("Can't add when things don't match"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VectorType {
    V128,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbsoluteHeapType {
    Any,
    Eq,
    I31,
    Struct,
    Array,
    None,
    Func,
    NoFunc,
    Extern,
    NoExn,
    Exn,
    NoExtern,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstHeapType {
    Abs(AbsoluteHeapType),
    TypeIndex(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReferenceType {
    ty: AstHeapType,
    is_null: bool,
}

impl ReferenceType {
    pub fn shorthand(ty: AbsoluteHeapType) -> Self {
        Self {
            ty: AstHeapType::Abs(ty),
            is_null: true,
        }
    }

    pub fn new(ty: AbsoluteHeapType) -> Self {
        Self {
            ty: AstHeapType::Abs(ty),
            is_null: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueType {
    Number(NumberType),
    Vector(VectorType),
    Ref(ReferenceType),
}

impl<V: NodeVisitor> Accept<V> for ValueType {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_value_type(self)
    }
}

impl TryFrom<&Token> for ValueType {
    type Error = Error;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use crate::lex::Keyword::*;
        use crate::lex::TokenType::*;
        Ok(match value.ty() {
            // Keyword(I32) => ValueType::Number(NumberType::I32),
            // Keyword(I64) => ValueType::Number(NumberType::I64),
            // Keyword(F32) => ValueType::Number(NumberType::F32),
            // Keyword(F64) => ValueType::Number(NumberType::F64),
            // Keyword(V128) => ValueType::Vector(VectorType::V128),
            // Keyword(Any) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Any)),
            // Keyword(Eq) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Eq)),
            // Keyword(I31) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::I31)),
            // Keyword(Struct) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Struct)),
            // Keyword(Array) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Array)),
            // Keyword(None) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::None)),
            Keyword(Func) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Func)),
            // Keyword(NoFunc) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::NoFunc)),
            // Keyword(Extern) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Extern)),
            // Keyword(NoExn) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::NoExn)),
            // Keyword(Exn) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::Exn)),
            // Keyword(NoExtern) => ValueType::Ref(ReferenceType::new(AbsoluteHeapType::NoExtern)),
            _ => {
                return Err(Error::Unexpected {
                    expected: "ValueType",
                    found: value.clone(),
                })
            }
        })
    }
}
