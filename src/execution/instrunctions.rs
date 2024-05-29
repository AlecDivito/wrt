use crate::{
    structure::types::{NumType, SignType},
    validation::instruction::Execute,
};

use super::{Number, Stack, Trap};

pub enum Bool {
    True,
    False,
}

pub enum IntType {
    I32,
    I64,
}

// Integers are represented as two base unsigned numbers
// i = 2^N-1
// N = 32, 64

pub struct Add;
impl Execute for Add {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
        use Number::*;
        let op1 = stack.pop_and_assert_int()?;
        let op2 = stack.pop_and_assert_int()?;
        let result = match (op1, op2) {
            (I32(i1), I32(i2)) => I32((i1 + i2) % 2_i32.pow(32)),
            (I64(i1), I64(i2)) => I64((i1 + i2) % 2_i64.pow(64)),
            _ => return Err(Trap::new()),
        };
        stack.push(result);
        Ok(())
    }
}

pub struct Subtract;
impl Execute for Subtract {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
        use Number::*;
        let op1 = stack.pop_and_assert_int()?;
        let op2 = stack.pop_and_assert_int()?;
        let result = match (op1, op2) {
            (I32(i1), I32(i2)) => I32((i1 - i2 + 2_i32.pow(32)) % 2_i32.pow(32)),
            (I64(i1), I64(i2)) => I64((i1 - i2 + 2_i64.pow(64)) % 2_i64.pow(64)),
            _ => return Err(Trap::new()),
        };
        stack.push(result);
        Ok(())
    }
}

pub struct Multiply;
impl Execute for Multiply {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
        use Number::*;
        let op1 = stack.pop_and_assert_int()?;
        let op2 = stack.pop_and_assert_int()?;
        let result = match (op1, op2) {
            (I32(i1), I32(i2)) => I32((i1 * i2) % 2_i32.pow(32)),
            (I64(i1), I64(i2)) => I64((i1 * i2) % 2_i64.pow(64)),
            _ => return Err(Trap::new()),
        };
        stack.push(result);
        Ok(())
    }
}

pub struct Divide {
    ty: IntType,
    sign: SignType,
}
impl Execute for Divide {
    fn exec(&self, stack: &mut Stack) -> Result<(), Trap> {
        use Number::*;
        let op1 = stack.pop_and_assert_int()?;
        let op2 = stack.pop_and_assert_int()?;
        let result = match (op1, op2) {
            (I32(i1), I32(i2)) => I32((i1 * i2) % 2_i32.pow(32)),
            (I64(i1), I64(i2)) => I64((i1 * i2) % 2_i64.pow(64)),
            _ => return Err(Trap::new()),
        };
        stack.push(result);
        Ok(())
    }
}
