use failure::Fail;

use smpl::ModuleId;
use smpl::error::Error as StaticError;
use smpl::byte_gen::{ Instruction, InstructionPointerType };

#[derive(Debug, Clone)]
pub enum VmError {
    StaticError(StaticError),
    BuiltinCollision(ModuleFnPair),
    NotABuiltin(ModuleFnPair),
    NotAFn(ModuleFnPair),
    NotAModule(String),
}

impl<T> From<T> for VmError where T: Into<StaticError> {
    fn from(e: T) -> VmError {
        VmError::StaticError(e.into())
    }
}

#[derive(Debug, Clone)]
pub struct ModuleFnPair {
    pub module: String,
    pub function: String,
}

#[derive(Fail, Debug, Clone)]
pub enum InternalError {
    #[fail(display = "Invalid number of arguments. Found {}. {}", _0, _1)]
    InvalidArgCount(usize, ExpectedArgCount),
    #[fail(
        display = "Unexpected argument type at {}. Found {}. Expected {}",
        index, found, expected
    )]
    InvalidArgType {
        index: usize,
        found: String,
        expected: String,
    },

    #[fail(display = "Invalid instruction pointer {}. Max: {}", ip, max)]
    InstructionPointerOutOfBounds {
        ip: InstructionPointerType,
        max: usize
    },

    #[fail(display = "Integer {} out of the range [{}, {}", v, min_inclusive, max_inclusive)]
    IntegerOutOfRange {
        v: i64,
        min_inclusive: i64,
        max_inclusive: i64,
    },

    #[fail(display = "InvalidInstruction: {}", _0)]
    InvalidInstruction(IIReason),

    #[fail(display = "Runtime instruction error: {}", _0)]
    RuntimeInstructionError(RuntimeInstructionError),
}

#[derive(Fail, Debug, Clone)]
pub enum RuntimeInstructionError {
    // TODO: canonical string representation of instructions
    #[fail(display = "Expected int in: {:?}", _0)]
    ExpectedInt(Instruction),

    #[fail(display = "Expected float in: {:?}", _0)]
    ExpectedFloat(Instruction),

    #[fail(display = "Expected bool in: {:?}", _0)]
    ExpectedBool(Instruction),

    #[fail(display = "Expected function in: {:?}", _0)]
    ExpectedFunction(Instruction),

    #[fail(display = "No return found for instruction at {}", _0)]
    NoReturnValue(InstructionPointerType),

    #[fail(display = "Attempting to add {} to current IP({}) results in underflow", addition, current)]
    IPUnderflow {
        current: InstructionPointerType,
        addition: i64,
    },

    #[fail(display = "Attempting to add {} to current IP({}) results in overflow", addition, current)]
    IPOverflow {
        current: InstructionPointerType,
        addition: i64,
    },
}

#[derive(Fail, Debug, Clone)]
pub enum IIReason {
    // TODO: canonical string representation of instructions
    #[fail(display = "Expected int in: {:?}", _0)]
    ExpectedInt(Instruction),

    #[fail(display = "Expected float in: {:?}", _0)]
    ExpectedFloat(Instruction),

    #[fail(display = "Expected bool in: {:?}", _0)]
    ExpectedBool(Instruction),
}

#[derive(Fail, Debug, Clone)]
pub enum ExpectedArgCount {
    #[fail(display = "Expected {}..={}", _0, _1)]
    Range(usize, usize),
    #[fail(display = "Expected >= {}", _0)]
    Min(usize),
    #[fail(display = "Expected <= {}", _0)]
    Max(usize),
    #[fail(display = "Expected == {}", _0)]
    Exact(usize),
}

// Checks for no arguments
#[macro_export]
macro_rules! no_args {
    ($args: expr) => {{
        use crate::err::*;
        if $args.len() != 0 {
            Err(InternalError::InvalidArgCount(
                $args.len(),
                ExpectedArgCount::Exact(0),
            ))
        } else {
            Ok(())
        }
    }};
}

#[macro_export]
macro_rules! exact_args {
    ($exact: expr, $args: expr) => {{
        use crate::err::*;
        if $args.len() != $exact {
            Err(InternalError::InvalidArgCount(
                $args.len(),
                ExpectedArgCount::Exact($exact),
            ))
        } else {
            Ok($args)
        }
    }};
}

// Checks for 1+ args
#[macro_export]
macro_rules! min_args {
    ($min: expr, $args: expr) => {{
        use crate::err::*;
        if $args.len() < $min {
            Err(InternalError::InvalidArgCount(
                $args.len(),
                ExpectedArgCount::Min($min),
            ))
        } else {
            Ok($args)
        }
    }};
}

// Checks for <= max args
#[macro_export]
macro_rules! max_args {
    ($max: expr, $args: expr) => {{
        use crate::err::*;
        if $args.len() > $max {
            Err(InternalError::InvalidArgCount(
                $args.len(),
                ExpectedArgCount::Max($max),
            ))
        } else {
            Ok($args)
        }
    }};
}

// Checks for [min, max] args
#[macro_export]
macro_rules! arg_range_inclusive {
    // Assume min_inclusive is greater than 1
    ($min_inclusive: expr, $max_inclusive: expr, $args: expr) => {{
        use crate::err::*;
        if $min_inclusive <= $args.len() && $max_inclusive >= $args.len() {
            Ok($args)
        } else {
            Err(InternalError::InvalidArgCount(
                $args.len(),
                ExpectedArgCount::Range($min_inclusive, $max_inclusive),
            ))
        }
    }};
}
