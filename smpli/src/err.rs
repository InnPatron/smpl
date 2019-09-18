use failure::Fail;

use smpl::ModuleId;
use smpl::Error as StaticError;
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
}

#[derive(Fail, Debug, Clone)]
pub enum IIReason {
    // TODO: canonical string representation of instructions
    #[fail(display = "Expected int in: {:?}", _0)]
    ExpectedInt(Instruction), 

    #[fail(display = "Expected float in: {:?}", _0)]
    ExpectedFloat(Instruction), 
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
        match $args {
            Some(args) => Err(InternalError::InvalidArgCount(
                args.len(),
                ExpectedArgCount::Exact(0),
            )),

            None => Ok(None),
        }
    }};
}

#[macro_export]
macro_rules! exact_args {
    ($exact: expr, $args: expr) => {{
        use crate::err::*;
        match $args {
            Some(args) => {
                if args.len() != $exact {
                    Err(InternalError::InvalidArgCount(
                        args.len(),
                        ExpectedArgCount::Exact($exact),
                    ))
                } else {
                    Ok(args)
                }
            }

            None => {
                // Assume $exact != 0 to make types nicer
                // Use no_args!() instead
                assert!($exact != 0);
                Err(InternalError::InvalidArgCount(
                    0,
                    ExpectedArgCount::Exact($exact),
                ))
            }
        }
    }};
}

// Checks for 1+ args
#[macro_export]
macro_rules! min_args {
    ($min: expr, $args: expr) => {{
        use crate::err::*;
        match $args {
            Some(args) => {
                if args.len() < $min {
                    Err(InternalError::InvalidArgCount(
                        args.len(),
                        ExpectedArgCount::Min($min),
                    ))
                } else {
                    Ok(args)
                }
            }

            None => {
                // min = 0 is the same as not checking the arguments
                assert!($min != 0);
                Err(InternalError::InvalidArgCount(
                    0,
                    ExpectedArgCount::Min($min),
                ))
            }
        }
    }};
}

// Checks for <= max args
#[macro_export]
macro_rules! max_args {
    ($max: expr, $args: expr) => {{
        use crate::err::*;
        match $args {
            Some(args) => {
                if args.len() > $max {
                    Err(InternalError::InvalidArgCount(
                        args.len(),
                        ExpectedArgCount::Max($max),
                    ))
                } else {
                    Ok(Some(args))
                }
            }

            None => Ok(None),
        }
    }};
}

// Checks for [min, max] args
#[macro_export]
macro_rules! arg_range_inclusive {
    // Assume min_inclusive is greater than 1
    ($min_inclusive: expr, $max_inclusive: expr, $args: expr) => {{
        use crate::err::*;
        match $args {
            Some(args) => {
                if $min_inclusive <= args.len() && $max_inclusive >= args.len() {
                    Ok(args)
                } else {
                    Err(InternalError::InvalidArgCount(
                        args.len(),
                        ExpectedArgCount::Range($min_inclusive, $max_inclusive),
                    ))
                }
            }

            None => {
                // Assume min_inclusive is greater than 0
                // If min_inclusive == 0, use max_args instead
                if $min_inclusive == 0 {
                    panic!("Minimum inclusive is 0. Use max_args!() instead");
                }
                Err(InternalError::InvalidArgCount(
                    0,
                    ExpectedArgCount::Range($min_inclusive, $max_inclusive),
                ))
            }
        }
    }};
}
