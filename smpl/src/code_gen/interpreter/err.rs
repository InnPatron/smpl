use failure::Fail;

use crate::analysis::error::AnalysisError;
use crate::analysis::ModuleId;
use crate::err::Error as StaticError;

#[derive(Debug)]
pub enum VmError {
    StaticError(StaticError),
    BuiltinCollision(ModuleId, String),
    NotABuiltin(ModuleId, String),
    NotAFn(ModuleId, String),
}

impl From<StaticError> for VmError {
    fn from(e: StaticError) -> VmError {
        VmError::StaticError(e)
    }
}

impl From<AnalysisError> for VmError {
    fn from(e: AnalysisError) -> VmError {
        VmError::StaticError(e.into())
    }
}

#[derive(Fail, Debug)]
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
}

#[derive(Fail, Debug)]
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
        use crate::code_gen::interpreter::err::*;
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
        use crate::code_gen::interpreter::err::*;
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
        use crate::code_gen::interpreter::err::*;
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
        use crate::code_gen::interpreter::err::*;
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
        use crate::code_gen::interpreter::err::*;
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
