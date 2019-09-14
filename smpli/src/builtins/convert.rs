use failure::Error;
use smpl::{UnparsedModule, parse_module};

use crate::exact_args;
use crate::*;

pub const MOD_CONVERT: &'static str = "convert";

pub const CONVERT_INT_TO_FLOAT: &'static str = "int_to_float";
pub const CONVERT_FLOAT_TO_INT: &'static str = "float_to_int";

pub const CONVERT_IS_FLOAT: &'static str = "is_float";
pub const CONVERT_IS_INT: &'static str = "is_int";

pub const CONVERT_STRING_TO_FLOAT: &'static str = "string_to_float";
pub const CONVERT_STRING_TO_INT: &'static str = "string_to_int";

pub const CONVERT_DECLARATION: &'static str = include_str!("convert.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(CONVERT_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(CONVERT_INT_TO_FLOAT, int_to_float)
        .add_builtin(CONVERT_FLOAT_TO_INT, float_to_int)
        .add_builtin(CONVERT_IS_FLOAT, is_float)
        .add_builtin(CONVERT_IS_INT, is_int)
        .add_builtin(CONVERT_STRING_TO_FLOAT, string_to_float)
        .add_builtin(CONVERT_STRING_TO_INT, string_to_int);

    module
}

#[derive(Fail, Debug)]
#[fail(display = "Error converting '{}' to {}'", _0, _1)]
pub struct ConversionError(String, ConversionTarget);

#[derive(Fail, Debug)]
pub enum ConversionTarget {
    #[fail(display = "String")]
    String,
    #[fail(display = "Int")]
    Int,
    #[fail(display = "Float")]
    Float,
}

fn int_to_float(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let a = args.remove(0);
    match a {
        Value::Int(i) => Ok(Value::Float(i as f32)),
        _ => unreachable!(),
    }
}

fn float_to_int(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let a = args.remove(0);
    match a {
        Value::Float(f) => Ok(Value::Int(f as i32)),
        _ => unreachable!(),
    }
}

fn is_float(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let a = args.remove(0);
    match a {
        Value::String(s) => Ok(Value::Bool(s.parse::<f32>().is_ok())),
        _ => unreachable!(),
    }
}

fn is_int(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let a = args.remove(0);
    match a {
        Value::String(s) => Ok(Value::Bool(s.parse::<i32>().is_ok())),
        _ => unreachable!(),
    }
}

fn string_to_float(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let a = args.remove(0);
    match a {
        Value::String(s) => {
            Ok(Value::Float(s.parse::<f32>().map_err(|_| {
                ConversionError(s, ConversionTarget::Float)
            })?))
        }
        _ => unreachable!(),
    }
}

fn string_to_int(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let a = args.remove(0);
    match a {
        Value::String(s) => Ok(Value::Int(
            s.parse::<i32>()
                .map_err(|_| ConversionError(s, ConversionTarget::Int))?,
        )),
        _ => unreachable!(),
    }
}
