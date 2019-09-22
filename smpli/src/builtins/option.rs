use failure::Error;
use smpl::{UnparsedModule, parse_module};

use crate::*;

pub const OPTION_SOME: &'static str = "some";
pub const OPTION_IS_SOME: &'static str = "is_some";

pub const OPTION_UNWRAP: &'static str = "unwrap";
pub const OPTION_EXPECT: &'static str = "expect";

pub const OPTION_NONE: &'static str = "none";
pub const OPTION_IS_NONE: &'static str = "is_none";

pub const OPTION_DATA_KEY: &'static str = "__DATA";
pub const OPTION_TAG_KEY: &'static str = "__TAG";

pub const OPTION_TAG_SOME: i64 = 1;
pub const OPTION_TAG_NONE: i64 = 0;

const OPTION_DECLARATION: &'static str = include_str!("option.smpl");

#[derive(Fail, Debug)]
pub enum OptionError {
    #[fail(display = "Value not an option")]
    NonOption,
    #[fail(display = "Optional was not 'some'")]
    UnwrapFailed,
    #[fail(display = "Optional was not 'some': {}", _0)]
    ExpectFailed(String)
}

pub const fn some_tag() -> Value {
    Value::Int(OPTION_TAG_SOME)
}

pub const fn none_tag() -> Value {
    Value::Int(OPTION_TAG_NONE)
}

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(OPTION_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(OPTION_SOME, builtin_make_some)
        .add_builtin(OPTION_IS_SOME, builtin_is_some)
        .add_builtin(OPTION_UNWRAP, builtin_unwrap)
        .add_builtin(OPTION_EXPECT, builtin_expect)
        .add_builtin(OPTION_NONE, builtin_make_none)
        .add_builtin(OPTION_IS_NONE, builtin_is_none)
    ;

    module
}

pub fn make_some(value: Value) -> Value {
    let mut some_struct = Struct::new();
    
    let tag = some_tag();

    some_struct.set_field(OPTION_DATA_KEY.to_string(), value);
    some_struct.set_field(OPTION_TAG_KEY.to_string(), tag);

    Value::Struct(some_struct)
}

pub fn make_none() -> Value {
    let mut none_struct = Struct::new();
    
    let tag = none_tag();
    none_struct.set_field(OPTION_TAG_KEY.to_string(), tag);

    Value::Struct(none_struct)
}

pub fn is_some(value: Value) -> bool {
    match value {
        Value::Struct(inner) => {

            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == some_tag())
                .unwrap_or(false)
        }

        _ => false,
    }
}

pub fn is_none(value: Value) -> bool {
    match value {
        Value::Struct(inner) => {
            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == none_tag())
                .unwrap_or(false)
        }

        _ => false,
    }
}

pub fn unwrap(value: Value) -> Result<Value, Error> {
    match value {
        Value::Struct(inner) => {

            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == some_tag())
                .ok_or(OptionError::NonOption.into())
                .and_then(|tag_is_some| {

                    if tag_is_some {
                        inner.get_field(OPTION_DATA_KEY)
                            .ok_or(OptionError::NonOption)
                    } else {
                        Err(OptionError::UnwrapFailed)
                    }
                })
            .map_err(|e| e.into())

        }

        v => Err(OptionError::NonOption)?,
    }
}

pub fn expect(value: Value, message: String) -> Result<Value, Error> {
    match value {
        Value::Struct(inner) => {

            inner.get_field(OPTION_TAG_KEY)
                .map(|tag_value| tag_value == some_tag())
                .ok_or(OptionError::NonOption.into())
                .and_then(|tag_is_some| {

                    if tag_is_some {
                        inner.get_field(OPTION_DATA_KEY)
                            .ok_or(OptionError::NonOption)
                    } else {
                        Err(OptionError::ExpectFailed(message))
                    }
                })
            .map_err(|e| e.into())

        }

        v => Err(OptionError::NonOption)?,
    }
}

fn builtin_make_some(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(make_some(data))
}

fn builtin_is_some(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(Value::Bool(is_some(data)))
}

fn builtin_unwrap(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    unwrap(data)
}

fn builtin_expect(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(2, args)?;

    let message = args.pop().unwrap();
    let message = irmatch!(message; Value::String(s) => s);

    let data = args.pop().unwrap();

    expect(data, message)
}

fn builtin_make_none(args: Option<Vec<Value>>) -> Result<Value, Error> {
    no_args!(args)?;

    Ok(make_none())
}

fn builtin_is_none(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(Value::Bool(is_none(data)))
}
