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
pub const OPTION_TAG_KEY: &'static str = "__LEN";

pub const OPTION_TAG_SOME: i64 = 1;
pub const OPTION_TAG_NONE: i64 = 0;

const OPTION_DECLARATION: &'static str = include_str!("option.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(OPTION_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(OPTION_SOME, builtin_make_some);

    module
}

pub fn make_some(value: Value) -> Value {
    let mut some_struct = Struct::new();
    
    let tag = Value::Int(OPTION_TAG_SOME);

    some_struct.set_field(OPTION_DATA_KEY.to_string(), value);
    some_struct.set_field(OPTION_TAG_KEY.to_string(), tag);

    Value::Struct(some_struct)
}

fn builtin_make_some(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = exact_args!(1, args)?;

    let data = args.pop().unwrap();

    Ok(make_some(data))
}

