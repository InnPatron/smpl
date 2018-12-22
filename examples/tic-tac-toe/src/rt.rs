use std::io;

use failure::Error;

use smpl::{ParsedModule, UnparsedModule};
use smpl::parse_module;
use smpl::interpreter::*;

const RT: &'static str = include_str!("rt.smpl");

pub fn vm_module() -> VmModule {
    VmModule::new(parse_module(UnparsedModule::anonymous(RT)).unwrap())
        .add_builtin("user_key", user_input)
}

fn user_input(_args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut input = String::new();
    let size = io::stdin().read_line(&mut input).unwrap();

    if size < 1 {
        return Ok(Value::String("_".to_string()));
    } else {
        return Ok(Value::String(input.get(0..1).unwrap().to_string()));
    }
}
