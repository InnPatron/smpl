use std::io;

use failure::Error;

use smpli::*;

const RT: &'static str = include_str!("rt.smpl");

pub fn vm_module() -> VmModule {
    VmModule::new(parse_module(UnparsedModule::anonymous(RT)).unwrap())
        .add_builtin("user_key", smpli::erase(user_input))
}

async fn user_input(_args: Vec<Value>) -> smpli::BuiltinResult {
    let mut input = String::new();
    let size = io::stdin().read_line(&mut input).unwrap();

    if size < 1 {
        return Ok(Value::String("_".to_string()));
    } else {
        return Ok(Value::String(input.get(0..1).unwrap().to_string()));
    }
}
