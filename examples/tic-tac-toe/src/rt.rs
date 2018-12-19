use std::io;

use failure::Error;

use smpl::Module;
use smpl::parse_module;
use smpl::interpreter::*;
use smpl::Err;

const RT: &'static str = include_str!("rt.smpl");

pub fn include(scripts: &mut Vec<Module>) -> Result<(), Err> {
    scripts.push(parse_module(RT)?);

    Ok(())
}

pub fn map_builtins(vm: &mut dyn BuiltinMap) {
    vm.insert_builtin("rt", "user_key", user_input).unwrap();
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
