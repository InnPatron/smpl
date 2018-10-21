use std::io;

use smpl::Module;
use smpl::parse_module;
use smpl::interpreter::*;

const RT: &'static str = include_str!("rt.smpl");

pub fn include(scripts: &mut Vec<Module>) {
    scripts.push(parse_module(RT).unwrap());
}

pub fn map_builtins(vm: &mut AVM) {
    vm.insert_builtin("rt", "user_key", Box::new(UserInput)).unwrap();
}

pub struct UserInput;

impl BuiltinFn for UserInput {
    fn execute(&self, _args: Option<Vec<Value>>) -> Value {
        let mut input = String::new();
        let size = io::stdin().read_line(&mut input).unwrap();

        if size < 1 {
            return Value::String("_".to_string());
        } else {
            return Value::String(input.get(0..1).unwrap().to_string());
        }
    }
}
