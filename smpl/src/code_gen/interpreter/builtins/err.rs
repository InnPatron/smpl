use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

pub const MOD_ERR: &'static str = "err";

pub const ERR_PANIC: &'static str = "panic";
pub const ERR_PANIC_MSG: &'static str = "panic_msg";
pub const ERR_ASSERT: &'static str = "assert";

pub const ERR_DECLARATION: &'static str = include_str!("err.smpl");

pub fn include(modules: &mut Vec<Module>) {
    modules.push(parse_module(ERR_DECLARATION).unwrap());
}

pub fn add(vm: &mut VM) {
    vm.insert_builtin(MOD_ERR, ERR_PANIC, Box::new(Panic));
    vm.insert_builtin(MOD_ERR, ERR_PANIC_MSG, Box::new(PanicMsg));
    vm.insert_builtin(MOD_ERR, ERR_ASSERT, Box::new(Assert));
}

pub struct Panic;

impl BuiltinFn for Panic {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        panic!();
    }
}

pub struct PanicMsg;

impl BuiltinFn for PanicMsg {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => panic!("{}", s),
            _ => unreachable!(),
        }
    }
}

pub struct Assert;

impl BuiltinFn for Assert {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        let a = irmatch!(a; Value::Bool(a) => a);
        assert!(a);

        Value::Unit
    }
}
