use failure::Fail;

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

pub fn add<MAP: BuiltinMap>(vm: &mut MAP) {
    vm.insert_builtin(MOD_ERR, ERR_PANIC, Box::new(Panic))
        .unwrap();
    vm.insert_builtin(MOD_ERR, ERR_PANIC_MSG, Box::new(PanicMsg))
        .unwrap();
    vm.insert_builtin(MOD_ERR, ERR_ASSERT, Box::new(Assert))
        .unwrap();
}

pub struct Panic;

impl BuiltinFn for Panic {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        panic!();
    }
}

pub struct PanicMsg;

impl BuiltinFn for PanicMsg {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
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
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let mut args = args.unwrap();
        let a = args.remove(0);

        let a = irmatch!(a; Value::Bool(a) => a);
        assert!(a);

        Ok(Value::Unit)
    }
}
