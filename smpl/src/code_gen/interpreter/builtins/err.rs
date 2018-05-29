use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

pub const MOD_ERR: &'static str = "err";

pub const ERR_PANIC: &'static str = "panic";

pub const ERR_DECLARATION: &'static str =
"
mod err;

builtin fn panic(msg: String);
";

pub fn include(modules: &mut Vec<Module>) {
    modules.push(parse_module(ERR_DECLARATION).unwrap());
}

pub fn add(vm: &mut VM) {
    vm.insert_builtin(MOD_ERR, ERR_PANIC, Box::new(Panic));
}

pub struct Panic;

impl BuiltInFn for Panic {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();
        let a = args.remove(0);

        match a {
            Value::String(s) => panic!("{}", s),
            _ => unreachable!(),
        }
    }
}
