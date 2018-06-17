use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

const MOD_STRING: &'static str = "str";

const STRING_LEN: &'static str = "len";
const STRING_TO_STRING: &'static str = "to_string";
const STRING_APPEND: &'static str = "append";
const STRING_TO_LOWER: &'static str = "to_lower";
const STRING_TO_UPPER: &'static str = "to_upper";

const STRING_DECLARATION: &'static str = include_str!("str.smpl");

pub fn include(modules: &mut Vec<Module>) {
    modules.push(parse_module(STRING_DECLARATION).unwrap());
}

pub fn add(vm: &mut VM) {
    vm.insert_builtin(MOD_STRING, STRING_LEN, Box::new(Len));
    vm.insert_builtin(MOD_STRING, STRING_TO_STRING, Box::new(ToString));
    vm.insert_builtin(MOD_STRING, STRING_APPEND, Box::new(Append));
    vm.insert_builtin(MOD_STRING, STRING_TO_LOWER, Box::new(ToLower));
    vm.insert_builtin(MOD_STRING, STRING_TO_UPPER, Box::new(ToUpper));
}

struct Len;

impl BuiltInFn for Len {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let string = args.pop().unwrap();
        let string = irmatch!(string; Value::String(s) => s);

        Value::Int(string.len() as i32)
    }
}

struct ToString;

impl BuiltInFn for ToString {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.expect("str::to_string() expected 1+ args");

        let mut s = String::new();

        for a in args {
            s.push_str(&a.to_string());
        }

        Value::String(s)
    }
}

struct Append;

impl BuiltInFn for Append {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let to_append = args.pop().unwrap();
        let to_append = irmatch!(to_append; Value::String(s) => s);

        let base = args.pop().unwrap();
        let mut base = irmatch!(base; Value::String(s) => s);
        
        base.push_str(&to_append);

        Value::String(base)
    }
}

struct ToLower;

impl BuiltInFn for ToLower {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let string = args.pop().unwrap();
        let string = irmatch!(string; Value::String(s) => s);

        Value::String(string.to_lowercase())
    }
}

struct ToUpper;

impl BuiltInFn for ToUpper {
    fn execute(&self, args: Option<Vec<Value>>) -> Value {
        let mut args = args.unwrap();

        let string = args.pop().unwrap();
        let string = irmatch!(string; Value::String(s) => s);

        Value::String(string.to_uppercase())
    }
}
