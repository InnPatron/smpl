use failure::Error;

use crate::module::*;
use crate::parser::parse_module;

use crate::code_gen::interpreter::*;

pub const MOD_ERR: &'static str = "err";

pub const ERR_PANIC: &'static str = "panic";
pub const ERR_PANIC_MSG: &'static str = "panic_msg";
pub const ERR_ASSERT: &'static str = "assert";

pub const ERR_DECLARATION: &'static str = include_str!("err.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(ERR_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let mut module = VmModule::new(parsed)
        .add_builtin(ERR_PANIC, panic)
        .add_builtin(ERR_PANIC_MSG, panic_msg)
        .add_builtin(ERR_ASSERT, assert);

    module
}

#[derive(Fail, Debug)]
pub struct RuntimeError(Option<String>);

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Some(ref s) => write!(f, "A runtime error occured.\n{}", s),
            None => write!(f, "A runtime error occured.")
        }
    }
}


fn panic(_args: Option<Vec<Value>>) -> Result<Value, Error> {
    Err(RuntimeError(None))?
}


fn panic_msg(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = args.unwrap();
    let a = args.remove(0);

    match a {
        Value::String(s) => Err(RuntimeError(Some(s)))?,
        _ => unreachable!(),
    }
}

fn assert(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = args.unwrap();
    let a = args.remove(0);

    let a = irmatch!(a; Value::Bool(a) => a);

    if a {
        Ok(Value::Unit)
    } else {
        Err(RuntimeError(Some("Assertion failed".to_string())))?
    }
}
