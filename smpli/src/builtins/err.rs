use failure::Error;
use smpl::{UnparsedModule, parse_module};

use crate::*;

pub const MOD_ERR: &'static str = "err";

pub const ERR_PANIC: &'static str = "panic";
pub const ERR_PANIC_MSG: &'static str = "panic_msg";
pub const ERR_ASSERT: &'static str = "assert";

pub const ERR_DECLARATION: &'static str = include_str!("err.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(ERR_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(ERR_PANIC,     boxed_panic)
        .add_builtin(ERR_PANIC_MSG, boxed_panic_msg)
        .add_builtin(ERR_ASSERT,    boxed_assert);

    module
}

#[derive(Fail, Debug)]
pub struct RuntimeError(Option<String>);

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Some(ref s) => write!(f, "A runtime error occured.\n{}", s),
            None => write!(f, "A runtime error occured."),
        }
    }
}


async_box!(panic);
async_box!(panic_msg);
async_box!(assert);

async fn panic(_args: Option<Vec<Value>>) -> Result<Value, Error> {
    Err(RuntimeError(None))?
}

async fn panic_msg(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = args.unwrap();
    let a = args.remove(0);

    match a {
        Value::String(s) => Err(RuntimeError(Some(s)))?,
        _ => unreachable!(),
    }
}

async fn assert(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let mut args = args.unwrap();
    let a = args.remove(0);

    let a = irmatch!(a; Value::Bool(a) => a);

    if a {
        Ok(Value::Unit)
    } else {
        Err(RuntimeError(Some("Assertion failed".to_string())))?
    }
}
