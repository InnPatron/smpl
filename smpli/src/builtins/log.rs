use failure::Error;
use std::io::Write;
use smpl::prelude::{UnparsedModule, parse_module};

use crate::*;

pub const MOD_LOG: &'static str = "log";
pub const LOG_PRINT: &'static str = "print";
pub const LOG_PRINTLN: &'static str = "println";

pub const LOG_DECLARATION: &'static str = include_str!("log.smpl");

pub fn vm_module() -> VmModule {
    let input = UnparsedModule::anonymous(LOG_DECLARATION);
    let parsed = parse_module(input).unwrap();

    let module = VmModule::new(parsed)
        .add_builtin(LOG_PRINT,   super::erase(print))
        .add_builtin(LOG_PRINTLN, super::erase(println));

    module
}

#[derive(Fail, Debug)]
#[fail(display = "Logging Error: '{}'", _0)]
pub struct LoggingError(std::io::Error);

async fn print(args: Vec<Value>) -> Result<Value, Error> {
    let args = min_args!(1, args)?;

    for arg in args {
        print!("{}", arg);
    }

    ::std::io::stdout().flush().map_err(|e| LoggingError(e))?;

    Ok(Value::Unit)
}

async fn println(args: Vec<Value>) -> Result<Value, Error> {
    for arg in args {
        print!("{}", arg);
    }

    print!("\n");

    ::std::io::stdout().flush().map_err(|e| LoggingError(e))?;

    Ok(Value::Unit)
}
