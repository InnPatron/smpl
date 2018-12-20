use std::io::Write;
use failure::Error;

use crate::min_args;
use crate::module::*;
use crate::parser::parse_module;

use crate::code_gen::interpreter::*;

pub const MOD_LOG: &'static str = "log";
pub const LOG_PRINT: &'static str = "print";
pub const LOG_PRINTLN: &'static str = "println";

pub const LOG_DECLARATION: &'static str = include_str!("log.smpl");

pub fn include(modules: &mut Vec<ParsedModule>) {
    let input = UnparsedModule::anonymous(LOG_DECLARATION.to_string());
    modules.push(parse_module(input).unwrap());
}

pub fn add<MAP: BuiltinMap>(vm: &mut MAP) {
    vm.insert_builtin(MOD_LOG, LOG_PRINT, print)
        .unwrap();
    vm.insert_builtin(MOD_LOG, LOG_PRINTLN, println)
        .unwrap();
}

#[derive(Fail, Debug)]
#[fail(display = "Logging Error: '{}'", _0)]
pub struct LoggingError(std::io::Error);

fn print(args: Option<Vec<Value>>) -> Result<Value, Error> {
    let args = min_args!(1, args)?;

    for arg in args {
        print!("{}", arg);
    }

    ::std::io::stdout()
        .flush()
        .map_err(|e| LoggingError(e))?;

    Ok(Value::Unit)
}

fn println(args: Option<Vec<Value>>) -> Result<Value, Error> {
    if let Some(args) = args {
        for arg in args {
            print!("{}", arg);
        }
    }

    print!("\n");

    ::std::io::stdout()
        .flush()
        .map_err(|e| LoggingError(e))?;

    Ok(Value::Unit)
}
