use std::io::Write;
use failure::Fail;

use ast::Module;
use parser::parse_module;

use code_gen::interpreter::*;

pub const MOD_LOG: &'static str = "log";
pub const LOG_PRINT: &'static str = "print";
pub const LOG_PRINTLN: &'static str = "println";

pub const LOG_DECLARATION: &'static str = include_str!("log.smpl");

pub fn include(modules: &mut Vec<Module>) {
    modules.push(parse_module(LOG_DECLARATION).unwrap());
}

pub fn add<MAP: BuiltinMap>(vm: &mut MAP) {
    vm.insert_builtin(MOD_LOG, LOG_PRINT, Box::new(Print))
        .unwrap();
    vm.insert_builtin(MOD_LOG, LOG_PRINTLN, Box::new(Println))
        .unwrap();
}

pub struct Print;

impl BuiltinFn for Print {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        let args = args.expect("Print() expects at least one argument");

        for arg in args {
            print!("{}", arg);
        }

        ::std::io::stdout().flush();

        Ok(Value::Unit)
    }
}

pub struct Println;

impl BuiltinFn for Println {
    fn execute(&self, args: Option<Vec<Value>>) -> Result<Value, Box<Fail>> {
        if let Some(args) = args {
            for arg in args {
                print!("{}", arg);
            }
        }

        print!("\n");

        ::std::io::stdout().flush();

        Ok(Value::Unit)
    }
}
