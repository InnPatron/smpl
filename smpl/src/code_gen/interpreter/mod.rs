mod vm;
mod value;
mod vm_i;
mod loader;
mod comp;
mod env;
mod avm;

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod vm_tests;

pub mod builtins;

pub use self::vm::VM;
pub use self::value::*;
pub use self::vm_i::*;
pub use self::avm::{AVM, Executor};

use self::vm_i::BuiltinFn;

pub trait BuiltinMap {
    fn insert_builtin(
        &mut self,
        module_str: &str,
        name_str: &str,
        builtin: Box<BuiltinFn>,
    ) -> Result<Option<Box<BuiltinFn>>, String>;
}
