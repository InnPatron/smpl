mod vm;
mod value;
mod vm_i;
mod loader;
mod comp;
mod env;

pub mod builtins;

pub use self::vm::VM;
pub use self::value::*;
pub use self::vm_i::*;

use self::vm_i::BuiltinFn;

pub trait BuiltinMap {
    fn insert_builtin(
        &mut self,
        module_str: &str,
        name_str: &str,
        builtin: Box<BuiltinFn>,
    ) -> Result<Option<Box<BuiltinFn>>, String>;
}
