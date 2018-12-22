use std::collections::HashMap;

use crate::code_gen::interpreter::BuiltinFn;
use crate::module::ParsedModule;

pub struct VmModule {
    parsed: ParsedModule,
    builtins: HashMap<String, BuiltinFn>,
}

impl VmModule {
    pub fn new(m: ParsedModule) -> VmModule {
        VmModule::with_builtins(m, HashMap::new())
    }

    pub fn with_builtins(m: ParsedModule, v: HashMap<String, BuiltinFn>) -> VmModule {
        VmModule {
            parsed: m,
            builtins: v,
        }
    }

    pub fn add_builtin(mut self, fn_name: &str, builtin: BuiltinFn) -> Result<VmModule, ()> {
        if self.builtins
            .insert(fn_name.to_string(), builtin)
            .is_none() {

            Ok(self)
        } else {
            // TODO: Error
            Err(())
        }
    }
}
