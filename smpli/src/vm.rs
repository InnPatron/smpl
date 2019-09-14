use failure::Error;
use smpl::{ FnId, ModuleId, Program, check_program };

use std::collections::HashMap;
use std::rc::Rc;

use crate::env::Env;
use crate::err::VmError;
use crate::module::VmModule;
use crate::std_options::Std;
use crate::value::Value;

use crate::vm_i::*;

pub struct AVM {
    program: Program,
    builtins: HashMap<FnId, BuiltinFn>,
}

impl AVM {
    pub fn new(std: Std, mut modules: Vec<VmModule>) -> Result<AVM, VmError> {
        std.include(&mut modules);

        let mut builtins = Vec::new();
        let modules = modules
            .into_iter()
            .map(|vmmod| {
                builtins.push((vmmod.id(), vmmod.builtins));
                vmmod.parsed
            })
            .collect();

        let program = check_program(modules)?;

        let mut vm = AVM {
            program: program,
            builtins: HashMap::new(),
        };

        for (mod_id, map) in builtins.into_iter() {
            for (name, builtin) in map.into_iter() {
                vm.map_builtin(mod_id, name, builtin)?;
            }
        }

        Ok(vm)
    }

    fn map_builtin(
        &mut self,
        mod_id: ModuleId,
        fn_name: String,
        builtin: BuiltinFn,
    ) -> Result<(), VmError> {
        let fn_id = self
            .program
            .metadata()
            .module_fn(mod_id, fn_name.clone())
            .ok_or(VmError::NotAFn(mod_id, fn_name.clone()))?;

        if self.program.metadata().is_builtin(fn_id) {
            if self.builtins.insert(fn_id, builtin).is_none() {
                Ok(())
            } else {
                Err(VmError::BuiltinCollision(mod_id, fn_name))
            }
        } else {
            Err(VmError::NotABuiltin(mod_id, fn_name))
        }
    }

    pub fn query_module(&self, module: &str, name: &str) -> Result<Option<FnHandle>, String> {
        let mod_id = self.program.metadata().get_module(module.to_string());

        match mod_id {
            Some(mod_id) => Ok(self
                .program
                .metadata()
                .module_fn(mod_id, name.to_string())
                .map(|fn_id| fn_id.into())),

            None => Err(format!("Module '{}' does not exist", module)),
        }
    }

    pub fn program(&self) -> &Program {
        &self.program
    }
}

pub struct StackInfo {
    pub func: FnId,
    pub func_env: Env,
}

pub enum ExecResult<T, E> {
    Ok(T),
    Pending,
    Err(E),
}
