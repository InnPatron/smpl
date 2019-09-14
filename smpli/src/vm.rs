use failure::Error;
use smpl::{ FnId, ModuleId, Program };

use std::collections::HashMap;
use std::sync::Arc;

use crate::env::Env;
use crate::err::*;
use crate::module::VmModule;
use crate::std_options::Std;
use crate::value::Value;

use crate::vm_i::*;

use smpl::byte_gen;

pub struct AVM {
    program: Program,
    compiled: HashMap<FnId, Arc<byte_gen::ByteCodeFunction>>,
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

        let program = smpl::check_program(modules)?;

        let mut compiled_fns = HashMap::new();
        for (fn_id, raw_fn_ref) in program.all_fns() {
            let compiled = byte_gen::compile_to_byte_code(raw_fn_ref);
            if compiled_fns.insert(fn_id, Arc::new(compiled)).is_some() {
                panic!("Multiple functions with ID {}. Should not have passed check_program()", fn_id);
            }
        }
        let mut vm = AVM {
            program: program,
            compiled: compiled_fns,
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
        let module_name = self.program
            .metadata()
            .get_module_by_id(mod_id).unwrap();

        let module_fn_pair = ModuleFnPair {
            module: module_name,
            function: fn_name.clone(),
        };
        let fn_id = self
            .program
            .metadata()
            .module_fn(mod_id, fn_name.clone())
            .ok_or(VmError::NotAFn(module_fn_pair.clone()))?;

        if self.program.metadata().is_builtin(fn_id) {

            if (self.compiled.get(&fn_id).is_some()) {
                panic!("ID {} was a builtin function but has a function definition.", fn_id);
            }

            if self.builtins.insert(fn_id, builtin).is_none() {
                Ok(())
            } else {
                Err(VmError::BuiltinCollision(module_fn_pair))
            }
        } else {
            Err(VmError::NotABuiltin(module_fn_pair))
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
