use failure::Error;
use smpl::prelude::{ Program, FnId, ModuleId };
use smpl::metadata::Metadata;

use std::collections::HashMap;
use std::sync::Arc;

use crate::err::*;
use crate::module::VmModule;
use crate::std_options::Std;
use crate::value::Value;
use crate::executor::Executor;
use crate::vm_i::*;

use smpl::byte_gen;

pub type CompiledProgram =
    Arc<HashMap<FnId, Arc<byte_gen::ByteCodeFunction>>>;
pub type MappedBuiltins =
    Arc<HashMap<FnId, Arc<BuiltinFn>>>;

#[derive(Debug, Clone)]
pub struct SpawnOptions {
    pub type_check: bool
}

#[derive(Clone)]
pub struct AVM {
    metadata: Arc<Metadata>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
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
            });

        let program = Program::from_parsed(modules)?;

        let mut compiled_fns = HashMap::new();

        for module in program.compilable_modules() {
            for compilable_fn in module.compilable_fns() {
                let fn_id = compilable_fn.fn_id();
                let compiled = byte_gen::compile_to_byte_code(&compilable_fn);
                if compiled_fns.insert(fn_id, Arc::new(compiled)).is_some() {
                    panic!("Multiple functions with ID {}. Should not have passed check_program()", fn_id);
                }
            }

        }

        let mut vm = AVM {
            metadata: Arc::new(program.metadata().clone()),
            compiled: Arc::new(compiled_fns),
            builtins: Arc::new(HashMap::new()),
        };

        for (mod_id, map) in builtins.into_iter() {
            for (name, builtin) in map.into_iter() {
                vm.map_builtin(mod_id, name, builtin)?;
            }
        }

        Ok(vm)
    }

    // Only callable during initialization OR when all executors dropped
    //   due to Arc::get_mut(self.builtins) requirement
    fn map_builtin(
        &mut self,
        mod_id: ModuleId,
        fn_name: String,
        builtin: BuiltinFn,
    ) -> Result<(), VmError> {
        let module_name = self.metadata
            .mod_metadata()
            .get_module_by_id(mod_id)
            .expect(&format!("Could not find metadata for module: {}", mod_id));

        let module_fn_pair = ModuleFnPair {
            module: module_name,
            function: fn_name.clone(),
        };
        let fn_id = self
            .metadata
            .module_fn(mod_id, fn_name.clone())
            .ok_or(VmError::NotAFn(module_fn_pair.clone()))?;

        if self.metadata.is_builtin(fn_id) {

            if (self.compiled.get(&fn_id).is_some()) {
                panic!("ID {} was a builtin function but has a function definition.", fn_id);
            }

            if Arc::get_mut(&mut self.builtins)
                    .expect("self.builtins should not be ref'd anywhere else")
                    .insert(fn_id, Arc::new(builtin)).is_none() {

                Ok(())
            } else {
                Err(VmError::BuiltinCollision(module_fn_pair))
            }
        } else {
            Err(VmError::NotABuiltin(module_fn_pair))
        }
    }

    pub fn query_module(&self, module: &str, name: &str) -> Result<Option<FnHandle>, String> {
        let mod_id = self.metadata
            .mod_metadata()
            .get_module(module.to_string());

        match mod_id {
            Some(mod_id) => Ok(self
                .metadata
                .module_fn(mod_id, name.to_string())
                .map(|fn_id| FnHandle::new(mod_id, fn_id))),

            None => Err(format!("Module '{}' does not exist", module)),
        }
    }

    pub fn spawn_executor(&self, fn_handle: FnHandle,
                          args: Vec<Value>,
                          spawn_options: SpawnOptions) -> Result<Executor, InternalError> {

        if spawn_options.type_check {
            unimplemented!();
        } else {
            Executor::new(self.metadata.clone(),
                          fn_handle,
                          self.compiled.clone(),
                          self.builtins.clone(),
                          args)
        }
    }
}
