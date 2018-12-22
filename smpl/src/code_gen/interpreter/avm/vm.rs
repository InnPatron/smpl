use failure::Error;

use std::collections::HashMap;
use std::rc::Rc;

use petgraph::graph::NodeIndex;

use crate::analysis::*;
use crate::module::*;
use crate::ast::{Ident, Module};

use crate::err::Error as StaticError;

use crate::code_gen::interpreter::module::VmModule;
use crate::code_gen::interpreter::std_options::Std;
use crate::code_gen::interpreter::value::Value;
use crate::code_gen::interpreter::env::Env;
use crate::code_gen::interpreter::err::VmError;

use crate::code_gen::interpreter::vm_i::*;

use super::internal_executor::InternalExecutor;

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

    fn map_builtin(&mut self, mod_id: ModuleId, fn_name: String, builtin: BuiltinFn) -> Result<(), VmError> {

        let fn_id = self.program
            .metadata()
            .module_fn(mod_id, 
                       Ident(fn_name.clone()))
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

    pub fn eval_fn_sync(&self, handle: FnHandle) -> Result<Value, Error> {
        self.eval_fn_args_sync(handle, None)
    }

    pub fn eval_fn_args_sync(&self, handle: FnHandle, args: Option<Vec<Value>>) -> Result<Value, Error> {
        let mut executor = self.eval_fn_args(handle, args)?;

        loop {
            match executor.step() {
                ExecResult::Ok(v) => return Ok(v),
                ExecResult::Pending => (),
                ExecResult::Err(e) => return Err(e),
            }
        }
    }

    pub fn eval_fn(&self, handle: FnHandle) -> Result<Executor, Error> {
        self.eval_fn_args(handle, None)
    }

    pub fn eval_fn_args(&self, handle: FnHandle, args: Option<Vec<Value>>) -> Result<Executor, Error> {
        let id = handle.id();
        if self.program.metadata().is_builtin(id) {
            Ok(Executor::builtin_stub(self.builtins
                .get(&id)
                .expect("Missing a built-in")(args)?
                )
            )
        } else {
            Ok(Executor::new_fn_executor(&self.program, &self.builtins, handle, args))
        }
    }

    pub fn query_module(&self, module: &str, name: &str) -> Result<Option<FnHandle>, String> {
        let module = Ident(module.to_string());
        let name = Ident(name.to_string());
        let mod_id = self.program.universe().module_id(&module);

        match mod_id {
            Some(mod_id) => Ok(self.program
                .metadata()
                .module_fn(mod_id, name)
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
    pub fn_context: FnContext,
    pub exec_state: ExecutorState,
}

pub struct ExecutionContext {
    stack: Vec<StackInfo>,
}

impl ExecutionContext {

    pub fn stack(&self) -> &Vec<StackInfo> {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut Vec<StackInfo> {
        &mut self.stack
    }

    pub fn top(&self) -> &StackInfo {
        self.stack.last().unwrap()
    }

    pub fn top_mut(&mut self) -> &mut StackInfo {
        self.stack.last_mut().unwrap()
    }

    pub fn push_info(&mut self, info: StackInfo) {
        self.stack.push(info);
    }
}

pub enum ExecutorState {
    Fetch(NodeIndex),
    Expr {
        node: NodeIndex,
        tmp_index: usize,
        expr_phase: usize,
    },
    Eval(NodeIndex),
}

pub struct FnContext {
    pub fn_id: FnId,
    pub loop_heads: HashMap<LoopId, NodeIndex>,
    pub loop_result: HashMap<LoopId, bool>,
    pub previous_is_loop_head: bool,
    pub loop_stack: Vec<LoopId>,
    pub return_store: Option<Value>,
}

impl FnContext {
    pub fn new(fn_id: FnId) -> FnContext {

        FnContext {
            fn_id: fn_id,
            loop_heads: HashMap::new(),
            loop_result: HashMap::new(),
            previous_is_loop_head: false,
            loop_stack: Vec::new(),
            return_store: None,
        }
    }

    pub fn get_fn(&self, program: &Program) -> Rc<Function> {
        program.universe().get_fn(self.fn_id)
    }

    pub fn pop_loop_stack(&mut self) -> LoopId {
        self.loop_stack.pop().unwrap()
    }

    pub fn get_loop_result(&self, id: LoopId) -> bool {
        self.loop_result.get(&id).unwrap().clone()
    }

    pub fn get_loop_head(&self, id: LoopId) -> NodeIndex {
        self.loop_heads.get(&id).unwrap().clone()
    }
}

pub enum ExecResult<T, E> {
    Ok(T),
    Pending,
    Err(E),
}

enum ExecutorType<'a> {
    Executor(InternalExecutor<'a>),
    BuiltinStub(Value),
}

pub struct Executor<'a> {
    exec_type: ExecutorType<'a>,
}

impl<'a> Executor<'a> {
    fn new_fn_executor(program: &'a Program,
                        builtins: &'a HashMap<FnId, BuiltinFn>,
                        fn_id: FnHandle, args: Option<Vec<Value>>) -> Executor<'a> {

        let mut exec_context = ExecutionContext {
            stack: Vec::new(),
        };

        // Set up the stack to execute the first node in the called function
        let fn_id = fn_id.id();
        let start = program.universe().get_fn(fn_id).cfg().start();
        exec_context.push_info(StackInfo {
            func: fn_id,
            func_env: Env::new(),
            fn_context: FnContext::new(fn_id),
            exec_state: ExecutorState::Fetch(start),
        });

        // Set up arguments
        if let Some(args) = args {
            for (arg, param_info) in args.into_iter()
                .zip(program.metadata().function_param_ids(fn_id))
            {
                exec_context.top_mut().func_env.map_var(param_info.var_id(), arg);
            }
        }

        Executor { 
            exec_type: ExecutorType::Executor(InternalExecutor {
                program: program,
                context: exec_context,
                builtins: builtins,
            })
        }
    }

    fn builtin_stub(value: Value) -> Executor<'a> {
        Executor {
            exec_type: ExecutorType::BuiltinStub(value),
        }
    }

    pub fn step(&mut self) -> ExecResult<Value, Error> {
        match self.exec_type {
            ExecutorType::Executor(ref mut internal_executor) => internal_executor.step(),
            ExecutorType::BuiltinStub(ref v) => ExecResult::Ok(v.clone()),
        }
    }
}
