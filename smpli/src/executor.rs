use std::sync::Arc;
use std::cell::RefCell;

use smpl::{ FnId, Program, byte_gen };
use smpl::byte_gen::{ to_fn_param, InstructionPointerType };

use crate::err::*;
use crate::env::Env;
use crate::value::Value;
use crate::vm::{ MappedBuiltins, CompiledProgram };

pub enum ExecResult<T, E> {
    Ok(T),
    Pending,
    Err(E),
}

#[derive(Debug)]
pub struct Executor {
    top: StackInfo,
    stack: Vec<StackInfo>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
}

impl Executor {
    pub(super) fn new(program: &Program,
                      fn_id: FnId, 
                      compiled: CompiledProgram,
                      builtins: MappedBuiltins,
                      args: Option<Vec<Value>>) -> Result<Executor, InternalError> {

        let current = if program.metadata().is_builtin(fn_id) {
            StackInfo::Builtin {
                id: fn_id, 
                compiled: compiled.clone(),
                builtins: builtins.clone(),
                args: args,
            }

        } else {

            let param_info: &[_]= program.metadata().function_param_ids(fn_id);

            let args_len = args.as_ref().map(|v| v.len()).unwrap_or(0);

            if param_info.len() != args_len {
                return Err(InternalError::InvalidArgCount(args_len, 
                    ExpectedArgCount::Exact(param_info.len())));
            }

            let mut stack_info = ByteCodeStack::new(fn_id, compiled.clone(), builtins.clone());
            
            if let Some(args) = args {
                for (arg, param_info) in args
                        .into_iter()
                        .zip(param_info) {

                   stack_info.env 
                        .map_value(to_fn_param(param_info.var_id()), arg);
                }
            }

            StackInfo::ByteCodeStack(stack_info)
        };

        let executor = Executor {
            top: current,
            stack: Vec::new(),
            compiled: compiled.clone(),
            builtins: builtins,
        };
        
        Ok(executor)
    }

    fn execute_instruction(&mut self) {
        match self.top {
            StackInfo::Builtin {
                ref id,
                ref compiled,
                ref builtins,
                ref args
            } => unimplemented!(),

            StackInfo::ByteCodeStack(ByteCodeStack {
                ref id,
                ref current_fn,
                ref compiled,
                ref builtins,
                ref mut env,
                ref mut instruction_pointer,
                ref mut return_register,
            }) => unimplemented!(),

        }
    }
        
}

#[derive(Debug)]
enum StackInfo {
    ByteCodeStack(ByteCodeStack),

    Builtin {
        id: FnId,
        compiled: CompiledProgram,
        builtins: MappedBuiltins,
        args: Option<Vec<Value>>,
    }
}

#[derive(Debug)]
struct ByteCodeStack {
    id: FnId,
    current_fn: Arc<byte_gen::ByteCodeFunction>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
    env: Env,
    instruction_pointer: InstructionPointerType,
    return_register: Option<Value>,
}

impl ByteCodeStack {
    fn new(fn_id: FnId, compiled: CompiledProgram, builtins: MappedBuiltins) -> ByteCodeStack {
        let current_fn: Arc<byte_gen::ByteCodeFunction> = compiled.get(&fn_id).unwrap().clone();
        let env = Env::new();

        ByteCodeStack {
            id: fn_id,
            current_fn: current_fn,
            compiled: compiled,
            builtins: builtins,
            env: env,
            instruction_pointer: 0,
            return_register: None,
        }
    }
}
