use std::sync::Arc;
use std::cell::RefCell;

use smpl::{ FnId, Program, byte_gen };
use smpl::byte_gen::{ to_fn_param, InstructionPointerType };

use crate::err::*;
use crate::env::Env;
use crate::value::Value;
use crate::vm::CompiledProgram;

pub enum ExecResult<T, E> {
    Ok(T),
    Pending,
    Err(E),
}

#[derive(Debug)]
pub struct Executor {
    stack: Vec<StackInfo>,
    compiled: CompiledProgram,
}

impl Executor {
    pub(super) fn new(program: &Program,
                      fn_id: FnId, 
                      compiled: CompiledProgram,
                      args: Option<Vec<Value>>) -> Result<Executor, InternalError> {

        let mut executor = Executor {
            stack: Vec::new(),
            compiled: compiled.clone(),
        };

        if program.metadata().is_builtin(fn_id) {
            executor.stack.push(StackInfo::Builtin {
                id: fn_id, 
                compiled: compiled.clone(),
                args: args
            });

        } else {

            let param_info: &[_]= program.metadata().function_param_ids(fn_id);

            let args_len = args.as_ref().map(|v| v.len()).unwrap_or(0);

            if param_info.len() != args_len {
                return Err(InternalError::InvalidArgCount(args_len, 
                    ExpectedArgCount::Exact(param_info.len())));
            }

            let mut stack_info = ByteCodeStack::new(fn_id, compiled);
            
            if let Some(args) = args {
                for (arg, param_info) in args
                        .into_iter()
                        .zip(param_info) {

                   stack_info.env 
                        .map_value(to_fn_param(param_info.var_id()), arg);
                }
            }

            executor.stack.push(StackInfo::ByteCodeStack(stack_info))
        }
        
        Ok(executor)
    }

    fn execute(ip: InstructionPointerType, env: &mut Env) -> InstructionPointerType {
        unimplemented!()
    }
}

#[derive(Debug)]
enum StackInfo {
    ByteCodeStack(ByteCodeStack),

    Builtin {
        id: FnId,
        compiled: CompiledProgram,
        args: Option<Vec<Value>>,
    }
}

#[derive(Debug)]
struct ByteCodeStack {
    id: FnId,
    current_fn: Arc<byte_gen::ByteCodeFunction>,
    compiled: CompiledProgram,
    env: Env,
    instruction_pointer: InstructionPointerType,
    return_register: RefCell<Option<Value>>,
}

impl ByteCodeStack {
    fn new(fn_id: FnId, compiled: CompiledProgram) -> ByteCodeStack {
        let current_fn: Arc<byte_gen::ByteCodeFunction> = compiled.get(&fn_id).unwrap().clone();
        let env = Env::new();

        ByteCodeStack {
            id: fn_id,
            current_fn: current_fn,
            compiled: compiled,
            env: env,
            instruction_pointer: 0,
            return_register: RefCell::new(None),
        }
    }
}
