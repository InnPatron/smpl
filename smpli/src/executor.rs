use std::sync::Arc;
use std::cell::RefCell;

use failure::Error;

use smpl::{ FnId, byte_gen };
use smpl::metadata::Metadata;
use smpl::byte_gen::{ to_fn_param, InstructionPointerType, Instruction };

use crate::err::*;
use crate::env::Env;
use crate::value::Value;
use crate::vm_i::BuiltinFn;
use crate::vm::{ MappedBuiltins, CompiledProgram };

pub enum ExecResult<T, E> {
    Ok(T),
    Pending,
    Err(E),
}

#[derive(Debug)]
pub struct Executor {
    metadata: Arc<Metadata>,
    top: StackInfo,
    stack: Vec<StackInfo>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
    return_register: Option<Value>,
}

impl Executor {
    pub(super) fn new(metadata: Arc<Metadata>,
                      fn_id: FnId, 
                      compiled: CompiledProgram,
                      builtins: MappedBuiltins,
                      args: Option<Vec<Value>>) -> Result<Executor, InternalError> {

        let current = 
            Executor::create_stack_info(&*metadata, 
                                        fn_id, 
                                        compiled.clone(), 
                                        builtins.clone(), 
                                        args)?;

        let executor = Executor {
            metadata: metadata,
            top: current,
            stack: Vec::new(),
            compiled: compiled,
            builtins: builtins,
            return_register: None,
        };
        
        Ok(executor)
    }

    fn create_stack_info(metadata: &Metadata,
                      fn_id: FnId, 
                      compiled: CompiledProgram,
                      builtins: MappedBuiltins,
                      args: Option<Vec<Value>>) -> Result<StackInfo, InternalError> {

        if metadata.is_builtin(fn_id) {
            Ok(StackInfo::BuiltinStack(BuiltinStack::new(fn_id, 
                                                      compiled.clone(), 
                                                      builtins.clone(), 
                                                      args)))

        } else {

            let param_info: &[_]= metadata.function_param_ids(fn_id);

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

            Ok(StackInfo::ByteCodeStack(stack_info))
        }
    }

    fn step(&mut self) -> Result<(), Error> {
        match self.top {
            StackInfo::BuiltinStack(BuiltinStack {
                ref current_fn,
                ref mut args,
                ..
            }) => {
                // TODO(alex): If StackInfo is going to be used for inspecting,
                //   need args.clone() instead of args.take()
                let result = (*current_fn)(args.take())?;

                self.return_register = Some(result);
                Ok(())
            }

            StackInfo::ByteCodeStack(ByteCodeStack {
                ref id,
                ref current_fn,
                ref compiled,
                ref builtins,
                ref mut env,
                ref mut instruction_pointer,
            }) => unimplemented!(),

        }
    }
}

#[derive(Debug)]
enum StackInfo {
    ByteCodeStack(ByteCodeStack),

    BuiltinStack(BuiltinStack),
}

#[derive(Debug)]
struct BuiltinStack {
    id: FnId,
    current_fn: Arc<BuiltinFn>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
    args: Option<Vec<Value>>,
}

impl BuiltinStack {
    fn new(id: FnId, compiled: CompiledProgram, 
           builtins: MappedBuiltins, args: Option<Vec<Value>>) -> BuiltinStack {

        let current_fn = builtins.get(&id).unwrap().clone();

        BuiltinStack {
            id: id,
            current_fn: current_fn,
            compiled: compiled,
            builtins: builtins,
            args: args,
        }
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
        }
    }
}
