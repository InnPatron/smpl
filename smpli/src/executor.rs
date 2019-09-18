use std::sync::Arc;
use std::cell::RefCell;
use std::mem;

use failure::Error;

use smpl::{ FnId, byte_gen };
use smpl::metadata::Metadata;
use smpl::byte_gen::{ to_fn_param, InstructionPointerType, Instruction, Location, Arg };

use crate::err::*;
use crate::env::Env;
use crate::value::{ Value, ReferableValue };
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
    finished: bool,
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
            finished: false,
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
        let exec_action = match self.top {
            StackInfo::BuiltinStack(BuiltinStack {
                ref current_fn,
                ref mut args,
                ..
            }) => {
                // TODO(alex): If StackInfo is going to be used for inspecting,
                //   need args.clone() instead of args.take()
                let result = (*current_fn)(args.take())?;

                ExecuteAction::PopStack(result)
            }

            StackInfo::ByteCodeStack(ByteCodeStack {
                ref id,
                ref current_fn,
                ref compiled,
                ref builtins,
                ref mut env,
                ref mut instruction_pointer,
            }) => {
                let instructions = current_fn.instructions();

                let instruction = instructions
                    .get(*instruction_pointer as usize)
                    .ok_or(InternalError::InstructionPointerOutOfBounds {
                        ip: *instruction_pointer,
                        max: instructions.len()
                    })?;

                let execute_action = Executor::execute_instruction(
                    instruction,
                    *instruction_pointer,
                    env
                )?;

                match execute_action {
                    ExecuteAction::PushStack(_) | ExecuteAction::IncrementIP => {
                        *instruction_pointer += 1; 
                    }

                    ExecuteAction::UpdateIP(new_ip) => {
                        *instruction_pointer = new_ip;
                    }

                    ExecuteAction::PopStack(_) => (),
                };

                execute_action
            }

        };

        match exec_action {
            ExecuteAction::PushStack(mut stack_frame) => {
                
                // Push the new stack frame by swapping it with self.top
                //  The old top is pushed onto the stack
                mem::swap(&mut stack_frame, &mut self.top);

                let old_top = stack_frame;
                self.stack.push(old_top);

                Ok(())
            }

            ExecuteAction::PopStack(value) => {
                match self.stack.pop() {

                    Some(mut stack_top) => { 
                        // Swap the Executor's top StackInfo and swap it with the top of the
                        //   internal stack
                        // Drop the old top
                        mem::swap(&mut stack_top, &mut self.top);
                        let _to_drop = stack_top;

                        self.return_register = Some(value);

                        Ok(())
                    }
 
                    // No more stack frames to pop
                    // Finished execution of the orignal function
                    None => {
                        
                        self.finished = true;
                        self.return_register = Some(value);
                        Ok(())
                    }
                }
            }

            ExecuteAction::IncrementIP | ExecuteAction::UpdateIP(_) => {
                Ok(())
            }
        }
    }

    fn fetch(env: &Env, location: &Location) -> ReferableValue {

        match location {
            Location::Compound { .. } => unimplemented!(),

            Location::Namespace(ref name) => {
                env.ref_value(name).unwrap()
            }

            Location::Tmp(ref name) => {
                env.ref_tmp(name).unwrap()
            }
        }
    }

    fn store(env: &mut Env, location: &Location, value: Value) {
        match location {
            Location::Compound { .. } => unimplemented!(),

            Location::Namespace(ref name) => {
                env.map_value(name.clone(), value);
            }

            Location::Tmp(ref name) => {
                env.map_tmp(name.clone(), value);
            }
        }

    }

    fn execute_instruction(instruction: &Instruction, ip: InstructionPointerType,
                           env: &mut Env) -> Result<ExecuteAction, InternalError> {

        macro_rules! integer_from_arg {
            ($arg: expr, $instr: expr) => {{
                let from_arg = match $arg {
                    Arg::Location(ref arg_loc) => Executor::fetch(env, arg_loc).clone_value(),
                    Arg::Int(ref i) => Value::Int(*i),

                    _ => return Err(InternalError::InvalidInstruction(
                        IIReason::ExpectedInt($instr.clone()))),
                };

                match from_arg {
                    Value::Int(i) => i,

                    _ => return Err(InternalError::RuntimeInstructionError(
                        RuntimeInstructionError::ExpectedInt($instr.clone()))),
                }
            }}
        }

        macro_rules! float_from_arg {
            ($arg: expr, $instr: expr) => {{
                let from_arg = match $arg {
                    Arg::Location(ref arg_loc) => Executor::fetch(env, arg_loc).clone_value(),
                    Arg::Float(ref f) => Value::Float(*f),

                    _ => return Err(InternalError::InvalidInstruction(
                        IIReason::ExpectedFloat($instr.clone()))),
                };

                match from_arg {
                    Value::Float(f) => f,

                    _ => return Err(InternalError::RuntimeInstructionError(
                        RuntimeInstructionError::ExpectedFloat($instr.clone()))),
                }
            }}
        }

        macro_rules! bool_from_arg {
            ($arg: expr, $instr: expr) => {{
                let from_arg = match $arg {
                    Arg::Location(ref arg_loc) => Executor::fetch(env, arg_loc).clone_value(),
                    Arg::Bool(ref b) => Value::Bool(*b),

                    _ => return Err(InternalError::InvalidInstruction(
                        IIReason::ExpectedBool($instr.clone()))),
                };

                match from_arg {
                    Value::Bool(b) => b,

                    _ => return Err(InternalError::RuntimeInstructionError(
                        RuntimeInstructionError::ExpectedBool($instr.clone()))),
                }
            }}
        }

        match instruction {
            Instruction::Store(ref store_loc, ref arg) => {
                let to_store = match arg {
                    Arg::Location(ref arg_loc) => Executor::fetch(env, arg_loc).clone_value(),
                    Arg::Int(ref i) => Value::Int(*i),
                    Arg::Float(ref f) => Value::Float(*f),
                    Arg::Bool(ref b) => Value::Bool(*b),
                    Arg::String(ref s) => Value::String(s.clone()),
                };

                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            },

            Instruction::StoreStructure(ref store_loc, ref string_value_map) => unimplemented!(),
            Instruction::StoreArray1(ref store_loc, ref value) => unimplemented!(),
            Instruction::StoreArray2(ref store_loc, ref value, size) => unimplemented!(),

            Instruction::AddI(ref store_loc, ref arg1, ref arg2) => {
                let lhs = integer_from_arg!(arg1, instruction);
                let rhs = integer_from_arg!(arg2, instruction);

                let to_store = Value::Int(lhs + rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::SubI(ref store_loc, ref arg1, ref arg2) => {
                let lhs = integer_from_arg!(arg1, instruction);
                let rhs = integer_from_arg!(arg2, instruction);

                let to_store = Value::Int(lhs - rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::MulI(ref store_loc, ref arg1, ref arg2) => {
                let lhs = integer_from_arg!(arg1, instruction);
                let rhs = integer_from_arg!(arg2, instruction);

                let to_store = Value::Int(lhs * rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::DivI(ref store_loc, ref arg1, ref arg2) => {
                let lhs = integer_from_arg!(arg1, instruction);
                let rhs = integer_from_arg!(arg2, instruction);

                let to_store = Value::Int(lhs / rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::ModI(ref store_loc, ref arg1, ref arg2) => {
                let lhs = integer_from_arg!(arg1, instruction);
                let rhs = integer_from_arg!(arg2, instruction);

                let to_store = Value::Int(lhs % rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::AddF(ref store_loc, ref arg1, ref arg2) => {
                let lhs = float_from_arg!(arg1, instruction);
                let rhs = float_from_arg!(arg2, instruction);

                let to_store = Value::Float(lhs + rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::SubF(ref store_loc, ref arg1, ref arg2) => {
                let lhs = float_from_arg!(arg1, instruction);
                let rhs = float_from_arg!(arg2, instruction);

                let to_store = Value::Float(lhs - rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::MulF(ref store_loc, ref arg1, ref arg2) => {
                let lhs = float_from_arg!(arg1, instruction);
                let rhs = float_from_arg!(arg2, instruction);

                let to_store = Value::Float(lhs * rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::DivF(ref store_loc, ref arg1, ref arg2) => {
                let lhs = float_from_arg!(arg1, instruction);
                let rhs = float_from_arg!(arg2, instruction);

                let to_store = Value::Float(lhs / rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::ModF(ref store_loc, ref arg1, ref arg2) => {
                let lhs = float_from_arg!(arg1, instruction);
                let rhs = float_from_arg!(arg2, instruction);

                let to_store = Value::Float(lhs % rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::And(ref store_loc, ref arg1, ref arg2) => {
                let lhs = bool_from_arg!(arg1, instruction);
                let rhs = bool_from_arg!(arg2, instruction);

                let to_store = Value::Bool(lhs && rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::Or(ref store_loc, ref arg1, ref arg2) => {
                let lhs = bool_from_arg!(arg1, instruction);
                let rhs = bool_from_arg!(arg2, instruction);

                let to_store = Value::Bool(lhs || rhs);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::GEq(ref store_loc, ref arg1, ref arg2) => unimplemented!(),
            Instruction::LEq(ref store_loc, ref arg1, ref arg2) => unimplemented!(),
            Instruction::GE(ref store_loc, ref arg1, ref arg2) => unimplemented!(),
            Instruction::LE(ref store_loc, ref arg1, ref arg2) => unimplemented!(),
            Instruction::Eq(ref store_loc, ref arg1, ref arg2) => unimplemented!(),
            Instruction::InEq(ref store_loc, ref arg1, ref arg2) => unimplemented!(),

            Instruction::Negate(ref store_loc, ref arg1) => unimplemented!(),
            Instruction::Invert(ref store_loc, ref arg1) => unimplemented!(),

            Instruction::FnCall(ref fn_loc, ref args) => unimplemented!(),
            Instruction::Return(ref return_value) => unimplemented!(),
            Instruction::TakeReturn(ref store_loc) => unimplemented!(),

            Instruction::Jump(ref jump_target) => unimplemented!(),
            Instruction::JumpCondition(ref jump_target, ref arg1) => unimplemented!(),
            Instruction::JumpNegateCondition(ref jump_target, ref arg1) => unimplemented!(),

            Instruction::RelJump(ref rel_jump_target) => unimplemented!(),
            Instruction::RelJumpCondition(ref rel_jump_target, ref arg1) => unimplemented!(),
            Instruction::RelJumpNegateCondition(ref rel_jump_target, ref arg1) => unimplemented!(),

        }
    }
}

enum FetchResult {
    Value(Value),
    ValueRef(ReferableValue) 
}

enum ExecuteAction {
    IncrementIP,
    UpdateIP(InstructionPointerType),
    PushStack(StackInfo),
    PopStack(Value),
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
