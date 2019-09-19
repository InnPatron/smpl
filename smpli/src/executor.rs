use std::sync::Arc;
use std::cell::RefCell;
use std::mem;

use failure::Error;

use smpl::{ FnId, byte_gen };
use smpl::metadata::Metadata;
use smpl::byte_gen::{ to_fn_param, InstructionPointerType, Instruction, Location, Arg, FieldAccess };

use crate::err::*;
use crate::env::Env;
use crate::value::{ Value, ReferableValue, Struct };
use crate::vm_i::{ FnHandle, BuiltinFn };
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
                      fn_handle: FnHandle, 
                      compiled: CompiledProgram,
                      builtins: MappedBuiltins,
                      args: Option<Vec<Value>>) -> Result<Executor, InternalError> {

        let current = 
            Executor::create_stack_info(&*metadata, 
                                        fn_handle, 
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

    pub fn execute_sync(mut self) -> Result<Value, Error> {
        while !self.finished {
            self.step()?;
        }

        Ok(self.return_register.take().unwrap_or(Value::Unit))
    }

    fn create_stack_info(metadata: &Metadata,
                      fn_handle: FnHandle, 
                      compiled: CompiledProgram,
                      builtins: MappedBuiltins,
                      args: Option<Vec<Value>>) -> Result<StackInfo, InternalError> {

        let fn_id = fn_handle.fn_id();
        if metadata.is_builtin(fn_id) {
            Ok(StackInfo::BuiltinStack(BuiltinStack::new(fn_handle, 
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

            let mut stack_info = ByteCodeStack::new(fn_handle, compiled.clone(), builtins.clone());
            
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
                ref current_fn,
                ref compiled,
                ref builtins,
                ref mut env,
                ref mut instruction_pointer,
                ..
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
                    env,
                    &mut self.return_register,
                )?;

                match execute_action {
                    ExecuteAction::PushStack(..) | ExecuteAction::IncrementIP => {
                        let (result, overflow) =
                            instruction_pointer.overflowing_add(1);
                        if overflow {
                            Err(InternalError::RuntimeInstructionError(
                                    RuntimeInstructionError::IPOverflow {
                                        current: *instruction_pointer,
                                        addition: 1,
                                    }))?;
                        } else {
                            *instruction_pointer = result;
                        }
                    }

                    ExecuteAction::SetIP(new_ip) => {
                        *instruction_pointer = new_ip;
                    }

                    ExecuteAction::AddIP(to_add) => {
                        // Overflow/underflow catching
                        if to_add >= 0 {
                            let (result, overflow) = 
                                instruction_pointer.overflowing_add(to_add as u64);

                            if overflow {
                                Err(InternalError::RuntimeInstructionError(
                                    RuntimeInstructionError::IPOverflow {
                                        current: *instruction_pointer,
                                        addition: to_add,
                                    }))?;
                            } else {
                                *instruction_pointer = result; 
                            }
                        } else {
                            let (result, underflow) =
                                instruction_pointer.overflowing_sub((-to_add) as u64);

                            if underflow {
                                Err(InternalError::RuntimeInstructionError(
                                    RuntimeInstructionError::IPUnderflow {
                                        current: *instruction_pointer,
                                        addition: to_add,
                                    }))?;
                            } else {
                                *instruction_pointer = result; 
                            }

                        }

                    }

                    ExecuteAction::PopStack(_) => (),
                };

                execute_action
            }

        };

        match exec_action {
            ExecuteAction::PushStack(fn_handle, args) => {

                let mut stack_frame = Executor::create_stack_info(
                    &*self.metadata,
                    fn_handle,
                    self.compiled.clone(),
                    self.builtins.clone(),
                    args
                )?;
                
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

            ExecuteAction::IncrementIP | ExecuteAction::SetIP(_) | ExecuteAction::AddIP(_) => {
                Ok(())
            }
        }
    }

    fn fetch(env: &Env, location: &Location) -> ReferableValue {

        match location {
            Location::Compound { 
                ref root,
                ref root_index,
                ref path,
            } => {
                let root_ref: ReferableValue = env.get_ref(root).unwrap();

                let root_ref: ReferableValue = match root_index {
                    Some(index_name) => {
                        let index_value: Value = env.get(index_name).unwrap();
                        let index = match index_value {
                            Value::Int(i) => {
                                i as usize
                            }

                            _ => unimplemented!(),
                        };

                        let inner_ref = root_ref.inner_ref();
                        match *inner_ref {
                            Value::Array(ref v) => {
                                v
                                    .get(index)
                                    .unwrap()
                                    .ref_clone()
                            }

                            _ => unimplemented!(),
                        }
                    },

                    None => root_ref,
                };

                let mut next_ref: ReferableValue = root_ref;
                for field_access in path {
                    match field_access {
                        FieldAccess::Field(ref field_name) => {
                            let new_ref = {
                                let inner_ref = next_ref.inner_ref();
                                match *inner_ref {
                                    Value::Struct(ref internal) => {
                                        let field: ReferableValue = internal
                                            .ref_field(field_name)
                                            .unwrap()
                                            .ref_clone();

                                        field
                                    },

                                    _ => unimplemented!(),
                                }
                            };

                            next_ref = new_ref;
                        }

                        FieldAccess::FieldIndex {
                            ref field, 
                            ref index_tmp,
                        } => {

                            let field_ref = {
                                let inner_ref = next_ref.inner_ref();
                                match *inner_ref {
                                    Value::Struct(ref internal) => {
                                        let field: ReferableValue = internal
                                            .ref_field(field)
                                            .unwrap()
                                            .ref_clone();

                                        field
                                    },

                                    _ => unimplemented!(),
                                }
                            };


                            let new_ref = {
                                let index_value: Value = env.get(index_tmp).unwrap();
                                let index = match index_value {
                                    Value::Int(i) => {
                                        i as usize
                                    }

                                    _ => unimplemented!(),
                                };

                                let inner_ref = next_ref.inner_ref();
                                match *inner_ref {
                                    Value::Array(ref v) => {
                                        v
                                            .get(index)
                                            .unwrap()
                                            .ref_clone()
                                    }

                                    _ => unimplemented!(),
                                }
                            };

                            next_ref = new_ref;
                        }
                    }
                }

                next_ref
            }

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
            Location::Compound { .. } => {
                // Guarenteed to get a reference, assuming program is correct
                let reference: ReferableValue = Executor::fetch(env, location);

                *reference.inner_ref_mut() = value;
            }

            Location::Namespace(ref name) => {
                env.map_value(name.clone(), value);
            }

            Location::Tmp(ref name) => {
                env.map_tmp(name.clone(), value);
            }
        }

    }

    fn arg_to_value(env: &Env, arg: &Arg) -> Value {
        match arg {
            Arg::Location(ref arg_loc) => Executor::fetch(env, arg_loc).clone_value(),
            Arg::Int(ref i) => Value::Int(*i),
            Arg::Float(ref f) => Value::Float(*f),
            Arg::Bool(ref b) => Value::Bool(*b),
            Arg::String(ref s) => Value::String(s.clone()),
        }
    }

    fn execute_instruction(instruction: &Instruction, ip: InstructionPointerType,
                           env: &mut Env, return_register: &mut Option<Value>) 
        -> Result<ExecuteAction, InternalError> {

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
        
        macro_rules! int_op {
            ($env: expr, $instruction: expr, $store_loc: expr, $arg1: expr, $arg2: expr, $op: tt) => {{

                let lhs = integer_from_arg!($arg1, $instruction);
                let rhs = integer_from_arg!($arg2, $instruction);

                let to_store = Value::Int(lhs $op rhs);
                Executor::store($env, $store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }}
        }

        macro_rules! float_op {
            ($env: expr, $instruction: expr, $store_loc: expr, $arg1: expr, $arg2: expr, $op: tt) => {{

                let lhs = float_from_arg!($arg1, $instruction);
                let rhs = float_from_arg!($arg2, $instruction);

                let to_store = Value::Float(lhs $op rhs);
                Executor::store($env, $store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }}
        }

        macro_rules! comp_int_op {
            ($env: expr, $instruction: expr, $store_loc: expr, $arg1: expr, $arg2: expr, $op: tt) => {{

                let lhs = integer_from_arg!($arg1, $instruction);
                let rhs = integer_from_arg!($arg2, $instruction);

                let to_store = Value::Bool(lhs $op rhs);
                Executor::store($env, $store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }}
        }

        macro_rules! comp_float_op {
            ($env: expr, $instruction: expr, $store_loc: expr, $arg1: expr, $arg2: expr, $op: tt) => {{

                let lhs = float_from_arg!($arg1, $instruction);
                let rhs = float_from_arg!($arg2, $instruction);

                let to_store = Value::Bool(lhs $op rhs);
                Executor::store($env, $store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }}
        }


        match instruction {
            Instruction::Store(ref store_loc, ref arg) => {
                let to_store = Executor::arg_to_value(env, arg);

                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            },

            Instruction::StoreStructure(ref store_loc, ref string_value_map) => {
                let mut internal_struct = Struct::new(); 

                string_value_map
                    .iter()
                    .for_each(|(key, arg)| {
                        let value = Executor::arg_to_value(env, arg);
                        internal_struct.set_field(key.clone(), value);
                    });

                Executor::store(env, store_loc, Value::Struct(internal_struct));

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::StoreArray1(ref store_loc, ref value) => {
                let internal_array: Vec<ReferableValue> = value
                    .iter()
                    .map(|arg| {
                        let raw_value = Executor::arg_to_value(env, arg);
                        ReferableValue::new(raw_value)
                    })
                    .collect();

                let to_store = Value::Array(internal_array);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            },

            Instruction::StoreArray2(ref store_loc, ref value, size) => {
                let cached_value = Executor::arg_to_value(env, value);
                let internal_array: Vec<ReferableValue> = (0..*size)
                    .map(|_index| {
                        ReferableValue::new(cached_value.clone())
                    })
                    .collect();

                let to_store = Value::Array(internal_array);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::AddI(ref store_loc, ref arg1, ref arg2) => 
                int_op!(env, instruction, store_loc, arg1, arg2, +),

            Instruction::SubI(ref store_loc, ref arg1, ref arg2) => 
                int_op!(env, instruction, store_loc, arg1, arg2, -),

            Instruction::MulI(ref store_loc, ref arg1, ref arg2) => 
                int_op!(env, instruction, store_loc, arg1, arg2, *),

            Instruction::DivI(ref store_loc, ref arg1, ref arg2) => 
                int_op!(env, instruction, store_loc, arg1, arg2, /),

            Instruction::ModI(ref store_loc, ref arg1, ref arg2) => 
                int_op!(env, instruction, store_loc, arg1, arg2, %),

            Instruction::AddF(ref store_loc, ref arg1, ref arg2) => 
                float_op!(env, instruction, store_loc, arg1, arg2, +),

            Instruction::SubF(ref store_loc, ref arg1, ref arg2) => 
                float_op!(env, instruction, store_loc, arg1, arg2, -),

            Instruction::MulF(ref store_loc, ref arg1, ref arg2) => 
                float_op!(env, instruction, store_loc, arg1, arg2, *),

            Instruction::DivF(ref store_loc, ref arg1, ref arg2) => 
                float_op!(env, instruction, store_loc, arg1, arg2, /),

            Instruction::ModF(ref store_loc, ref arg1, ref arg2) => 
                float_op!(env, instruction, store_loc, arg1, arg2, %),

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

            Instruction::GEqI(ref store_loc, ref arg1, ref arg2) => 
                comp_int_op!(env, instruction, store_loc, arg1, arg2, >=),

            Instruction::LEqI(ref store_loc, ref arg1, ref arg2) => 
                comp_int_op!(env, instruction, store_loc, arg1, arg2, <=),

            Instruction::GEI(ref store_loc, ref arg1, ref arg2) => 
                comp_int_op!(env, instruction, store_loc, arg1, arg2, >),

            Instruction::LEI(ref store_loc, ref arg1, ref arg2) => 
                comp_int_op!(env, instruction, store_loc, arg1, arg2, <),

            Instruction::GEqF(ref store_loc, ref arg1, ref arg2) => 
                comp_float_op!(env, instruction, store_loc, arg1, arg2, >=),

            Instruction::LEqF(ref store_loc, ref arg1, ref arg2) => 
                comp_float_op!(env, instruction, store_loc, arg1, arg2, <=),

            Instruction::GEF(ref store_loc, ref arg1, ref arg2) => 
                comp_float_op!(env, instruction, store_loc, arg1, arg2, >),

            Instruction::LEF(ref store_loc, ref arg1, ref arg2) => 
                comp_float_op!(env, instruction, store_loc, arg1, arg2, <),

            Instruction::Eq(ref store_loc, ref arg1, ref arg2) => {
                let v1 = Executor::arg_to_value(env, arg1);
                let v2 = Executor::arg_to_value(env, arg2);

                let to_store = Value::Bool(v1 == v2);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::InEq(ref store_loc, ref arg1, ref arg2) => {
                let v1 = Executor::arg_to_value(env, arg1);
                let v2 = Executor::arg_to_value(env, arg2);

                let to_store = Value::Bool(v1 != v2);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::Negate(ref store_loc, ref arg1) => {
                let to_store = match Executor::arg_to_value(env, arg1) {
                    Value::Int(i) => Value::Int(-i),
                    Value::Float(f) => Value::Float(-f),

                    _ => return Err(InternalError::RuntimeInstructionError(
                        RuntimeInstructionError::ExpectedInt(instruction.clone())))

                };

                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::Invert(ref store_loc, ref arg1) => {
                let b = bool_from_arg!(arg1, instruction);

                let to_store = Value::Bool(!b);
                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::FnCall(ref fn_loc, ref args) => {
                let func = Executor::fetch(env, fn_loc);

                let inner = func.inner_ref();
                match *inner {
                    Value::Function(ref handle) => {
                        let args = if args.len() == 0 {
                            None
                        } else {
                            let args = args
                                .iter()
                                .map(|a| Executor::arg_to_value(env, a))
                                .collect();
                            Some(args)
                        };

                        Ok(ExecuteAction::PushStack(handle.clone(), args))
                    }
                    
                    _ => Err(InternalError::RuntimeInstructionError(
                            RuntimeInstructionError::ExpectedFunction(instruction.clone()))),
                }
            },

            Instruction::Return(ref return_value) => {
                let return_value = return_value
                    .as_ref()
                    .map(|a| Executor::arg_to_value(env, a))
                    .unwrap_or(Value::Unit);
                Ok(ExecuteAction::PopStack(return_value))
            }

            Instruction::TakeReturn(ref store_loc) => {
                let to_store = return_register
                    .take()
                    .ok_or(InternalError::RuntimeInstructionError(
                            RuntimeInstructionError::NoReturnValue(ip)))?;

                Executor::store(env, store_loc, to_store);

                Ok(ExecuteAction::IncrementIP)
            }

            Instruction::Jump(ref jump_target) => {
                Ok(ExecuteAction::SetIP(jump_target.absolute_target()))
            }

            Instruction::JumpCondition(ref jump_target, ref arg1) => {
                let condition = bool_from_arg!(arg1, instruction);
                let action = if condition {
                    ExecuteAction::SetIP(jump_target.absolute_target()) 
                } else {
                    ExecuteAction::IncrementIP
                };

                Ok(action)
            }

            Instruction::JumpNegateCondition(ref jump_target, ref arg1) => {
                let condition = bool_from_arg!(arg1, instruction);
                let action = if !condition {
                    ExecuteAction::SetIP(jump_target.absolute_target()) 
                } else {
                    ExecuteAction::IncrementIP
                };

                Ok(action)
            }

            Instruction::RelJump(ref rel_jump_target) => {
                Ok(ExecuteAction::AddIP(rel_jump_target.relative_target()))
            }

            Instruction::RelJumpCondition(ref rel_jump_target, ref arg1) => {
                let condition = bool_from_arg!(arg1, instruction);
                let action = if condition {
                    ExecuteAction::AddIP(rel_jump_target.relative_target()) 
                } else {
                    ExecuteAction::IncrementIP
                };

                Ok(action)
            }

            Instruction::RelJumpNegateCondition(ref rel_jump_target, ref arg1) => {
                let condition = bool_from_arg!(arg1, instruction);
                let action = if !condition {
                    ExecuteAction::AddIP(rel_jump_target.relative_target()) 
                } else {
                    ExecuteAction::IncrementIP
                };

                Ok(action)
            }

        }
    }
}

enum FetchResult {
    Value(Value),
    ValueRef(ReferableValue) 
}

enum ExecuteAction {
    IncrementIP,
    SetIP(InstructionPointerType),
    AddIP(i64),
    PushStack(FnHandle, Option<Vec<Value>>),
    PopStack(Value),
}

#[derive(Debug)]
enum StackInfo {
    ByteCodeStack(ByteCodeStack),

    BuiltinStack(BuiltinStack),
}

#[derive(Debug)]
struct BuiltinStack {
    handle: FnHandle,
    current_fn: Arc<BuiltinFn>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
    args: Option<Vec<Value>>,
}

impl BuiltinStack {
    fn new(handle: FnHandle, compiled: CompiledProgram, 
           builtins: MappedBuiltins, args: Option<Vec<Value>>) -> BuiltinStack {

        let current_fn = builtins.get(&handle.fn_id()).unwrap().clone();

        BuiltinStack {
            handle: handle,
            current_fn: current_fn,
            compiled: compiled,
            builtins: builtins,
            args: args,
        }
    }
}

#[derive(Debug)]
struct ByteCodeStack {
    handle: FnHandle,
    current_fn: Arc<byte_gen::ByteCodeFunction>,
    compiled: CompiledProgram,
    builtins: MappedBuiltins,
    env: Env,
    instruction_pointer: InstructionPointerType,
}

impl ByteCodeStack {
    fn new(handle: FnHandle, compiled: CompiledProgram, builtins: MappedBuiltins) -> ByteCodeStack {
        let current_fn: Arc<byte_gen::ByteCodeFunction> = compiled.get(&handle.fn_id()).unwrap().clone();
        let env = Env::new();

        ByteCodeStack {
            handle: handle,
            current_fn: current_fn,
            compiled: compiled,
            builtins: builtins,
            env: env,
            instruction_pointer: 0,
        }
    }
}
