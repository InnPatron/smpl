use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use petgraph::graph::NodeIndex;
use petgraph::Direction;

use analysis::*;
use analysis::{Value as AbstractValue};
use analysis::smpl_type::*;

use ast::{Ident, Module};

use err::Err;

use code_gen::interpreter::value::{Struct, Value as Value};
use code_gen::interpreter::env::Env;

use code_gen::interpreter::BuiltinMap;
use code_gen::interpreter::loader;
use code_gen::interpreter::vm_i::*;


use super::node_fetch::*;
use super::expr_eval::*;
use super::node_eval::*;

type TmpIndex = usize;

pub struct AVM {
    program: Program,
    builtins: HashMap<FnId, Box<BuiltinFn>>,
}

impl AVM {
    pub fn new(user_modules: Vec<Module>) -> Result<AVM, Err> {
        let modules = loader::include(user_modules);
        let program = check_program(modules)?;
        let mut vm = AVM {
            program: program,
            builtins: HashMap::new(),
        };

        loader::load(&mut vm);

        Ok(vm)
    }

    pub fn eval_fn_sync(&self, handle: FnHandle) -> Result<Value, ()> {
        self.eval_fn_args_sync(handle, None)
    }

    pub fn eval_fn_args_sync(&self, handle: FnHandle, args: Option<Vec<Value>>) -> Result<Value, ()> {
        let mut executor = self.eval_fn_args(handle, args);

        loop {
            match executor.step() {
                ExecResult::Ok(v) => return Ok(v),
                ExecResult::Pending => (),
                ExecResult::Err(e) => return Err(e),
            }
        }
    }

    pub fn eval_fn(&self, handle: FnHandle) -> Executor {
        self.eval_fn_args(handle, None)
    }

    pub fn eval_fn_args(&self, handle: FnHandle, args: Option<Vec<Value>>) -> Executor {
        let id = handle.id();
        if self.program.metadata().is_builtin(id) {
            Executor::builtin_stub(self.builtins
                .get(&id)
                .expect("Missing a built-in")
                .execute(args))
        } else {
            Executor::new_fn_executor(&self.program, handle, args)
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

impl BuiltinMap for AVM {
    fn insert_builtin(
        &mut self,
        module_str: &str,
        name_str: &str,
        builtin: Box<BuiltinFn>,
    ) -> Result<Option<Box<BuiltinFn>>, String> {
        let module = Ident(module_str.to_string());
        let name = Ident(name_str.to_string());
        let mod_id = self.program.universe().module_id(&module);

        match mod_id {
            Some(mod_id) => match self.program.metadata().module_fn(mod_id, name) {
                Some(fn_id) => {
                    if self.program.metadata().is_builtin(fn_id) {
                        Ok(self.builtins.insert(fn_id, builtin))
                    } else {
                        Err(format!(
                            "{}::{} is not a valid builtin function",
                            module_str, name_str
                        ))
                    }
                }

                None => Err(format!("{} is not a function in {}", name_str, module_str)),
            },

            None => Err(format!("Module '{}' does not exist", module_str)),
        }
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
    return_value: Option<Value>,
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
    fn new(program: &Program, fn_id: FnId) -> FnContext {

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

pub struct Executor<'a> {
    context: ExecutionContext,
    program: &'a Program,
}

impl<'a> Executor<'a> {
    pub fn step(&mut self) -> ExecResult<Value, ()> {
        match self.context.top().exec_state {
            ExecutorState::Fetch(node_index) => {
                let node_fetch = match node_fetch(&mut self.context.top_mut().fn_context, 
                                                  self.program, 
                                                  node_index) {
                    Ok(d) => d,
                    Err(e) => return ExecResult::Err(e),

                };
                match node_fetch {
                    FetchResult::Next(next) => self.context.top_mut().exec_state = ExecutorState::Fetch(next),
                    FetchResult::Expr(expr_phase) => { 
                        self.context.top_mut().exec_state = ExecutorState::Expr {
                            node: node_index,
                            tmp_index: 0,
                            expr_phase: expr_phase,
                        };
                    },
                    FetchResult::Return(v) => {
                        let stack = self.context.stack_mut();

                        let _returning_fn = stack.pop();

                        match stack.last_mut() {
                            Some(ref mut top) => top.fn_context.return_store = Some(v),

                            None => return ExecResult::Ok(v),
                        }
                    },
                }
            }

            ExecutorState::Expr {
                node: node,
                tmp_index: tmp_index,
                expr_phase: expr_phase
            } => {
                match eval_node_tmp(self.program, 
                                    &mut self.context, 
                                    node, 
                                    tmp_index, 
                                    expr_phase) {
                    ExprEvalResult::Value(v, tmp_id) => {
                        self.context.top_mut().exec_state = ExecutorState::Expr {
                            node: node,
                            tmp_index: tmp_index + 1, // Go to the next tmp
                            expr_phase: expr_phase,
                        };

                        // Map the tmp
                        self.context.top_mut().func_env.map_tmp(tmp_id, v);
                    },

                    ExprEvalResult::PhaseChange(v, tmp_id) => {
                        self.context.top_mut().exec_state = ExecutorState::Expr {
                            node: node,
                            tmp_index: 0, // Go to the start of the next phase
                            expr_phase: expr_phase + 1, // Go to the next phase
                        };

                        // Map the tmp
                        self.context.top_mut().func_env.map_tmp(tmp_id, v);
                    },

                    ExprEvalResult::FnCall(fn_id, args) => {
                        let fn_context = FnContext::new(self.program, fn_id);
                        let start = fn_context.get_fn(self.program).cfg().start();

                        // Push a StackInfo onto the stack, diverting the control flow to that
                        // function.
                        self.context.push_info(StackInfo {
                            func: fn_id,
                            func_env: Env::new(),
                            fn_context: fn_context,
                            exec_state: ExecutorState::Fetch(start),
                        });
                    },

                    ExprEvalResult::Finished(v, tmp_id) => {
                        // All expressions have been evaluated. Perform any node post-processing
                        // eval
                        self.context.top_mut().exec_state = ExecutorState::Eval(node);

                        // Map the tmp
                        self.context.top_mut().func_env.map_tmp(tmp_id, v);
                    },
                }
            }

            ExecutorState::Eval(current_node) => {
                let result = eval_node(self.context.top_mut(), self.program, current_node);

                let result = match result {
                    Ok(r) => r,
                    Err(e) => {
                        return ExecResult::Err(e);
                    }
                };

                match result {

                    NodeEval::Next(next_node) => {
                        self.context.top_mut().exec_state = ExecutorState::Fetch(next_node);
                    }

                    NodeEval::Return(value) => {
                        let stack = self.context.stack_mut();

                        let _returning_fn = stack.pop();

                        match stack.last_mut() {
                            Some(ref mut top) => top.fn_context.return_store = Some(value),

                            None => return ExecResult::Ok(value),
                        }
                    },
                }
            }
        }

        ExecResult::Pending
    }
}
