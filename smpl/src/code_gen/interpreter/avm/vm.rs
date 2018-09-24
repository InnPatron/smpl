use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use petgraph::graph::NodeIndex;
use petgraph::Direction;

use analysis::*;
use analysis::{Value as AbstractValue};
use analysis::smpl_type::*;

use code_gen::interpreter::value::{Struct, Value as Value};
use code_gen::interpreter::env::Env;

use super::node_fetch::*;

type TmpIndex = usize;

pub struct AVM {
    program: Program
}

impl AVM {
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
    return_value: Option<Value>,
}

impl ExecutionContext {
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
}

impl FnContext {
    fn new(program: &Program, fn_id: FnId) -> FnContext {

        FnContext {
            fn_id: fn_id,
            loop_heads: HashMap::new(),
            loop_result: HashMap::new(),
            previous_is_loop_head: false,
            loop_stack: Vec::new(),
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
    state: ExecutorState,
    context: ExecutionContext,
    program: &'a Program,
}

impl<'a> Executor<'a> {
    pub fn step(&mut self) -> ExecResult<Value, ()> {
        match self.state {
            ExecutorState::Fetch(node_index) => {
                let node_fetch = match node_fetch(&mut self.context.top_mut().fn_context, 
                                                  self.program, 
                                                  node_index) {
                    Ok(d) => d,
                    Err(e) => return ExecResult::Err(e),

                };
                match node_fetch {
                    FetchResult::Next(next) => self.state = ExecutorState::Fetch(next),
                    FetchResult::Expr(expr_phase) => { 
                        self.state = ExecutorState::Expr {
                            node: node_index,
                            tmp_index: 0,
                            expr_phase: expr_phase,
                        };
                    },
                    FetchResult::Return(v) => return ExecResult::Ok(v),
                }
            }

            ExecutorState::Expr {
                node: node,
                tmp_index: tmp_index,
                expr_phase: expr_phase
            } => unimplemented!(),

            ExecutorState::Eval(node_index) => unimplemented!(),
        }

        ExecResult::Pending
    }
}
