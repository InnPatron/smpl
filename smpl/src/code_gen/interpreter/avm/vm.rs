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
}

pub enum GraphInfo {
    Node(NodeIndex),
    NodeExpr(NodeIndex, TmpIndex),
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

pub struct FnContext {
    pub fn_id: FnId,
    pub loop_heads: HashMap<LoopId, NodeIndex>,
    pub loop_result: HashMap<LoopId, bool>,
    pub previous_is_loop_head: bool,
    pub loop_stack: Vec<LoopId>,
    pub graph_info: GraphInfo,
}

impl FnContext {
    fn new(program: &Program, fn_id: FnId) -> FnContext {

        let info = GraphInfo::Node(program.universe().get_fn(fn_id).cfg().start());
        FnContext {
            fn_id: fn_id,
            loop_heads: HashMap::new(),
            loop_result: HashMap::new(),
            previous_is_loop_head: false,
            loop_stack: Vec::new(),
            graph_info: info,
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
