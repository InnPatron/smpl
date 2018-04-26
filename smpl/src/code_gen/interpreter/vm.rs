use std::collections::HashMap;

use petgraph::graph;
use petgraph::graph::NodeIndex;
use petgraph::Direction;
use petgraph::visit::EdgeRef;

use feature::*;

use ast::{Ident, BinOp, UniOp};

use analysis::*;
use analysis::smpl_type::*;
use analysis::metadata::*;

use super::value::Value;

pub struct VM {

}

struct Env {
    env: HashMap<String, Value>,
}

impl Env {
    pub fn map_var(&mut self, var: VarId, value: Value) -> Option<Value> {
        self.map_value(Env::var_id(var), value)
    }

    pub fn map_tmp(&mut self, tmp: TmpId, value: Value) -> Option<Value> {
        self.map_value(Env::tmp_id(tmp), value)
    }

    pub fn map_value(&mut self, name: String, value: Value) -> Option<Value> {
        self.env.insert(name, value)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.env.get_mut(name)
    }

    pub fn get_var_mut(&mut self, id: VarId) -> Option<&mut Value> {
        self.env.get_mut(&Env::var_id(id))
    }

    pub fn get_tmp_mut(&mut self, id: TmpId) -> Option<&mut Value> {
        self.env.get_mut(&Env::tmp_id(id))
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.env.get(name)
    }

    pub fn get_var(&self, id: VarId) -> Option<&Value> {
        self.env.get(&Env::var_id(id))
    }

    pub fn get_tmp(&self, id: TmpId) -> Option<&Value> {
        self.env.get(&Env::tmp_id(id))
    }

    fn tmp_id(id: TmpId) -> String {
        format!("_tmp_{}", id.raw())
    }

    fn var_id(id: VarId) -> String {
        format!("_var_{}", id.raw())
    }
}

struct FnEnv {
    loop_heads: HashMap<LoopId, NodeIndex>,
    loop_result: HashMap<LoopId, bool>,
    previous_is_loop_head: bool,
    loop_stack: Vec<LoopId>, 
}

enum NodeEval {
    Value(Value),
    Next(NodeIndex),
    End,
}

impl FnEnv {

    fn pop_loop_stack(&mut self) -> LoopId {
        self.loop_stack.pop().unwrap()
    }

    fn get_loop_result(&self, id: LoopId) -> bool {
        self.loop_result.get(&id).unwrap().clone()
    }

    fn get_loop_head(&self, id: LoopId) -> NodeIndex {
        self.loop_heads.get(&id).unwrap().clone()
    }

    fn eval_node(&mut self, graph: &CFG, current: NodeIndex) -> Result<NodeEval, ()> {
        match *graph.node_weight(current) {
            Node::End => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::Start => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::BranchSplit(id) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::BranchMerge(id) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::LoopHead(id) => {
                self.previous_is_loop_head = true;

                self.loop_stack.push(id);
                self.loop_heads.insert(id, current);

                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::LoopFoot(id) => {
                self.previous_is_loop_head = false;

                let loop_id = self.pop_loop_stack();
                let loop_result = self.get_loop_result(loop_id);

                if loop_result {
                    return Ok(NodeEval::Next(self.get_loop_head(loop_id)));
                } else {
                    let neighbors = neighbors!(graph, current);
                    for n in neighbors {
                        match *node_w!(graph, n) {
                            Node::LoopHead(_) => continue,
                            _ => return Ok(NodeEval::Next(n)),
                        }
                    }
                }

                unreachable!();
            }

            Node::Continue(id) => {
                self.previous_is_loop_head = false;
                let loop_id = self.pop_loop_stack();
                Ok(NodeEval::Next(self.get_loop_head(loop_id)))
            }

            Node::Break(_) => {
                self.previous_is_loop_head = false;

                let neighbors = neighbors!(graph, current);
                for n in neighbors {
                    match *node_w!(graph, current) {
                        Node::LoopFoot(_) => return Ok(NodeEval::Next(n)),
                        _ => continue,
                    }
                }

                unreachable!();
            }

            Node::EnterScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::ExitScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::LocalVarDecl(ref decl) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Assignment(ref assign) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Expr(ref expr) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Return(ref ret_expr) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }

            Node::Condition(ref condition) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::Next(graph.next(current)))
            }
        }
    }
}
