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

struct FnEnv {
    env: HashMap<String, Value>,
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
                Ok(NodeEval::End)
            }

            Node::ExitScope => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::LocalVarDecl(ref decl) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::Assignment(ref assign) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::Expr(ref expr) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::Return(ref ret_expr) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }

            Node::Condition(ref condition) => {
                self.previous_is_loop_head = false;
                Ok(NodeEval::End)
            }
        }
    }
}
