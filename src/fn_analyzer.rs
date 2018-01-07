use control_flow::*;
use typed_ast::*;
use semantic_ck::{Universe, ScopedData, Err};
use smpl_type::*;

use petgraph::graph::NodeIndex;

#[derive(Clone, Debug)]
pub enum TypeErr {

}

pub fn analyze_fn(global_scope: &ScopedData, fn_type: &FunctionType, cfg: &CFG) -> Result<(), Err> {

    let mut neighbors = cfg.graph().neighbors.directed(cfg.start()).collect();
    let mut current_scope = global_scope.clone();
    let scope_stack = Vec::new();

    scope_stack.push(current_scope.clone());

    unimplemented!();
}
