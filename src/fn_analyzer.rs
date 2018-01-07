use std::collections::HashMap;

use control_flow::*;
use typed_ast::*;
use semantic_ck::{Universe, ScopedData, Err};
use smpl_type::*;

use petgraph::graph::NodeIndex;

#[derive(Clone, Debug)]
pub enum TypeErr {

}

pub fn analyze_fn(universe: &Universe, global_scope: &ScopedData, cfg: &CFG, fn_type: &FunctionType) -> Result<(), Err> {

    let mut scope_stack = Vec::new();
    let mut current_scope = global_scope.clone();

    let mut to_check = cfg.after_start().unwrap();

    loop {
        match *node_w!(cfg, to_check) {
            Node::End => break,
            Node::Start | Node::BranchSplit | Node::BranchMerge | Node::LoopHead => {
                to_check = cfg.next(to_check).unwrap();
            }

            Node::EnterScope => scope_stack.push(current_scope.clone()),
            Node::ExitScope => current_scope = scope_stack.pop().expect("If CFG was generated properly and the graph is being walked correctly, there should be a scope to pop"),



            Node::LocalVarDecl(ref var_decl) => {
                let name = var_decl.var_name();
                let var_id = var_decl.var_id();
                let type_path = var_decl.type_path();

                let type_id = current_scope.type_id(type_path)?;

                current_scope.insert_var(name.to_owned(), var_id, type_id);

                to_check = cfg.next(to_check).unwrap();
            }

            _ => unimplemented!(),
        }
    }

    if scope_stack.len() != 0 {
        panic!("Should be no left over scopes if CFG was generated properly and fully-walked.");
    }
    unimplemented!();
}
