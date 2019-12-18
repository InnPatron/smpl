use std::collections::HashSet;

use petgraph::graph::NodeIndex;

use crate::span::Span;

use super::control_data::Node;
use super::control_flow::CFG;
use super::error::{AnalysisError, ControlFlowError};
use super::semantic_data::*;
use super::semantic_data::{AnonymousFn, Function};

pub fn return_trace_prime(
    to_trace: &Function,
) -> Result<(), AnalysisError> {

    match to_trace {
        Function::SMPL(ref smpl_fn) => {
            let cfg = smpl_fn.cfg();
            let fn_span = &smpl_fn.span();
            check_returns_form(cfg, fn_span)
        }

        Function::Anonymous(ref anon_fn) => match anon_fn {
            AnonymousFn::Reserved(..) => {
                panic!("Anonymous function should be resolved")
            }

            AnonymousFn::Resolved {
                ref cfg,
                ref span,
                ..
            } => {
                check_returns_form(cfg, span)
            }
        },

        Function::Builtin(..) => {
            panic!("Unable to return trace builtin functions")
        }
    }
}

fn check_returns_form(
    cfg: &CFG,
    fn_span: &Span,
) -> Result<(), AnalysisError> {

    let end = cfg.end();
    let scope_exit = cfg.previous(end);

    let unknown = cfg.previous(scope_exit);

    let mut traced = HashSet::new();
    let mut node_stack = Vec::new();
    node_stack.push(unknown);

    for _ in 0..cfg.graph().node_count() {
        let to_trace = node_stack.pop();
        match to_trace {
            Some(id) => {
                if (traced.contains(&id)) == false {
                    traced.insert(id);

                    let more_to_trace = return_check_id(cfg, id, fn_span)?;
                    if let Some(vec) = more_to_trace {
                        node_stack.extend(vec);
                    }
                }
            }

            None => return Ok(()),
        }
    }

    unreachable!();
}

fn return_check_id(
    cfg: &CFG,
    id: NodeIndex,
    fn_span: &Span,
) -> Result<Option<Vec<NodeIndex>>, AnalysisError> {
    match *cfg.node_weight(id) {
        Node::Return(..) => Ok(None),

        Node::BranchMerge(_) => Ok(Some(cfg.before_branch_merge(id))),

        Node::ExitScope => Ok(Some(vec![cfg.previous(id)])),

        _ => return Err(ControlFlowError::MissingReturn(fn_span.clone()).into()),
    }
}
