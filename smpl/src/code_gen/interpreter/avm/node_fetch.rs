use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use petgraph::graph::NodeIndex;
use petgraph::Direction;

use analysis::*;
use analysis::{Value as AbstractValue};
use analysis::smpl_type::*;

use code_gen::interpreter::value::{Struct, Value as Value};
use super::vm::FnContext;

type ExprStage = usize;

pub enum FetchResult {
    Next(NodeIndex),
    Expr(ExprStage),
    Return(Value),
}

pub fn node_fetch(context: &mut FnContext, program: &Program, current: NodeIndex) -> Result<FetchResult, ()> {
    let func = context.get_fn(program);
    match *func.cfg().node_weight(current) {
        Node::End => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Return(Value::Unit))
        }

        Node::Start => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Next(func.cfg().next(current)))
        }

        Node::BranchSplit(_) => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Next(func.cfg().next(current)))
        }

        Node::BranchMerge(_) => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Next(func.cfg().next(current)))
        }

        Node::LoopHead(ref data) => {
            context.previous_is_loop_head = true;

            context.loop_stack.push(data.loop_id);
            context.loop_heads.insert(data.loop_id, current);

            Ok(FetchResult::Next(func.cfg().next(current)))
        }

        Node::LoopFoot(_) => {
            context.previous_is_loop_head = false;

            let loop_id = context.pop_loop_stack();
            let loop_result = context.get_loop_result(loop_id);

            if loop_result {
                return Ok(FetchResult::Next(context.get_loop_head(loop_id)));
            } else {
                let cfg = func.cfg();
                let neighbors = neighbors!(&*cfg, current);
                for n in neighbors {
                    match *node_w!(func.cfg(), n) {
                        Node::LoopHead(_) => continue,
                        _ => return Ok(FetchResult::Next(n)),
                    }
                }
            }

            unreachable!();
        }

        Node::Continue(_) => {
            context.previous_is_loop_head = false;
            let loop_id = context.pop_loop_stack();
            Ok(FetchResult::Next(context.get_loop_head(loop_id)))
        }

        Node::Break(_) => {
            context.previous_is_loop_head = false;

            let cfg = func.cfg();
            let neighbors = neighbors!(&*cfg, current);
            for n in neighbors {
                match *node_w!(func.cfg(), current) {
                    Node::LoopFoot(_) => return Ok(FetchResult::Next(n)),
                    _ => continue,
                }
            }

            unreachable!();
        }

        Node::EnterScope => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Next(func.cfg().next(current)))
        }

        Node::ExitScope => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Next(func.cfg().next(current)))
        }

        Node::LocalVarDecl(ref data) => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Expr(0))
        }

        Node::Assignment(ref data) => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Expr(0))
        }

        Node::Expr(ref data) => {
            context.previous_is_loop_head = false;
            Ok(FetchResult::Expr(0))
        }

        Node::Return(ref data) => {
            context.previous_is_loop_head = false;
            match data.expr {
                Some(_) => Ok(FetchResult::Expr(0)),
                None => Ok(FetchResult::Return(Value::Unit)),
            }
        }

        Node::Condition(ref data) => {
            Ok(FetchResult::Expr(0))
        }
    }
}
