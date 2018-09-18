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

enum NodeEval {
    Next(NodeIndex),
    Return(Value),
}

fn eval_node(context: &mut FnContext, program: &Program, current: NodeIndex) -> Result<NodeEval, ()> {
    let func = context.get_fn(program);
    match *func.cfg().node_weight(current) {
        Node::End => unreachable!(),

        Node::Start => unreachable!(),

        Node::BranchSplit(_) => unreachable!(),

        Node::BranchMerge(_) => unreachable!(),

        Node::LoopHead(ref data) => unreachable!(),

        Node::LoopFoot(_) => unreachable!(),

        Node::Continue(_) => unreachable!(),

        Node::Break(_) => unreachable!(),

        Node::EnterScope => unreachable!(),

        Node::ExitScope => unreachable!(),

        Node::LocalVarDecl(ref data) => {
            context.previous_is_loop_head = false;
            let value = Expr::eval_expr(context.vm, &self.env, data.decl.init_expr());
            context.env.map_var(data.decl.var_id(), value);
            Ok(NodeEval::Next(func.cfg().next(current)))
        }

        Node::Assignment(ref data) => {
            context.previous_is_loop_head = false;
            let path = data.assignment.assignee().path();

            let root_var = path.root_var_id();
            let root_var = context.env.ref_var(root_var).unwrap();

            let mut value = root_var;
            if let Some(tmp) = path.root_indexing_expr() {
                value = {
                    let borrow = value.borrow();
                    let indexer = context.env.get_tmp(tmp).unwrap();
                    let indexer = irmatch!(indexer; Value::Int(i) => i);
                    let array = irmatch!(*borrow; Value::Array(ref a) => a);
                    array.get(indexer as usize).unwrap().clone()
                };
            }

            for ps in path.path() {
                match *ps {
                    PathSegment::Ident(ref f) => {
                        value = {
                            let value = value.borrow();
                            let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                            struct_value.ref_field(f.name().as_str()).unwrap()
                        };
                    }

                    PathSegment::Indexing(ref f, ref indexer) => {
                        value = {
                            let value = value.borrow();
                            let struct_value = irmatch!(*value; Value::Struct(ref s) => s);
                            let field_to_index =
                                struct_value.ref_field(f.name().as_str()).unwrap();
                            let field_to_index = field_to_index.borrow();
                            let field = irmatch!(*field_to_index; Value::Array(ref a) => a);

                            
                            let indexer = context.env.get_tmp(*indexer).unwrap();
                            let indexer = irmatch!(indexer; Value::Int(i) => i);
                            field.get(indexer as usize).unwrap().clone()
                        };
                    }
                }
            }

            let result = Expr::eval_expr(context.vm, &self.env, data.assignment.value());

            let mut borrow = value.borrow_mut();
            *borrow = result;

            Ok(NodeEval::Next(func.cfg().next(current)))
        }

        Node::Expr(ref data) => {
            context.previous_is_loop_head = false;
            Expr::eval_expr(context.vm, &self.env, &data.expr);
            Ok(NodeEval::Next(func.cfg().next(current)))
        }

        Node::Return(ref data) => {
            context.previous_is_loop_head = false;
            let value = match data.expr {
                Some(ref expr) => Expr::eval_expr(context.vm, &self.env, expr),
                None => Value::Unit,
            };
            Ok(NodeEval::Return(value))
        }

        Node::Condition(ref data) => {
            let value = Expr::eval_expr(context.vm, &self.env, &data.expr);
            let value = irmatch!(value; Value::Bool(b) => b);
            let (t_b, f_b) = func.cfg().after_condition(current);
            let next = if value { t_b } else { f_b };

            if context.previous_is_loop_head {
                let id = context.pop_loop_stack();
                context.loop_result.insert(id, value);
                context.loop_stack.push(id);
            }

            context.previous_is_loop_head = false;
            Ok(NodeEval::Next(func.cfg().next(next)))
        }
    }
}
