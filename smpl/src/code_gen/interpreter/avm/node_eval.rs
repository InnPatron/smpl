use petgraph::graph::NodeIndex;

use failure::Error;

use crate::analysis::*;

use super::vm::StackInfo;
use crate::code_gen::interpreter::value::Value;

pub enum NodeEval {
    Next(NodeIndex),
    Return(Value),
}

/// Performs any Node post-processing
pub fn eval_node(
    stack_info: &mut StackInfo,
    program: &Program,
    current: NodeIndex,
) -> Result<NodeEval, Error> {
    let context = &mut stack_info.fn_context;
    let func = context.get_fn(program);
    match *func.cfg().node_weight(current) {
        Node::LocalVarDecl(ref data) => {
            context.previous_is_loop_head = false;
            let value_tmp_id = data.decl.init_expr().last();
            let value = stack_info.func_env.get_tmp(value_tmp_id).unwrap();
            stack_info.func_env.map_var(data.decl.var_id(), value);
            Ok(NodeEval::Next(func.cfg().next(current)))
        }

        Node::Assignment(ref data) => {
            context.previous_is_loop_head = false;
            let path = data.assignment.assignee().path();

            let root_var = path.root_var_id();
            let root_var = stack_info.func_env.ref_var(root_var).unwrap();

            let mut value = root_var;
            if let Some(tmp) = path.root_indexing_expr() {
                value = {
                    let borrow = value.borrow();
                    let indexer = stack_info.func_env.get_tmp(tmp).unwrap();
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
                            let field_to_index = struct_value.ref_field(f.name().as_str()).unwrap();
                            let field_to_index = field_to_index.borrow();
                            let field = irmatch!(*field_to_index; Value::Array(ref a) => a);

                            let indexer = stack_info.func_env.get_tmp(*indexer).unwrap();
                            let indexer = irmatch!(indexer; Value::Int(i) => i);
                            field.get(indexer as usize).unwrap().clone()
                        };
                    }
                }
            }

            let result_tmp_id = data.assignment.value().last();
            let result = stack_info.func_env.get_tmp(result_tmp_id).unwrap();

            let mut borrow = value.borrow_mut();
            *borrow = result;

            Ok(NodeEval::Next(func.cfg().next(current)))
        }

        Node::Return(ref data) => {
            context.previous_is_loop_head = false;
            let value = match data.expr {
                Some(ref expr) => {
                    let result_tmp_id = expr.last();
                    let result = stack_info.func_env.get_tmp(result_tmp_id);
                    result.unwrap()
                }

                None => Value::Unit,
            };
            Ok(NodeEval::Return(value))
        }

        Node::Condition(ref data) => {
            let value_tmp_id = data.expr.last();
            let value = stack_info.func_env.get_tmp(value_tmp_id).unwrap();
            let value = irmatch!(value; Value::Bool(b) => b);

            let (t_b, f_b) = func.cfg().after_condition(current);
            let next = if value { t_b } else { f_b };

            if context.previous_is_loop_head {
                let id = context.pop_loop_stack();
                context.loop_result.insert(id, value);
                context.loop_stack.push(id);
            }

            context.previous_is_loop_head = false;
            Ok(NodeEval::Next(next))
        }

        Node::Expr(_) => Ok(NodeEval::Next(func.cfg().next(current))),

        _ => unreachable!(),
    }
}
