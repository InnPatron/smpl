use std::cell::RefCell;
use std::rc::Rc;

use petgraph::graph::NodeIndex;

use crate::analysis::type_cons::Type;
use crate::analysis::Value as AbstractValue;
use crate::analysis::*;

use crate::code_gen::interpreter::comp::*;
use crate::code_gen::interpreter::value::{Struct, Value};

use super::vm::*;

const LHS_PHASE: usize = 0;
const RHS_PHASE: usize = 1;

pub enum ExprEvalResult {
    Value(Value, TmpId),
    PhaseChange(Value, TmpId),
    FnCall(FnId, Option<Vec<Value>>),
    Finished(Value, TmpId),
}

impl ExprEvalResult {
    fn from_tmp_result(
        tmp_result: TmpResult,
        node: &Node,
        current_tmp: usize,
        current_tmp_id: TmpId,
        expr_phase: usize,
    ) -> ExprEvalResult {
        match *node {
            Node::LocalVarDecl(ref data) => {
                assert_eq!(
                    expr_phase, 0,
                    "Local Variable Declaration node only has phase 0. Found phase {}",
                    expr_phase
                );
                let expr = data.decl.init_expr();

                match tmp_result {
                    TmpResult::Value(v) => {
                        if current_tmp == expr.order_length() - 1 {
                            ExprEvalResult::Finished(v, current_tmp_id)
                        } else {
                            ExprEvalResult::Value(v, current_tmp_id)
                        }
                    }

                    TmpResult::FnCall(fn_id, args) => ExprEvalResult::FnCall(fn_id, args),
                }
            }

            Node::Assignment(ref data) => {
                assert!(
                    expr_phase < 2,
                    "Assignment node only has phase 0-1. Found phase {}",
                    expr_phase
                );

                match tmp_result {
                    TmpResult::Value(v) => {
                        if expr_phase == LHS_PHASE {
                            let expr = data.assignment.access();
                            if current_tmp == expr.order_length() - 1 {
                                ExprEvalResult::PhaseChange(v, current_tmp_id)
                            } else {
                                ExprEvalResult::Value(v, current_tmp_id)
                            }
                        } else {
                            let expr = data.assignment.value();
                            if current_tmp == expr.order_length() - 1 {
                                ExprEvalResult::Finished(v, current_tmp_id)
                            } else {
                                ExprEvalResult::Value(v, current_tmp_id)
                            }
                        }
                    }

                    TmpResult::FnCall(fn_id, args) => ExprEvalResult::FnCall(fn_id, args),
                }
            }

            Node::Expr(ref data) => {
                assert_eq!(
                    expr_phase, 0,
                    "Expression node only has phase 0. Found phase {}",
                    expr_phase
                );
                let expr = &data.expr;

                match tmp_result {
                    TmpResult::Value(v) => {
                        if current_tmp == expr.order_length() - 1 {
                            ExprEvalResult::Finished(v, current_tmp_id)
                        } else {
                            ExprEvalResult::Value(v, current_tmp_id)
                        }
                    }

                    TmpResult::FnCall(fn_id, args) => ExprEvalResult::FnCall(fn_id, args),
                }
            }

            Node::Return(ref data) => {
                assert_eq!(
                    expr_phase, 0,
                    "Return node only has phase 0. Found phase {}",
                    expr_phase
                );
                let expr = match data.expr {
                    Some(ref expr) => expr,
                    None => unreachable!(),
                };

                match tmp_result {
                    TmpResult::Value(v) => {
                        if current_tmp == expr.order_length() - 1 {
                            ExprEvalResult::Finished(v, current_tmp_id)
                        } else {
                            ExprEvalResult::Value(v, current_tmp_id)
                        }
                    }

                    TmpResult::FnCall(fn_id, args) => ExprEvalResult::FnCall(fn_id, args),
                }
            }

            Node::Condition(ref data) => {
                assert_eq!(
                    expr_phase, 0,
                    "Condition node only has phase 0. Found phase {}",
                    expr_phase
                );
                let expr = &data.expr;

                match tmp_result {
                    TmpResult::Value(v) => {
                        if current_tmp == expr.order_length() - 1 {
                            ExprEvalResult::Finished(v, current_tmp_id)
                        } else {
                            ExprEvalResult::Value(v, current_tmp_id)
                        }
                    }

                    TmpResult::FnCall(fn_id, args) => ExprEvalResult::FnCall(fn_id, args),
                }
            }

            _ => unreachable!(),
        }
    }
}

pub fn eval_node_tmp(
    program: &Program,
    context: &mut ExecutionContext,
    current_node: NodeIndex,
    tmp_index: usize,
    expr_phase: usize,
) -> ExprEvalResult {
    let func = context.top().fn_context.get_fn(program);
    let cfg = func.cfg();
    let node = cfg.node_weight(current_node);
    match *node {
        Node::LocalVarDecl(ref data) => {
            assert_eq!(
                expr_phase, 0,
                "Local Variable Declaration node only has phase 0. Found phase {}",
                expr_phase
            );
            let expr = data.decl.init_expr();
            let tmp_id = expr.tmp_by_index(tmp_index);
            let tmp = expr.get_tmp(tmp_id);
            ExprEvalResult::from_tmp_result(
                eval_tmp(program, context, tmp),
                node,
                tmp_index,
                tmp_id,
                expr_phase,
            )
        }

        Node::Assignment(ref data) => {
            assert!(
                expr_phase < 2,
                "Assignment node only has phase 0-1. Found phase {}",
                expr_phase
            );

            if expr_phase == LHS_PHASE {
                let expr = data.assignment.access();
                let tmp_id = expr.tmp_by_index(tmp_index);
                let tmp = expr.get_tmp(tmp_id);
                ExprEvalResult::from_tmp_result(
                    eval_tmp(program, context, tmp),
                    node,
                    tmp_index,
                    tmp_id,
                    expr_phase,
                )
            } else {
                let expr = data.assignment.value();
                let tmp_id = expr.tmp_by_index(tmp_index);
                let tmp = expr.get_tmp(tmp_id);
                ExprEvalResult::from_tmp_result(
                    eval_tmp(program, context, tmp),
                    node,
                    tmp_index,
                    tmp_id,
                    expr_phase,
                )
            }
        }

        Node::Expr(ref data) => {
            assert_eq!(
                expr_phase, 0,
                "Expression node only has phase 0. Found phase {}",
                expr_phase
            );
            let tmp_id = data.expr.tmp_by_index(tmp_index);
            let tmp = data.expr.get_tmp(tmp_id);
            ExprEvalResult::from_tmp_result(
                eval_tmp(program, context, tmp),
                node,
                tmp_index,
                tmp_id,
                expr_phase,
            )
        }

        Node::Return(ref data) => {
            assert_eq!(
                expr_phase, 0,
                "Return node only has phase 0. Found phase {}",
                expr_phase
            );
            let expr = match data.expr {
                Some(ref expr) => expr,
                None => unreachable!(),
            };
            let tmp_id = expr.tmp_by_index(tmp_index);
            let tmp = expr.get_tmp(tmp_id);
            ExprEvalResult::from_tmp_result(
                eval_tmp(program, context, tmp),
                node,
                tmp_index,
                tmp_id,
                expr_phase,
            )
        }

        Node::Condition(ref data) => {
            assert_eq!(
                expr_phase, 0,
                "Condition node only has phase 0. Found phase {}",
                expr_phase
            );
            let tmp_id = data.expr.tmp_by_index(tmp_index);
            let tmp = data.expr.get_tmp(tmp_id);
            ExprEvalResult::from_tmp_result(
                eval_tmp(program, context, tmp),
                node,
                tmp_index,
                tmp_id,
                expr_phase,
            )
        }

        _ => unreachable!(),
    }
}

enum TmpResult {
    Value(Value),
    FnCall(FnId, Option<Vec<Value>>),
}

fn eval_tmp(_program: &Program, context: &mut ExecutionContext, tmp: &Tmp) -> TmpResult {
    let tmp_value = match *tmp.value().data() {
        AbstractValue::Literal(ref literal) => match *literal {
            Literal::Bool(b) => Value::Bool(b),
            Literal::Int(i) => Value::Int(i as i32),
            Literal::Float(f) => Value::Float(f as f32),
            Literal::String(ref s) => Value::String(s.to_string()),
        },

        AbstractValue::Binding(ref binding) => {
            let id = binding.get_id().unwrap();
            match id {
                BindingId::Var(id) => context
                    .top()
                    .func_env
                    .get_var(id)
                    .map(|v| v.clone())
                    .unwrap(),
                BindingId::Fn(id) => Value::Function(id.into()),
            }
        }

        AbstractValue::FieldAccess(ref access) => {
            let path = access.path();

            let root_var = path.root_var_id();
            let root_var = context.top().func_env.ref_var(root_var).unwrap();

            let mut value = root_var;

            if let Some(e) = path.root_indexing_expr() {
                value = {
                    let borrow = value.borrow();
                    let indexer = context.top().func_env.get_tmp(e).unwrap();
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

                            let indexer = context.top().func_env.get_tmp(*indexer).unwrap();
                            let indexer = irmatch!(indexer; Value::Int(i) => i);
                            field.get(indexer as usize).unwrap().clone()
                        };
                    }
                }
            }
            let borrow = value.borrow();
            let ret = borrow.clone();

            ret
        }

        AbstractValue::FnCall(ref call) => {
            if context.top_mut().fn_context.return_store.is_some() {
                return TmpResult::Value(context.top_mut().fn_context.return_store.take().unwrap());
            }

            let fn_value = call.fn_value();

            let fn_id = irmatch!(context
                .top()
                .func_env
                .get_tmp(fn_value)
                .expect("Type checker should have caught fn call on non-fn binding"); 
                Value::Function(fn_handle) => fn_handle.id());

            let args: Option<Vec<_>> = call.args().map(|v| {
                v.iter()
                    .map(|tmp| {
                        context
                            .top()
                            .func_env
                            .get_tmp(tmp.data().clone())
                            .unwrap()
                            .clone()
                    })
                    .collect()
            });

            return TmpResult::FnCall(fn_id, args);
        }

        AbstractValue::BinExpr(ref op, ref lhs, ref rhs) => {
            let lh_id = lhs.data().clone();
            let rh_id = rhs.data().clone();

            let lh_v = context.top().func_env.get_tmp(lh_id).unwrap();
            let rh_v = context.top().func_env.get_tmp(rh_id).unwrap();

            match lhs.get_type().unwrap() {
                Type::Int => {
                    let lhs = irmatch!(lh_v; Value::Int(i) => i);
                    let rhs = irmatch!(rh_v; Value::Int(i) => i);

                    if is_math(op.clone()) {
                        let result = math_op(op.clone(), lhs, rhs);
                        Value::Int(result)
                    } else {
                        let result = cmp(op.clone(), lhs, rhs);
                        Value::Bool(result)
                    }
                }

                Type::Float => {
                    let lhs = irmatch!(lh_v; Value::Float(f) => f);
                    let rhs = irmatch!(rh_v; Value::Float(f) => f);

                    if is_math(op.clone()) {
                        let result = math_op(op.clone(), lhs, rhs);
                        Value::Float(result)
                    } else {
                        let result = cmp(op.clone(), lhs, rhs);
                        Value::Bool(result)
                    }
                }

                Type::Bool => {
                    let lhs = irmatch!(lh_v; Value::Bool(b) => b);
                    let rhs = irmatch!(rh_v; Value::Bool(b) => b);

                    if is_logical(op.clone()) {
                        let result = logical(op.clone(), lhs, rhs);
                        Value::Bool(result)
                    } else {
                        let result = cmp(op.clone(), lhs, rhs);
                        Value::Bool(result)
                    }
                }

                _ => Value::Bool(partial_cmp(op.clone(), lh_v, rh_v)),
            }
        }

        AbstractValue::UniExpr(ref _op, ref t) => {
            let t_id = t.data().clone();
            let t_v = context.top().func_env.get_tmp(t_id).unwrap();

            irmatch!(t.get_type().unwrap();
                    Type::Float => {
                        let f = irmatch!(t_v; Value::Float(f) => f);
                        Value::Float(negate(f))
                    },

                    Type::Int => {
                        let i = irmatch!(t_v; Value::Int(i) => i);
                        Value::Int(negate(i))
                    },

                    Type::Bool => {
                        let b = irmatch!(t_v; Value::Bool(b) => b);
                        Value::Bool(not(b))
                    }
            )
        }

        AbstractValue::StructInit(ref init) => {
            let mut s = Struct::new();

            match init.field_init() {
                Some(ref v) => {
                    let init_order = init.init_order().unwrap();
                    for (ident, (_, ref tmp)) in init_order.into_iter().zip(v.iter()) {
                        let field_value =
                            context.top().func_env.get_tmp(tmp.data().clone()).unwrap();
                        s.set_field(ident.as_str().to_string(), field_value.clone());
                    }
                }

                None => (),
            }

            Value::Struct(s)
        }

        AbstractValue::ArrayInit(ref init) => match *init {
            ArrayInit::List(ref v) => Value::Array(
                v.iter()
                    .map(|element| {
                        let element_id = element.data().clone();
                        Rc::new(RefCell::new(
                            context.top().func_env.get_tmp(element_id).unwrap().clone(),
                        ))
                    })
                    .collect(),
            ),

            ArrayInit::Value(ref v, size) => {
                let element = context.top().func_env.get_tmp(v.data().clone()).unwrap();
                Value::Array(
                    (0..size)
                        .into_iter()
                        .map(|_| Rc::new(RefCell::new(element.clone())))
                        .collect(),
                )
            }
        },

        AbstractValue::Indexing(ref indexing) => {
            let array = context
                .top()
                .func_env
                .ref_tmp(indexing.array.data().clone())
                .unwrap();
            let array = array.borrow();
            let array = irmatch!(*array; Value::Array(ref v) => v);

            let indexer = context
                .top()
                .func_env
                .get_tmp(indexing.indexer.data().clone())
                .unwrap();
            let indexer = irmatch!(indexer; Value::Int(i) => i);

            let indexed_value = array.get(indexer as usize).unwrap();
            let indexed_value = indexed_value.borrow();
            indexed_value.clone()
        }

        AbstractValue::ModAccess(ref access) => {
            let fn_id = access.fn_id().unwrap();
            Value::Function(fn_id.into())
        }

        AbstractValue::AnonymousFn(ref a_fn) => {
            let fn_id = a_fn.fn_id();
            Value::Function(fn_id.into())
        }

        AbstractValue::TypeInst(ref type_inst) => {
            let fn_id = type_inst.get_id().unwrap();
            Value::Function(fn_id.into())
        }
    };

    TmpResult::Value(tmp_value)
}
