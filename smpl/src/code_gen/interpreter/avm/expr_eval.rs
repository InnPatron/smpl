use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use analysis::*;
use analysis::{Value as AbstractValue};
use analysis::smpl_type::*;

use code_gen::interpreter::value::{Struct, Value as Value};
use code_gen::interpreter::comp::*;

use super::vm::*;

enum TmpResult {
    Value(Value),
    FnCall(FnId),
}

fn eval_tmp(vm: &AVM, context: &ExecutionContext, tmp: &Tmp) -> TmpResult {
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
                BindingId::Var(id) => context.top().func_env.get_var(id).map(|v| v.clone()).unwrap(),
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
                            let field_to_index =
                                struct_value.ref_field(f.name().as_str()).unwrap();
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
            let fn_id = match call.get_id().unwrap() {
                BindingId::Var(var) => {
                    let var = context.top().func_env.get_var(var).unwrap();
                    let function = irmatch!(var; Value::Function(fn_id) => fn_id);
                    function.id()
                }

                BindingId::Fn(fn_id) => fn_id,
            };

            /*
            let args: Option<Vec<_>> = call.args().map(|v| {
                v.iter()
                    .map(|tmp| expr_env.get_tmp(tmp.data().clone()).unwrap().clone())
                    .collect()
            });

            match args {
                Some(args) => {
                    if args.len() > 0 {
                        vm.eval_fn_args(fn_id.into(), args)
                    } else {
                        vm.eval_fn(fn_id.into())
                    }
                }

                None => vm.eval_fn(fn_id.into()),
            }
            */
            unimplemented!();
        }

        AbstractValue::BinExpr(ref op, ref lhs, ref rhs) => {
            let lh_id = lhs.data().clone();
            let rh_id = rhs.data().clone();

            let lh_v = context.top().func_env.get_tmp(lh_id).unwrap();
            let rh_v = context.top().func_env.get_tmp(rh_id).unwrap();

            match *vm.program().universe().get_type(lhs.type_id().unwrap()) {
                SmplType::Int => {
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

                SmplType::Float => {
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

                SmplType::Bool => {
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

            irmatch!(*vm.program().universe().get_type(t.type_id().unwrap());
                     SmplType::Float => {
                         let f = irmatch!(t_v; Value::Float(f) => f);
                         Value::Float(negate(f))
                     },

                     SmplType::Int => {
                         let i = irmatch!(t_v; Value::Int(i) => i);
                         Value::Int(negate(i))
                     },

                     SmplType::Bool => {
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
                        let field_value = context.top().func_env.get_tmp(tmp.data().clone()).unwrap();
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
                        Rc::new(RefCell::new(context.top().func_env.get_tmp(element_id).unwrap().clone()))
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
            let array = context.top().func_env.ref_tmp(indexing.array.data().clone()).unwrap();
            let array = array.borrow();
            let array = irmatch!(*array; Value::Array(ref v) => v);

            let indexer = context.top().func_env.get_tmp(indexing.indexer.data().clone()).unwrap();
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
    };

    TmpResult::Value(tmp_value)
}