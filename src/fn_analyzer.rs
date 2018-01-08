use std::collections::HashMap;

use ast;
use control_flow::*;
use typed_ast::*;
use err::Err;
use semantic_ck::{Universe, ScopedData, TypeId};
use smpl_type::*;

use petgraph::graph::NodeIndex;

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
                let name = var_decl.var_name().clone();
                let var_id = var_decl.var_id();
                let type_path = var_decl.type_path();

                let type_id = current_scope.type_id(type_path)?;

                current_scope.insert_var(name, var_id, type_id);

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

fn resolve_expr(universe: &Universe, scope: &ScopedData, expr: &Expr) -> Result<TypeId, Err> {
    let mut expr_type = None;

    for tmp_id in expr.execution_order() {
        let tmp = expr.get_tmp(*tmp_id);
        let tmp_type;
        match *tmp.value().data() {
            Value::Literal(ref literal) => {
                use ast::Literal;
                match *literal {
                    Literal::Number(ref num) => unimplemented!(),
                    Literal::String(_) => tmp_type = universe.string(),
                    Literal::Bool(_) => tmp_type = universe.boolean(),
                }
            },

            Value::Variable(ref var) => {
                let (var_id, type_id) = scope.var_info(var.ident())?;
                var.set_id(var_id);

                tmp_type = type_id;
            }

            Value::BinExpr(ref op, ref lhs, ref rhs) => {
                let lhs_type_id = expr.get_tmp(*lhs.data()).value().type_id().unwrap();
                let rhs_type_id = expr.get_tmp(*rhs.data()).value().type_id().unwrap();
                
                lhs.set_type_id(lhs_type_id);
                rhs.set_type_id(rhs_type_id);

                tmp_type = resolve_bin_op(universe, op, lhs_type_id, rhs_type_id)?;
            }

            Value::UniExpr(ref op, ref tmp) => {
                let tmp_type_id = expr.get_tmp(*tmp.data()).value().type_id().unwrap();

                tmp.set_type_id(tmp_type_id);

                tmp_type = resolve_uni_op(universe, op, tmp_type_id)?;
            }

            Value::FnCall(ref fn_call) => {
                let fn_id = scope.get_fn(&fn_call.name().clone().into())?;
                let func = universe.get_fn(fn_id);
                let fn_type_id = func.type_id();
                let fn_type = universe.get_type(fn_type_id);

                if let SmplType::Function(ref fn_type) = *fn_type {
                    let arg_types = fn_call.args()
                                           .map(|ref vec| {
                                               vec.iter().map(|ref tmp_id| {
                                                   let tmp = expr.get_tmp(*tmp_id.data());
                                                   let tmp_value = tmp.value();
                                                   let tmp_value_type_id = tmp_value.type_id().unwrap();
                                                   tmp_id.set_type_id(tmp_value_type_id);
                                                   universe.get_type(tmp_value_type_id)
                                               }).collect::<Vec<_>>()
                                           });

                    match arg_types {
                        Some(arg_types) => {
                            if fn_type.args.len() != arg_types.len() {
                                unimplemented!("Arity error: Arg length does not match fn definition.");
                            }

                            let fn_param_types = fn_type.args.iter().map(|id| universe.get_type(*id));

                            for (arg, param) in arg_types.iter().zip(fn_param_types) {
                                if (*arg != param) {
                                    unimplemented!("Arg does not match param type.");
                                }
                            }
                        }

                        None => {
                            if fn_type.args.len() != 0 {
                                unimplemented!("Arg lengths do not match");
                            }
                        }
                    }

                    tmp_type = fn_type.return_type;
                } else {
                    panic!("{} was mapped to {}, which is not SmplType::Function but {:?}", fn_id, fn_type_id, fn_type );
                }
            }
        }

        expr_type = Some(tmp_type);
    }

    Ok(expr_type.unwrap())
}

fn resolve_bin_op(universe: &Universe, op: &ast::BinOp, lhs: TypeId, rhs: TypeId) -> Result<TypeId, Err> {
    use ast::BinOp::*;

    let lh_type = universe.get_type(lhs);
    let rh_type = universe.get_type(rhs);

    match *op {
        Add | Sub | Mul | Div | Mod | GreaterEq | LesserEq | Greater | Lesser => {
            match (&*lh_type, &*rh_type) {
                (&SmplType::Int, &SmplType::Int) => Ok(universe.int()),
                (&SmplType::Float, &SmplType::Float) => Ok(universe.float()),
                _ => unimplemented!("Err"),
            }
        },

        LogicalAnd | LogicalOr => {
            match (&*lh_type, &*rh_type) {
                (&SmplType::Bool, &SmplType::Bool) => Ok(universe.boolean()),
                _ => unimplemented!("Err"),
            }
        }

        Eq | InEq => {
            if *lh_type == *rh_type {
                Ok(universe.boolean())
            } else {
                unimplemented!("Err")
            }
        }
    }
}

fn resolve_uni_op(universe: &Universe, op: &ast::UniOp, tmp_type_id: TypeId) -> Result<TypeId, Err> {
    use ast::UniOp::*;

    let tmp_type = universe.get_type(tmp_type_id);

    match *op {
        Negate => {
            match &*tmp_type {
                &SmplType::Int | &SmplType::Float => Ok(tmp_type_id),
                _ => unimplemented!("Err"),
            }
        },

        LogicalInvert => {
            match &*tmp_type {
                &SmplType::Bool => Ok(tmp_type_id),
                _ => unimplemented!("Err"),
            }
        }

        _ => unimplemented!(),
    }
}
