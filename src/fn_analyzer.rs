use std::collections::HashMap;
use std::slice::Iter;
use std::rc::Rc;

use ast;
use control_flow::*;
use typed_ast::*;
use err::Err;
use semantic_ck::{Universe, ScopedData, TypeId, FnId};
use smpl_type::*;

use petgraph::graph::NodeIndex;

struct FnAnalyzerData<'a, 'b> {
    universe: &'a Universe,
    cfg: &'b CFG,
    fn_return_type: Rc<SmplType>,
}

pub fn analyze_fn(universe: &Universe, global_scope: &ScopedData, cfg: &CFG, fn_id: FnId) -> Result<(), Err> {

    let fn_return_type;
    let func = universe.get_fn(fn_id);
    match *universe.get_type(func.type_id()) {
        SmplType::Function(ref fn_type) => {
            fn_return_type = universe.get_type(fn_type.return_type);
        }

        ref t @ _ => panic!("{} not mapped to a function but a {:?}", fn_id, t),
    }

    let analyzer_data = FnAnalyzerData {
        universe: universe,
        cfg: cfg,
        fn_return_type: fn_return_type
    };

    let mut scope_stack = Vec::new();
    let mut current_scope = global_scope.clone();

    let mut to_check = cfg.after_start().unwrap();

    loop {
        match visit_node(&analyzer_data, &mut current_scope, &mut scope_stack, to_check)? {
            Some(next) => to_check = next,
            None => break,
        }
    }

    if scope_stack.len() != 0 {
        panic!("Should be no left over scopes if CFG was generated properly and fully-walked.");
    }
    unimplemented!();
}

fn visit_node(data: &FnAnalyzerData, current_scope: &mut ScopedData, scope_stack: &mut Vec<ScopedData>, to_check: NodeIndex) -> Result<Option<NodeIndex>, Err> {
    match *node_w!(data.cfg, to_check) {
        Node::End => Ok(None),
        Node::Start | Node::BranchSplit | Node::BranchMerge | Node::LoopHead => {
            Ok(Some(data.cfg.next(to_check).unwrap()))
        }

        Node::LoopFoot => Ok(Some(data.cfg.after_loop_foot(to_check).unwrap())),
        Node::Continue => Ok(Some(data.cfg.after_continue(to_check).unwrap())),
        Node::Break => Ok(Some(data.cfg.after_break(to_check).unwrap())),

        Node::EnterScope => {
            scope_stack.push(current_scope.clone());
            Ok(Some(data.cfg.next(to_check).unwrap()))
        }
        Node::ExitScope => {
            *current_scope = scope_stack.pop().expect("If CFG was generated properly and the graph is being walked correctly, there should be a scope to pop");
            Ok(Some(data.cfg.next(to_check).unwrap()))
        }

        Node::LocalVarDecl(ref var_decl) => {
            let name = var_decl.var_name().clone();
            let var_id = var_decl.var_id();
            let var_type_path = var_decl.type_path();
            let var_type_id = current_scope.type_id(var_type_path)?;
            let var_type = data.universe.get_type(var_type_id);

            let expr_type_id = resolve_expr(data.universe, &current_scope, var_decl.init_expr())?;
            let expr_type = data.universe.get_type(expr_type_id);

            if var_type == expr_type {
                current_scope.insert_var(name, var_id, var_type_id);
            } else {
                unimplemented!("LHS type != RHS type");
            }

            Ok(Some(data.cfg.next(to_check).unwrap()))
        }

        Node::Assignment(ref assignment) => {
            let mut assignee = assignment.name().iter();
            let root_var_name = assignee.next().unwrap();

            let (root_var_id, root_var_type_id) = current_scope.var_info(root_var_name)?;

            let assignee_type_id = walk_field_access(data.universe, root_var_type_id, assignee)?;
            assignment.set_var_id(root_var_id);
            assignment.set_type_id(assignee_type_id);

            let expr_type_id = resolve_expr(data.universe, &current_scope, assignment.value())?;

            let assignee_type = data.universe.get_type(assignee_type_id);
            let expr_type = data.universe.get_type(expr_type_id);

            if assignee_type != expr_type {
                unimplemented!("LHS Type != RGHS Type");
            }

            Ok(Some(data.cfg.next(to_check).unwrap()))
        },

        Node::Expr(ref expr) => {
            resolve_expr(data.universe, &current_scope, expr)?;
            Ok(Some(data.cfg.next(to_check).unwrap()))            
        }

        Node::Return(ref return_expr) => {
            let expr_type_id = match return_expr.as_ref() {
                Some(ref expr) => resolve_expr(data.universe, &current_scope, expr)?,

                None => data.universe.unit(),
            };

            if data.universe.get_type(expr_type_id) != data.fn_return_type {
                unimplemented!("Return expr type and Fn Return types do not match");
            }

            Ok(Some(data.cfg.next(to_check).unwrap()))
        }

        Node::Condition(ref condition_expr) => {
            let expr_type_id = resolve_expr(data.universe, current_scope, condition_expr)?;

            if *data.universe.get_type(expr_type_id) != SmplType::Bool {
                unimplemented!("Expr to a condition must be of type Bool.");
            }

            let mut merge_node = None;
            let (true_branch_head, false_branch_head) = data.cfg.after_condition(to_check).unwrap();

            let mut current_true_node = true_branch_head;
            let mut current_false_node = false_branch_head;
            let mut true_merged = false;
            let mut false_merged = false;

            // Go through all the nodes in each branch until each branch hits the
            // Node::BranchMerge.
            //
            // Can continue going through the CFG linearly afterwords.
            loop {
                if true_merged == false {
                    match *node_w!(data.cfg, current_true_node) {
                        Node::BranchMerge => {
                            merge_node = Some(current_true_node);
                            true_merged = true;
                        }

                        _ => (),
                    }
                    match visit_node(data, current_scope, scope_stack, current_true_node)? {
                        Some(next) => current_true_node = next,
                        None => return Ok(None),
                    }
                }

                if false_merged == false {
                    match *node_w!(data.cfg, current_false_node) {
                        Node::BranchMerge => {
                            merge_node = Some(current_false_node);
                            false_merged = true;
                        }

                        _ => (),
                    }
                    match visit_node(data, current_scope, scope_stack, current_false_node)? {
                        Some(next) => current_false_node = next,
                        None => return Ok(None),
                    }
                }

                if true_merged && false_merged {
                    break;
                }
            }

            let merge_node = merge_node.unwrap();
            Ok(Some(merge_node))
        }
    }
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

fn walk_field_access(universe: &Universe, root_type_id: TypeId, path: Iter<ast::Ident>) -> Result<TypeId, Err> {
    let mut current_type_id = root_type_id;
    let mut current_type = universe.get_type(root_type_id);
    for field in path {
        match *current_type {
            SmplType::Struct(ref struct_type) => {
                current_type_id = *struct_type.fields.get(field).ok_or(Err::UnknownField(field.clone()))?;
             }

            _ => unimplemented!("Unexpected field access on non-struct type"),
        }

        current_type = universe.get_type(current_type_id);
    }

    Ok(current_type_id)
}
