use std::rc::Rc;

use ast;
use linear_cfg_traversal::*;
use control_flow::CFG;
use typed_ast::*;
use err::*;
use semantic_ck::{Universe, ScopedData, TypeId, FnId};
use smpl_type::*;

use petgraph::graph::NodeIndex;

struct FnAnalyzer<'a> {
    universe: &'a Universe,
    fn_return_type: Rc<SmplType>,
    fn_return_type_id: TypeId,
    current_scope: ScopedData,
    scope_stack: Vec<ScopedData>,
}

pub fn analyze_fn(universe: &Universe, global_scope: &ScopedData, cfg: &CFG, fn_id: FnId) -> Result<(), Err> {
    let fn_return_type;
    let fn_return_type_id;
    let func = universe.get_fn(fn_id);
    let unknown_type = universe.get_type(func.type_id());
    let func_type;
    match *unknown_type {
        SmplType::Function(ref fn_type) => {
            fn_return_type = universe.get_type(fn_type.return_type.clone());
            fn_return_type_id = fn_type.return_type;
            func_type = fn_type;
        }

        ref t @ _ => panic!("{} not mapped to a function but a {:?}", fn_id, t),
    }

    let mut analyzer = FnAnalyzer {
        universe: universe,
        fn_return_type: fn_return_type,
        fn_return_type_id: fn_return_type_id,
        current_scope: global_scope.clone(),
        scope_stack: Vec::new(),
    };

    // Add parameters to the current scope.
    for param in func_type.params.iter() {
        let var_id = universe.new_var_id();
        analyzer.current_scope.insert_var(param.name.clone(), 
                                          var_id, 
                                          param.param_type);
        param.set_var_id(var_id);
    }

    let traverser = Traverser::new(cfg, &mut analyzer);

    traverser.traverse()
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
                    Literal::Int(_) => tmp_type = universe.int(),
                    Literal::Float(_) => tmp_type = universe.float(),
                    Literal::String(_) => tmp_type = universe.string(),
                    Literal::Bool(_) => tmp_type = universe.boolean(),
                }
            },

            Value::StructInit(ref init) => {

                // Get type info
                let type_name = init.type_name();
                let unknown_type_id = scope.type_id(&type_name)?;
                let unknown_type = universe.get_type(unknown_type_id);

                // Check if type is a struct.
                let struct_type_id = unknown_type_id;
                let struct_type;
                match *unknown_type {
                    SmplType::Struct(ref t) => struct_type = t,
                    _ => {
                        return Err(TypeErr::NotAStruct {
                            type_name: type_name.clone(),
                            found: struct_type_id,
                        }.into());
                    }
                }

                // Check struct field initialization expressions.
                match init.field_init() {

                    // Found field initializations
                    Some(init_list) => {
                        if init_list.len() != struct_type.fields.len() {
                            // Missing fields -> struct is not fully initialized
                            return Err(TypeErr::StructNotFullyInitialized {
                                type_name: type_name.clone(),
                                struct_type: struct_type_id,
                                missing_fields: {
                                    let inits = init_list.iter().map(|&(ref name, ref id)| name.clone())
                                                         .collect::<Vec<_>>();
                                    
                                    struct_type.fields.keys()
                                               .cloned().filter(|ident| !inits.contains(ident))
                                               .collect::<Vec<_>>()
                                }
                            }.into());
                        }
                        
                        // Go threw initialization list and check expressions 
                        for &(ref ident, ref typed_tmp_id) in init_list {
                            match struct_type.fields.get(ident) {
                                
                                // Field being checked exists in the struct type
                                Some(field_type_id) => {
                                    let tmp = expr.get_tmp(*typed_tmp_id.data());
                                    let tmp_type_id = tmp.value().type_id().unwrap();
                                    typed_tmp_id.set_type_id(tmp_type_id);

                                    // Expression type the same as the field type?
                                    if (universe.get_type(tmp_type_id) != universe.get_type(*field_type_id)) {
                                        return Err(TypeErr::UnexpectedType {
                                            found: tmp_type_id,
                                            expected: *field_type_id,
                                        }.into());
                                    }
                                }

                                // Field being checked does not exist in the struct type
                                None => {
                                    return Err(TypeErr::UnknownField {
                                        name: ident.clone(),
                                        struct_type: struct_type_id,
                                    }.into());
                                }
                            }
                        }
                    }

                    // No field initializations
                    None => {
                        if struct_type.fields.len() != 0 {
                            // No field initializations but the struct type has fields
                            return Err(TypeErr::StructNotFullyInitialized {
                                type_name: type_name.clone(),
                                struct_type: struct_type_id,
                                missing_fields: struct_type.fields.keys().cloned().collect::<Vec<_>>(),
                            }.into());
                        }
                    }
                }

                tmp_type = struct_type_id;
            },

            Value::Variable(ref var) => {
                let (var_id, type_id) = scope.var_info(var.ident())?;
                var.set_id(var_id);

                tmp_type = type_id;
            }

            Value::FieldAccess(ref field_access) => {
                let root_var_name = field_access.path().iter().next().unwrap();
                let (root_var_id, root_var_type_id) = scope.var_info(root_var_name)?;

                let accessed_field_type_id = walk_field_access(universe, root_var_type_id, field_access.path().clone())?;

                field_access.set_field_type_id(accessed_field_type_id);
                field_access.set_root_var_id(root_var_id);

                tmp_type = accessed_field_type_id;
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

                fn_call.set_id(fn_id);

                if let SmplType::Function(ref fn_type) = *fn_type {
                    let arg_type_ids = fn_call.args()
                                           .map(|ref vec| {
                                               vec.iter().map(|ref tmp_id| {
                                                   let tmp = expr.get_tmp(*tmp_id.data());
                                                   let tmp_value = tmp.value();
                                                   let tmp_value_type_id = tmp_value.type_id().unwrap();
                                                   tmp_id.set_type_id(tmp_value_type_id);
                                                   tmp_value_type_id
                                               }).collect::<Vec<_>>()
                                           });

                    match arg_type_ids {
                        Some(arg_type_ids) => {
                            if fn_type.params.len() != arg_type_ids.len() {
                                return Err(TypeErr::Arity {
                                    fn_type: fn_type_id,
                                    found_args: arg_type_ids.len(),
                                    expected_param: fn_type.params.len(),

                                }.into());
                            }

                            let fn_param_type_ids = fn_type.params.iter();

                            for (index, (arg, param)) in arg_type_ids.iter().zip(fn_param_type_ids).enumerate() {
                                let arg_type = universe.get_type(*arg);
                                let param_type = universe.get_type(param.param_type);
                                if arg_type != param_type {
                                    return Err(TypeErr::ArgMismatch {
                                        fn_id: fn_id,
                                        index: index,
                                        arg: *arg,
                                        param: param.param_type,
                                    }.into());
                                }
                            }
                        }

                        None => {
                            if fn_type.params.len() != 0 {
                                return Err(TypeErr::Arity {
                                    fn_type: fn_type_id,
                                    found_args: 0,
                                    expected_param: fn_type.params.len(),
                                }.into());
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

                _ => {
                    Err(TypeErr::BinOp {
                        op: op.clone(),
                        expected: vec![universe.int(), universe.float()],
                        lhs: lhs,
                        rhs: rhs,
                    }.into())
                }
            }
        },

        LogicalAnd | LogicalOr => {
            match (&*lh_type, &*rh_type) {
                (&SmplType::Bool, &SmplType::Bool) => Ok(universe.boolean()),
                _ => {
                    Err(TypeErr::BinOp {
                        op: op.clone(),
                        expected: vec![universe.boolean()],
                        lhs: lhs,
                        rhs: rhs,
                    }.into())
                }
            }
        }

        Eq | InEq => {
            if *lh_type == *rh_type {
                Ok(universe.boolean())
            } else {
                Err(TypeErr::LhsRhsInEq(lhs, rhs).into())
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
                _ => {
                    Err(TypeErr::UniOp {
                        op: op.clone(),
                        expected: vec![universe.int(), universe.float()],
                        expr: tmp_type_id,
                    }.into())
                }
            }
        },

        LogicalInvert => {
            match &*tmp_type {
                &SmplType::Bool => Ok(tmp_type_id),
                _ => {
                    Err(TypeErr::UniOp {
                        op: op.clone(),
                        expected: vec![universe.boolean()],
                        expr: tmp_type_id,
                    }.into())
                },
            }
        }

        _ => unimplemented!(),
    }
}

impl<'a> Passenger<Err> for FnAnalyzer<'a> {
    fn start(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn end(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn branch_merge(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn loop_head(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn loop_foot(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn cont(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn br(&mut self, id: NodeIndex) -> Result<(), Err> {
        Ok(())
    }

    fn enter_scope(&mut self, id: NodeIndex) -> Result<(), Err> {
        self.scope_stack.push(self.current_scope.clone());
        Ok(())
    }

    fn exit_scope(&mut self, id: NodeIndex) -> Result<(), Err> {
        let popped = self.scope_stack.pop()
                                     .expect("If CFG was generated properly and the graph is being walked correctly, there should be a scope to pop");
        self.current_scope = popped;
        Ok(())
    }

    fn local_var_decl(&mut self, id: NodeIndex, var_decl: &LocalVarDecl) -> Result<(), Err> {
        let name = var_decl.var_name().clone();
        let var_id = var_decl.var_id();
        let var_type_path = var_decl.type_path();
        let var_type_id = self.current_scope.type_id(var_type_path)?;
        let var_type = self.universe.get_type(var_type_id);

        let expr_type_id = resolve_expr(self.universe, 
                                        &self.current_scope, 
                                        var_decl.init_expr())?;
        let expr_type = self.universe.get_type(expr_type_id);

        var_decl.set_type_id(var_type_id);

        if var_type == expr_type {
            self.current_scope.insert_var(name, var_id, var_type_id);
        } else {
            return Err(TypeErr::LhsRhsInEq(var_type_id, expr_type_id).into());
        }

        Ok(())
    }

    fn assignment(&mut self, id: NodeIndex, assignment: &Assignment) -> Result<(), Err> {
        let mut assignee = assignment.name().iter();
        let root_var_name = assignee.next().unwrap();

        let (root_var_id, root_var_type_id) = self.current_scope.var_info(root_var_name)?;

        let assignee_type_id = walk_field_access(self.universe, 
                                                 root_var_type_id, 
                                                 assignment.name().clone())?;
        assignment.set_var_id(root_var_id);
        assignment.set_type_id(assignee_type_id);

        let expr_type_id = resolve_expr(self.universe, 
                                        &self.current_scope, 
                                        assignment.value())?;

        let assignee_type = self.universe.get_type(assignee_type_id);
        let expr_type = self.universe.get_type(expr_type_id);

        if assignee_type != expr_type {
            return Err(TypeErr::LhsRhsInEq(assignee_type_id, expr_type_id).into());
        }

        Ok(())
    }


    fn expr(&mut self, id: NodeIndex, expr: &Expr) -> Result<(), Err> {
        resolve_expr(self.universe, &self.current_scope, expr).map(|_| ())
    }

    fn ret(&mut self, id: NodeIndex, expr: Option<&Expr>) -> Result<(), Err> {
        let expr_type_id = match expr {
            Some(ref expr) => resolve_expr(self.universe, &self.current_scope, expr)?,

            None => self.universe.unit(),
        };

        if self.universe.get_type(expr_type_id) != self.fn_return_type {
            return Err(TypeErr::InEqFnReturn {
                expr: expr_type_id,
                fn_return: self.fn_return_type_id,
            }.into());
        }

        Ok(())
    }

    fn loop_condition(&mut self, id: NodeIndex, condition: &Expr) -> Result<(), Err> {
        let expr_type_id = resolve_expr(self.universe, &self.current_scope, condition)?;

        if *self.universe.get_type(expr_type_id) != SmplType::Bool {
            return Err(TypeErr::UnexpectedType {
                found: expr_type_id,
                expected: self.universe.boolean(),
            }.into());
        }

        Ok(())
    }

    fn loop_start_true_path(&mut self, id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn loop_end_true_path(&mut self, id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_condition(&mut self, id: NodeIndex, condition: &Expr) -> Result<(), Err> {
        let expr_type_id = resolve_expr(self.universe, &self.current_scope, condition)?;

        if *self.universe.get_type(expr_type_id) != SmplType::Bool {
            return Err(TypeErr::UnexpectedType {
                found: expr_type_id,
                expected: self.universe.boolean(),
            }.into());
        }

        Ok(())
    }

    fn branch_start_true_path(&mut self, id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_start_false_path(&mut self, id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_end_true_path(&mut self, id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }

    fn branch_end_false_path(&mut self, id: NodeIndex) -> Result<(), Err> {
        // Do nothing
        Ok(())
    }
}

fn walk_field_access(universe: &Universe, root_type_id: TypeId, full_path: ast::Path) -> Result<TypeId, Err> {
    let mut current_type_id = root_type_id;
    let mut current_type = universe.get_type(root_type_id);

    let mut path_iter = full_path.iter();
    path_iter.next();

    for (index, field) in path_iter.enumerate() {
        match *current_type {
            SmplType::Struct(ref struct_type) => {
                current_type_id = *struct_type.fields
                                              .get(field)
                                              .ok_or(TypeErr::UnknownField {
                                                  name: field.clone(),
                                                  struct_type: current_type_id,
                                              })?;
             }

            _ => {
                return Err(TypeErr::FieldAccessOnNonStruct {
                    path: full_path.clone(),
                    index: index,
                    invalid_type: current_type_id,
                    root_type: root_type_id,
                }.into());
            },
        }

        current_type = universe.get_type(current_type_id);
    }

    Ok(current_type_id)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;
    use ast::*;
    use semantic_ck::*;
    use ascii::*;
    use std::str::FromStr;

    #[test]
    fn test_walk_field_access() {
        let mut universe = Universe::std();

        let fields_1 = vec![(ident!("field1"), universe.int()),
                        (ident!("field2"), universe.boolean())]

                        .into_iter().collect::<HashMap<_,_>>();
        let struct_type_1 = StructType {
            name: ident!("foo"),
            fields: fields_1,
        };
        let struct_type_1_id = universe.new_type_id();
        universe.insert_type(struct_type_1_id, SmplType::Struct(struct_type_1));


        let fields_2 = vec![(ident!("foo_field"), struct_type_1_id)]

                           .into_iter().collect::<HashMap<_,_>>();

        let struct_type_2 = StructType {
            name: ident!("bar"),
            fields: fields_2,
        };
        let struct_type_2_id = universe.new_type_id();
        universe.insert_type(struct_type_2_id, SmplType::Struct(struct_type_2));

        let mut scope = universe.std_scope();
        let root_var_id = universe.new_var_id();
        scope.insert_var(ident!("baz"), root_var_id, struct_type_2_id);

        let path = path!("root (ignored", "foo_field", "field1");
        let field_type_id = walk_field_access(&universe, struct_type_2_id, path).unwrap();

        assert_eq!(field_type_id, universe.int());
    }
}
