use super::semantic_data::{TmpId, Universe};
use super::typed_ast::*;
use super::typed_ast::Binding as TypedBinding;
use ast::{Expr as AstExpr, Literal, ArrayInit as AstArrayInit};

pub fn flatten(universe: &Universe, e: AstExpr) -> Expr {
    let mut expr = Expr::new();

    flatten_expr(universe, &mut expr, e);

    expr
}

pub fn flatten_expr(universe: &Universe, scope: &mut Expr, e: AstExpr) -> TmpId {
    match e {
        AstExpr::Bin(bin) => {
            let lhs = flatten_expr(universe, scope, *bin.lhs);
            let rhs = flatten_expr(universe, scope, *bin.rhs);
            scope.map_tmp(
                universe,
                Value::BinExpr(bin.op, Typed::untyped(lhs), Typed::untyped(rhs)),
            )
        }

        AstExpr::Uni(uni) => {
            let expr = flatten_expr(universe, scope, *uni.expr);
            scope.map_tmp(universe, Value::UniExpr(uni.op, Typed::untyped(expr)))
        }

        AstExpr::Literal(literal) => {
            scope.map_tmp(universe, Value::Literal(literal))
        }

        AstExpr::StructInit(init) => {
            let struct_name = init.struct_name;
            let field_init = init.field_init.map(|field_init_list| {
                field_init_list
                    .into_iter()
                    .map(|(name, expr)| {
                        let expr = Typed::untyped(flatten_expr(universe, scope, *expr));
                        (name, expr)
                    })
                    .collect::<Vec<_>>()
            });
            scope.map_tmp(
                universe,
                Value::StructInit(StructInit::new(struct_name, field_init)),
            )
        }

        AstExpr::Binding(ident) => {
            scope.map_tmp(universe, Value::Binding(TypedBinding::new(ident)))
        }

        AstExpr::FieldAccess(path) => {
            scope.map_tmp(universe, Value::FieldAccess(FieldAccess::new(universe, path)))
        }

        AstExpr::FnCall(fn_call) => {
            let path = fn_call.path;
            let args = fn_call.args.map(|vec| {
                vec.into_iter()
                    .map(|e| Typed::untyped(flatten_expr(universe, scope, e)))
                    .collect::<Vec<_>>()
            });

            let fn_call = FnCall::new(path, args);

            scope.map_tmp(universe, Value::FnCall(fn_call))
        }

        AstExpr::ArrayInit(init) => {
            match init {
                AstArrayInit::InitList(vec) => {
                    let list = vec.into_iter()
                        .map(|element| {
                            Typed::untyped(flatten_expr(universe, scope, element))
                        })
                    .collect();

                    let init = ArrayInit::List(list);

                    scope.map_tmp(universe, Value::ArrayInit(init))
                }
                AstArrayInit::Value(expr, size) => {
                    let value = Typed::untyped(flatten_expr(universe, scope, *expr));
                    let init = ArrayInit::Value(value, size);

                    scope.map_tmp(universe, Value::ArrayInit(init))
                }
            }
        }

        AstExpr::Indexing(indexing) => {
            let array_expr = indexing.array;
            let indexing_expr = indexing.indexer;

            let array = Typed::untyped(flatten_expr(universe, scope, *array_expr));
            let indexer = Typed::untyped(flatten_expr(universe, scope, *indexing_expr));

            let indexing = Indexing {
                array: array,
                indexer: indexer,
            };

            scope.map_tmp(universe, Value::Indexing(indexing))
        }

        AstExpr::ModAccess(path) => {
            scope.map_tmp(universe, Value::ModAccess(ModAccess::new(path)))
        }
    }
}

#[cfg(test)]
mod tests {
    use parser::*;
    use super::super::semantic_data::*;
    use super::*;

    #[test]
    fn expr_exec_order_ck() {
        let input = "5 + 2 / 3";
        let expr = parse_Expr(input).unwrap();

        let universe = Universe::std();

        let expr = flatten(&universe, expr);

        let mut order = expr.execution_order();

        // Find and validate tmp storing 5.
        let _5_id = order.next().unwrap();
        {
            match *expr.get_tmp(*_5_id).value().data() {
                Value::Literal(ref literal) => {
                    assert_eq!(*literal, Literal::Int(5));
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected a literal number 5", v),
            }
        }

        // Find and validate tmp storing 2.
        let _2_id = order.next().unwrap();
        {
            match *expr.get_tmp(*_2_id).value().data() {
                Value::Literal(ref literal) => {
                    assert_eq!(*literal, Literal::Int(2));
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected a literal number 3", v),
            }
        }

        // Find and validate tmp storing 3.
        let _3_id = order.next().unwrap();
        {
            match *expr.get_tmp(*_3_id).value().data() {
                Value::Literal(ref literal) => {
                    assert_eq!(*literal, Literal::Int(3));
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected a literal number 3", v),
            }
        }

        let div_id = order.next().unwrap();
        {
            let (l_id, r_id) = match *expr.get_tmp(*div_id).value().data() {
                Value::BinExpr(ref op, ref lhs, ref rhs) => {
                    assert_eq!(*op, BinOp::Div);
                    (lhs.data(), rhs.data())
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected a division expr", v),
            };

            assert_eq!(l_id, _2_id);
            assert_eq!(r_id, _3_id);
        }

        let add_id = order.next().unwrap();
        {
            let (l_id, r_id) = match *expr.get_tmp(*add_id).value().data() {
                Value::BinExpr(ref op, ref lhs, ref rhs) => {
                    assert_eq!(*op, BinOp::Add);
                    (lhs.data(), rhs.data())
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected an addition expr", v),
            };

            assert_eq!(l_id, _5_id);
            assert_eq!(r_id, div_id);
        }
    }
}
