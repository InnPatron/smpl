use std::cell::Cell;
use std::collections::HashMap;
use std::slice::Iter;

use semantic_ck::{Universe, FnId, TypeId, VarId, TmpId};
use typed_ast::*;
use ast::{FnCall as AstFnCall, Ident as AstIdent, Literal, UniOp, BinOp, Expr as AstExpr};

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
            scope.map_tmp(universe, Value::BinExpr(bin.op, 
                                                   Typed::untyped(lhs),
                                                   Typed::untyped(rhs)),
                                                   None)
        }

        AstExpr::Uni(uni) => {
            let expr = flatten_expr(universe, scope, *uni.expr);
            scope.map_tmp(universe, Value::UniExpr(uni.op,
                                                   Typed::untyped(expr)),
                                                   None)
        }

        AstExpr::Literal(literal) => {
            let lit_type = match literal {
                Literal::String(_) => Some(universe.string()),
                Literal::Number(ref num) => {
                    None
                },
                Literal::Bool(_) => Some(universe.boolean()),
            };

            scope.map_tmp(universe, Value::Literal(literal), lit_type)
        }

        AstExpr::Ident(ident) => scope.map_tmp(universe, Value::Variable(Variable::new(ident)), None),

        AstExpr::FnCall(fn_call) => {
            let name = fn_call.name;
            let args = fn_call.args.map(|vec| vec.into_iter().map(|e| Typed::untyped(flatten_expr(universe, scope, e))).collect::<Vec<_>>());

            let fn_call = FnCall::new(name, args);

            scope.map_tmp(universe, Value::FnCall(fn_call), None)
        }
    }
    
}

#[cfg(test)]
mod tests {
    use parser::*;
    use semantic_ck::*;
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
                    assert_eq!(*literal, Literal::Number("5".to_string()));
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected a literal number 5", v),
            }
        }

        // Find and validate tmp storing 2.
        let _2_id = order.next().unwrap();
        {
            match *expr.get_tmp(*_2_id).value().data() {
                Value::Literal(ref literal) => {
                    assert_eq!(*literal, Literal::Number("2".to_string()));
                }

                ref v @ _ => panic!("Unexpected value {:?}. Expected a literal number 3", v),
            }
        }

        // Find and validate tmp storing 3.
        let _3_id = order.next().unwrap();
        {
            match *expr.get_tmp(*_3_id).value().data() {
                Value::Literal(ref literal) => {
                    assert_eq!(*literal, Literal::Number("3".to_string()));
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
