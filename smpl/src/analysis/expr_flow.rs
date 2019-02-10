use super::semantic_data::{TmpId, Universe};
use super::typed_ast::*;
use super::typed_ast::Binding as TypedBinding;

use crate::span::Span;

use crate::ast::{ArrayInit as AstArrayInit, Expr as AstExpr, AstNode, TypedPath};

pub fn flatten(universe: &Universe, e: AstExpr) -> Expr {
    let mut expr = Expr::new();

    let (_, span) = flatten_expr(universe, &mut expr, e);
    expr.set_span(span);

    expr
}

pub fn flatten_expr(universe: &Universe, scope: &mut Expr, e: AstExpr) -> (TmpId, Span) {
    match e {
        AstExpr::Bin(bin) => {
            let (bin, span) = bin.to_data();
            let (lhs, _) = flatten_expr(universe, scope, *bin.lhs);
            let (rhs, _) = flatten_expr(universe, scope, *bin.rhs);
            (
                scope.map_tmp(
                    universe,
                    Value::BinExpr(bin.op, Typed::untyped(lhs), Typed::untyped(rhs)),
                    span,
                ),
                span,
            )
        }

        AstExpr::Uni(uni) => {
            let (uni, span) = uni.to_data();
            let expr = flatten_expr(universe, scope, *uni.expr).0;
            (
                scope.map_tmp(universe, Value::UniExpr(uni.op, Typed::untyped(expr)), span),
                span,
            )
        }

        AstExpr::Literal(literal) => {
            let (literal, span) = literal.to_data();
            (scope.map_tmp(universe, Value::Literal(literal), span), span)
        }

        AstExpr::StructInit(init) => {
            let (init, span) = init.to_data();
            let struct_name = init.struct_name;
            let field_init = init.field_init.map(|field_init_list| {
                field_init_list
                    .into_iter()
                    .map(|(name, expr)| {
                        let expr = Typed::untyped(flatten_expr(universe, scope, *expr).0);
                        (name.data().clone(), expr)
                    })
                    .collect::<Vec<_>>()
            });
            (
                scope.map_tmp(
                    universe,
                    Value::StructInit(StructInit::new(struct_name, field_init)),
                    span,
                ),
                span,
            )
        }

        AstExpr::Binding(ident) => {
            let span = ident.span();
            (
                scope.map_tmp(universe, Value::Binding(TypedBinding::new(ident)), span),
                span,
            )
        }

        AstExpr::FieldAccess(path) => {
            let (path, span) = path.to_data();
            let field_access = FieldAccess::new(universe, scope, path);
            (
                scope.map_tmp(
                    universe,
                    Value::FieldAccess(field_access),
                    span,
                ),
                span,
            )
        }

        AstExpr::FnCall(fn_call) => {
            let (fn_call, span) = fn_call.to_data();
            let path = fn_call.path;
            let (fn_val, fn_val_span) = flatten_expr(universe, scope, AstExpr::Path(path));
            let args = fn_call.args.map(|vec| {
                vec.into_iter()
                    .map(|e| Typed::untyped(flatten_expr(universe, scope, e).0))
                    .collect::<Vec<_>>()
            });

            let fn_call = FnCall::new(fn_val, args);

            (scope.map_tmp(universe, Value::FnCall(fn_call), span), span)
        }

        AstExpr::ArrayInit(init) => {
            let (init, span) = init.to_data();
            match init {
                AstArrayInit::InitList(vec) => {
                    let list = vec.into_iter()
                        .map(|element| Typed::untyped(flatten_expr(universe, scope, element).0))
                        .collect();

                    let init = ArrayInit::List(list);

                    (scope.map_tmp(universe, Value::ArrayInit(init), span), span)
                }
                AstArrayInit::Value(expr, size) => {
                    let value = Typed::untyped(flatten_expr(universe, scope, *expr).0);
                    let init = ArrayInit::Value(value, size);

                    (scope.map_tmp(universe, Value::ArrayInit(init), span), span)
                }
            }
        }

        AstExpr::Indexing(indexing) => {
            let (indexing, span) = indexing.to_data();
            let array_expr = indexing.array;
            let indexing_expr = indexing.indexer;

            let array = Typed::untyped(flatten_expr(universe, scope, *array_expr).0);
            let indexer = Typed::untyped(flatten_expr(universe, scope, *indexing_expr).0);

            let indexing = Indexing {
                array: array,
                indexer: indexer,
            };

            (
                scope.map_tmp(universe, Value::Indexing(indexing), span),
                span,
            )
        }

        AstExpr::Path(path) => {
            let (path, span) = path.to_data();
            let tmp = match path {
                TypedPath::NillArity(path) => 
                    Value::ModAccess(ModAccess::new(path)),
                TypedPath::Parameterized(path, args) => 
                    Value::TypeInst(TypeInst::new(path, args)),
            };

            (
                scope.map_tmp(universe, tmp, span),
                span
            )
        }

        AstExpr::AnonymousFn(a_fn) => {
            let (a_fn, span) = a_fn.to_data();
            (
                scope.map_tmp(universe, Value::AnonymousFn(AnonymousFn::new(a_fn)), span),
                span,
            )
        }

        AstExpr::FnCallChain(chain) => {
            let (chain, _span) = chain.to_data();

            let (base, span) = chain.base.to_data();
            let base_expr = AstExpr::FnCall(AstNode::new(base, span));

            let (base_result, span) = flatten_expr(universe, scope, base_expr);

            let mut previous_result = base_result;
            let mut span = span;

            for fn_call in chain.chain.into_iter() {
                let (call, next_span) = fn_call.to_data();
                let fn_call_expr = AstExpr::FnCall(AstNode::new(call, next_span));

                let (fn_call_result, next_span) = flatten_expr(universe, scope, fn_call_expr);

                let fn_call = scope.get_tmp_mut(fn_call_result);

                let fn_call = irmatch!(fn_call.value_mut().data_mut(); 
                                       Value::FnCall(ref mut call) => call);

                // Use the result of the function call as the first argument
                // of the next function.
                let first_arg = Typed::untyped(previous_result);

                let args = fn_call.args_mut();

                match args {
                    Some(ref mut vec) => vec.insert(0, first_arg),

                    None => *args = Some(vec![first_arg]),
                }

                previous_result = fn_call_result;
                span = next_span;
            }

            (previous_result, span)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use crate::parser::expr_parser::*;
    use super::super::semantic_data::*;
    use super::*;

    #[test]
    fn expr_exec_order_ck() {
        let input = "5 + 2 / 3";
        let mut input = buffer_input(input);
        let expr = piped_expr(&mut input, &[]).unwrap().to_data().0;

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
