use std::collections::HashMap;

use crate::ast;
use crate::span::Span;

use super::type_cons::*;
use super::error::*;
use super::semantic_data::{TypeParamId, Universe };
use super::resolve_scope::ScopedData;

#[derive(Clone)]
pub struct TypingContext {
    map: HashMap<TypeParamId, AbstractType>,
}

pub fn resolve_types(universe: &Universe, scoped_data: &ScopedData, typing_context: TypingContext,
    synthesis: &AbstractType, constraint: &AbstractType, span: Span) 
    -> Result<(AbstractType, TypingContext), TypeError> {

    use super::type_cons::AbstractType::*;

    let synthesis = synthesis.apply(universe, scoped_data).unwrap();
    let constraint = constraint.apply(universe, scoped_data).unwrap();

    match (synthesis, constraint) {
        (AbstractType::Record {
            type_id: synth_type_id,
            abstract_field_map: synth_afm,
        }, AbstractType::Record {
            type_id: constraint_type_id,
            abstract_field_map: AbstractFieldMap {
                fields: constraint_fields,
                ..
            }, 
        }) => {
            unimplemented!()
        }

        (UncheckedFunction {
            return_type: ref synth_return,
        }, UncheckedFunction {
            return_type: ref constraint_return,
        }) => {
            resolve_types(universe, scoped_data, typing_context, 
                synth_return, constraint_return, span)
        }

        (AbstractType::App { .. }, _) | (_, AbstractType::App { .. }) => {
            unreachable!("No AbstractType::App after apply");
        }

        _ => unimplemented!(),
    }
}

pub fn resolve_bin_op(
    universe: &Universe,
    scoped_data: &ScopedData,
    typing_context: TypingContext,
    op: &ast::BinOp,
    lhs: AbstractType,
    rhs: AbstractType,
    span: Span,
) -> Result<(AbstractType, TypingContext), AnalysisError> {
    use crate::ast::BinOp::*;

    let expected_int = AbstractType::Int;

    let expected_float = AbstractType::Float;

    let expected_bool = AbstractType::Bool;

    // TODO: error handling
    let lhs = lhs.apply(universe, scoped_data)
        .expect("LHS BINOP TYPE ERROR");
    let rhs = rhs.apply(universe, scoped_data)
        .expect("LHS BINOP TYPE ERROR");

    let resolve_type = match *op {
        Add | Sub | Mul | Div | Mod => match (&lhs, &rhs) {
            (&AbstractType::Int, &AbstractType::Int) => AbstractType::Int,
            (&AbstractType::Float, &AbstractType::Float) => AbstractType::Float,

            _ => {
                return Err(TypeError::BinOp {
                    op: op.clone(),
                    expected: vec![expected_int, expected_float],
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    span: span,
                }
                .into());
            }
        },

        LogicalAnd | LogicalOr => match (&lhs, &rhs) {
            (&AbstractType::Bool, &AbstractType::Bool) => AbstractType::Bool,
            _ => {
                return Err(TypeError::BinOp {
                    op: op.clone(),
                    expected: vec![expected_bool],
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    span: span,
                }
                .into());
            }
        },

        GreaterEq | LesserEq | Greater | Lesser => match (&lhs, &rhs) {
            (&AbstractType::Int, &AbstractType::Int) => AbstractType::Bool,
            (&AbstractType::Float, &AbstractType::Float) => AbstractType::Bool,

            _ => {
                return Err(TypeError::BinOp {
                    op: op.clone(),
                    expected: vec![expected_int, expected_float],
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                    span: span,
                }
                .into());
            }
        },

        Eq | InEq => {
            // TODO: Better equality check
            /*
            if lhs != rhs {
                AbstractType::Bool
            } else {
                return Err(TypeError::LhsRhsInEq(lhs.clone(), rhs.clone(), span).into());
            }
            */
            unimplemented!()
        }
    };

    Ok((resolve_type, typing_context))
}

pub fn resolve_uni_op(
    universe: &Universe,
    scoped_data: &ScopedData,
    mut typing_context: TypingContext,
    op: &ast::UniOp,
    tmp_type: AbstractType,
    span: Span,
) -> Result<(AbstractType, TypingContext), AnalysisError> {
    use crate::ast::UniOp::*;

    let expected_int = AbstractType::Int;

    let expected_float = AbstractType::Float;

    let expected_bool = AbstractType::Bool;

    // TODO: error handling
    let tmp_type = tmp_type.apply(universe, scoped_data)
        .expect("UNI OP TYPE ERROR");

    match *op {
        Negate => match tmp_type {
            AbstractType::Int | AbstractType::Float => Ok((tmp_type.clone(), typing_context)),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_int, expected_float],
                expr: tmp_type.clone(),
                span: span,
            }
            .into()),
        },

        LogicalInvert => match tmp_type {
            AbstractType::Bool => Ok((tmp_type.clone(), typing_context)),
            _ => Err(TypeError::UniOp {
                op: op.clone(),
                expected: vec![expected_bool],
                expr: tmp_type.clone(),
                span: span,
            }
            .into()),
        },

        _ => unimplemented!(),
    }
}
