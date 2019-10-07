use std::collections::HashMap;

use crate::ast;
use crate::span::Span;

use super::type_cons::*;
use super::error::*;
use super::semantic_data::{TypeVarId, Universe };
use super::resolve_scope::ScopedData;
use super::type_checker::TypingContext;

/// May or may not alter the typing context for inference purposes
pub fn resolve_types(universe: &Universe, scoped_data: &ScopedData, 
    typing_context: &mut TypingContext, synthesis: &AbstractType, 
    constraint: &AbstractType, span: Span) 
    -> Result<(), TypeError> {

    resolve_types_static(universe, 
        scoped_data, 
        typing_context, 
        synthesis, 
        constraint, 
        span)
}

/// Will NOT perform any inferences
pub fn resolve_types_static(universe: &Universe, scoped_data: &ScopedData, 
    typing_context: &TypingContext, synthesis: &AbstractType, 
    constraint: &AbstractType, span: Span) 
    -> Result<(), TypeError> {

    use super::type_cons::AbstractType::*;

    let new_synthesis = synthesis.apply(universe, scoped_data, typing_context).unwrap();
    let new_constraint = constraint.apply(universe, scoped_data, typing_context).unwrap();

    match (new_synthesis, new_constraint) {

        (_, Any) => Ok(()),

        (Record {
            type_id: synth_type_id,
            abstract_field_map: AbstractFieldMap {
                fields: synth_fields,
                ..
            }
        }, Record {
            type_id: constraint_type_id,
            abstract_field_map: AbstractFieldMap {
                fields: constraint_fields,
                ..
            }, 
        }) => {

            // Nominal check
            if synth_type_id != constraint_type_id {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }.into());
            }

            for (synth_field_id, synth_type) in synth_fields.iter() {
                let constraint_type = constraint_fields
                    .get(synth_field_id)
                    .expect("Equal type id means identical field IDs");

                resolve_types_static(universe, scoped_data, typing_context,
                    synth_type, constraint_type, span)?;
            }

            Ok(())
        }

        // Synth width must be wider than constraint width
        (WidthConstraint(ref synth_awc), 
         WidthConstraint(ref constraint_awc)) => {

            for (constraint_ident, constraint_type) in constraint_awc.fields.iter() {
                match synth_awc.fields.get(constraint_ident) {
                    Some(synth_type) => {
                        resolve_types_static(universe, scoped_data, typing_context,
                            synth_type, constraint_type, span)?;
                    }

                    None => {
                        // Synth width constraint is not wider than constraint width
                        return Err(TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into());
                    }
                }
            }

            Ok(())
        }

        // Synth nominal record must be wider than constraint width
        (Record {
            abstract_field_map: ref synth_afm,
            ..
        }, WidthConstraint(ref constraint_awc)) => {

            for (constraint_ident, constraint_type) in constraint_awc.fields.iter() {
                match synth_afm.get(constraint_ident) {
                    Some(synth_type) => {
                        resolve_types_static(universe, scoped_data, typing_context,
                            synth_type, constraint_type, span)?;
                    }

                    None => {
                        // TODO: Missing field
                        // Synth nominal record is not wider than constraint width
                        return Err(TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into());
                    }
                }
            }

            Ok(())
        }

        (UncheckedFunction {
            return_type: ref synth_return,
        }, UncheckedFunction {
            return_type: ref constraint_return,
        }) => {
            resolve_types_static(universe, scoped_data, typing_context, 
                synth_return, constraint_return, span)
        }

        (Function {
            parameters: ref synth_params,
            return_type: ref synth_return,
        }, Function {
            parameters: ref constraint_params,
            return_type: ref constraint_return,
        }) => {

            if synth_params.len() != constraint_params.len() {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }.into());
            }

            // Function parameters must be contravariant
            for (sp, cp) in synth_params.iter().zip(constraint_params) {
                resolve_param_static(universe, scoped_data, typing_context,
                    sp, cp, span)?;
            }

            resolve_types_static(universe, scoped_data, typing_context,
                synth_return, constraint_return, span)
        }

        (Array {
            element_type: ref synth_element,
            size: synth_size,
        }, Array {
            element_type: ref constraint_element,
            size: constraint_size,
        }) => {

            // TODO: Allow synth to be larger?
            if synth_size != constraint_size {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }.into());
            }

            resolve_types_static(universe, scoped_data, typing_context,
                synth_element, constraint_element, span)
        }

        (App { .. }, _) | (_, App { .. }) => {
            unreachable!("No AbstractType::App after apply");
        }

        (Int, Int) => Ok(()),
        (Float, Float) => Ok(()),
        (Bool, Bool) => Ok(()),
        (String, String) => Ok(()),
        (Unit, Unit) => Ok(()), 

        // Unconstrained type parameters
        // Check if type parameters are equal
        (TypeVar(synth_id), TypeVar(constraint_id)) => {
            if synth_id == constraint_id {
                Ok(())
            } else {

                let synth_type = typing_context.get_type_var(synth_id)
                    .expect("Missing synth var");
                let constraint_type = typing_context.get_type_var(constraint_id)
                    .expect("Missing synth var");

                resolve_types_static(universe, scoped_data, typing_context,
                    synth_type, constraint_type, span)
                    .map_err(|_| {
                        TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into()
                    })
            }
        }

        (TypeVar(synth_id), _) => {
            let synth_var_type = typing_context.get_type_var(synth_id)
                    .expect("Missing synth var");

            resolve_types_static(universe, scoped_data, typing_context,
                    synth_var_type, constraint, span)
                    .map_err(|_| {
                        TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into()
                    })
        }

        (_, TypeVar(constraint_id)) => {
            let constraint_var_type = typing_context.get_type_var(constraint_id)
                    .expect("Missing synth var");

            resolve_types_static(universe, scoped_data, typing_context,
                    synthesis, constraint_var_type, span)
                    .map_err(|_| {
                        TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into()
                    })
        }

        _ => Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }.into()),
    }
}

fn resolve_param(universe: &Universe, scoped_data: &ScopedData, 
    typing_context: &mut TypingContext, synth: &AbstractType, 
    constraint: &AbstractType, span: Span) 
    -> Result<(), TypeError> {

    resolve_param_static(universe,
        scoped_data,
        typing_context,
        synth,
        constraint,
        span)
}

fn resolve_param_static(universe: &Universe, scoped_data: &ScopedData, 
    typing_context: &TypingContext, synth: &AbstractType, 
    constraint: &AbstractType, span: Span) 
    -> Result<(), TypeError> {

    use super::type_cons::AbstractType::*;

    match (synth, constraint) {


        (Any, Any) => {
            Ok(())
        }

        // If the synth is Any but a specific type is expected, reject
        // EVAL ORDER
        (Any, _) => {
            return Err(TypeError::UnexpectedType {
                found: constraint.clone(),
                expected: synth.clone(),
                span: span,
            }.into());
        }

        // If the constraint is a width constraint, the provided parameter type cannot be a nominal
        // type. Values of nominal types may carry additional metadata that is generally difficult to
        // construct from anonymous types. On the otherhand, its simple to strip this metadata from
        // values of nominal types.
        //
        // The constraint being a width constriant means that in general, an anonymous struct will
        // be provided to the concrete function.
        (Record { .. }, WidthConstraint { .. }) => {
            return Err(TypeError::UnexpectedType {
                found: constraint.clone(),
                expected: synth.clone(),
                span: span,
            }.into());
        }

        // NOTE(alex): Synth width must be narrower than the constraint width
        (WidthConstraint(ref synth_awc), 
         WidthConstraint(ref constraint_awc)) => {

            for (synth_ident, synth_type) in synth_awc.fields.iter() {
                match constraint_awc.fields.get(synth_ident) {
                    Some(constraint_type) => {
                        resolve_types_static(universe, scoped_data, typing_context,
                            synth_type, constraint_type, span)?;
                    }

                    None => {
                        // Synth width constraint is not narrower than constraint width

                        return Err(TypeError::UnexpectedType {
                            found: constraint.clone(),
                            expected: synth.clone(),
                            span: span,
                        }.into());
                    }
                }
            }

            Ok(())
        }
    
        // NOTE(alex): Synth width must be narrower than the nominal record constraint
        // Calling with the nominal record value on the width constraint is allowed
        (WidthConstraint(ref synth_awc),
        Record {
            abstract_field_map: ref afm,
            ..
        }) => {
            for (synth_ident, synth_type) in synth_awc.fields.iter() {
                match afm.get(synth_ident) {
                    Some(constraint_type) => {
                        resolve_types_static(universe, scoped_data, typing_context,
                            synth_type, constraint_type, span)?
                    }

                    None => {
                        // Synth width constraint is not narrower than nominal constraint
                        return Err(TypeError::UnexpectedType {
                            found: synth.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into());
                    }
                }
            }

            Ok(())
        }

        (TypeVar(synth_id), TypeVar(constraint_id)) => {
            if synth_id == constraint_id {
                Ok(())
            } else {

                let synth_type = typing_context.get_type_var(synth_id.clone())
                    .expect("Missing synth var");
                let constraint_type = typing_context.get_type_var(constraint_id.clone())
                    .expect("Missing synth var");

                resolve_param_static(universe, scoped_data, typing_context,
                    synth_type, constraint_type, span)
                    .map_err(|_| {
                        TypeError::UnexpectedType {
                            found: synth.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into()
                    })
            }
        }

        (TypeVar(synth_id), _) => {
            let synth_var_type = typing_context.get_type_var(synth_id.clone())
                    .expect("Missing synth var");

            resolve_param_static(universe, scoped_data, typing_context,
                    synth_var_type, constraint, span)
                    .map_err(|_| {
                        TypeError::UnexpectedType {
                            found: synth.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into()
                    })
        }

        (_, TypeVar(constraint_id)) => {
            let constraint_var_type = typing_context.get_type_var(constraint_id.clone())
                    .expect("Missing synth var");

            resolve_param_static(universe, scoped_data, typing_context,
                    synth, constraint_var_type, span)
                    .map_err(|_| {
                        TypeError::UnexpectedType {
                            found: synth.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }.into()
                    })
        }

        _ => resolve_types_static(
                universe, 
                scoped_data, 
                typing_context, 
                synth, 
                constraint, 
                span),
    }
}
