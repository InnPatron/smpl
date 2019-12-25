use crate::span::Span;

use super::error::*;
use super::resolve_scope::ScopedData;
use super::analysis_context::AnalysisUniverse;
use super::type_checker::TypingContext;
use super::abstract_type::*;

/// May or may not alter the typing context for inference purposes
pub fn resolve_types(
    universe: &AnalysisUniverse,
    scoped_data: &ScopedData,
    typing_context: &mut TypingContext,
    synthesis: &AbstractType,
    constraint: &AbstractType,
    span: Span,
) -> Result<(), AnalysisError> {
    resolve_types_static(
        universe,
        scoped_data,
        typing_context,
        synthesis,
        constraint,
        span,
    )
}

/// Will NOT perform any inferences
pub fn resolve_types_static(
    universe: &AnalysisUniverse,
    scoped_data: &ScopedData,
    typing_context: &TypingContext,
    synthesis: &AbstractType,
    constraint: &AbstractType,
    span: Span,
) -> Result<(), AnalysisError> {
    use super::abstract_type::AbstractTypeX::*;

    match (synthesis, constraint) {
        (_, Any(_)) => Ok(()),

        (
            Record {
                data: ref synth_span,
                type_id: synth_type_id,
                abstract_field_map:
                    AbstractFieldMap {
                        fields: synth_fields,
                        ..
                    },
            },
            Record {
                data: ref constraint_span,
                type_id: constraint_type_id,
                abstract_field_map:
                    AbstractFieldMap {
                        fields: constraint_fields,
                        ..
                    },
            },
        ) => {
            // Nominal check
            if synth_type_id != constraint_type_id {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into());
            }

            for (synth_field_id, synth_type) in synth_fields.iter() {
                let constraint_type = constraint_fields
                    .get(synth_field_id)
                    .expect("Equal type id means identical field IDs");

                resolve_types_static(
                    universe,
                    scoped_data,
                    typing_context,
                    synth_type,
                    constraint_type,
                    span.clone(),
                )?;
            }

            Ok(())
        }

        // Synth width must be wider than constraint width
        (
            WidthConstraint {
                data: ref synth_span,
                width: ref synth_awc
            },
            WidthConstraint {
                data: ref constraint_span,
                width: ref constraint_awc,
            },
        ) => {
            let constraint_awc = constraint_awc
                .clone()
                .evaluate(universe, scoped_data, typing_context)?;
            let synth_awc = synth_awc
                .clone()
                .evaluate(universe, scoped_data, typing_context)?;
            for (constraint_ident, constraint_type) in
                constraint_awc.fields().iter()
            {
                match synth_awc.fields().get(constraint_ident) {
                    Some(synth_type) => {
                        resolve_types_static(
                            universe,
                            scoped_data,
                            typing_context,
                            synth_type,
                            constraint_type,
                            span.clone(),
                        )?;
                    }

                    None => {
                        // Synth width constraint is not wider than constraint width
                        return Err(TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }
                        .into());
                    }
                }
            }

            Ok(())
        }

        // Synth nominal record must be wider than constraint width
        (
            Record {
                abstract_field_map: ref synth_afm,
                ..
            },
            WidthConstraint {
                data: ref constraint_span,
                width: ref constraint_awc
            },
        ) => {
            let constraint_awc = constraint_awc
                .clone()
                .evaluate(universe, scoped_data, typing_context)?;

            for (constraint_ident, constraint_type) in
                constraint_awc.fields().iter()
            {
                match synth_afm.get(constraint_ident) {
                    Some(synth_type) => {
                        resolve_types_static(
                            universe,
                            scoped_data,
                            typing_context,
                            synth_type,
                            constraint_type,
                            span.clone(),
                        )?;
                    }

                    None => {
                        // TODO: Missing field
                        // Synth nominal record is not wider than constraint width
                        return Err(TypeError::UnexpectedType {
                            found: synthesis.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }
                        .into());
                    }
                }
            }

            Ok(())
        }

        (
            UncheckedFunction {
                data: ref synth_span,
                return_type: ref synth_return,
            },
            UncheckedFunction {
                data: ref constraint_span,
                return_type: ref constraint_return,
            },
        ) => resolve_types_static(
            universe,
            scoped_data,
            typing_context,
            synth_return,
            constraint_return,
            synth_span.clone(),
        ),

        (
            Function {
                data: ref synth_span,
                parameters: ref synth_params,
                return_type: ref synth_return,
            },
            Function {
                data: ref constraint_span,
                parameters: ref constraint_params,
                return_type: ref constraint_return,
            },
        ) => {
            if synth_params.len() != constraint_params.len() {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into());
            }

            // Function parameters must be contravariant
            for (index, (sp, cp)) in
                synth_params.iter().zip(constraint_params).enumerate()
            {
                resolve_param_static(
                    universe,
                    scoped_data,
                    typing_context,
                    sp,
                    cp,
                    synth_span.clone(),
                )
                .map_err(|_| {
                    TypeError::FunctionTypeMismatch {
                        fn_found: synthesis.clone(),
                        fn_expected: constraint.clone(),
                        param_found: sp.clone(),
                        param_expected: cp.clone(),
                        index: index,
                        span: synth_span.clone(),
                    }
                })?;
            }

            resolve_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_return,
                constraint_return,
                synth_span.clone(),
            )
        }

        (
            Array {
                data: ref synth_span,
                element_type: ref synth_element,
                size: synth_size,
            },
            Array {
                data: ref constraint_span,
                element_type: ref constraint_element,
                size: constraint_size,
            },
        ) => {
            // TODO: Allow synth to be larger?
            if synth_size != constraint_size {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span.clone(),
                }
                .into());
            }

            resolve_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_element,
                constraint_element,
                synth_span.clone(),
            )
        }

        (synth_app @ App { .. }, constraint) => {
            let new_synthesis = synth_app
                .substitute(universe, scoped_data, typing_context)
                .unwrap();

            resolve_types_static(
                universe,
                scoped_data,
                typing_context,
                &new_synthesis,
                constraint,
                span,
            )
        }

        (synthesis, constraint_app @ App { .. }) => {
            let new_constraint = constraint_app
                .substitute(universe, scoped_data, typing_context)
                .unwrap();

            resolve_types_static(
                universe,
                scoped_data,
                typing_context,
                synthesis,
                &new_constraint,
                span,
            )
        }

        (Int(_), Int(_)) => Ok(()),
        (Float(_), Float(_)) => Ok(()),
        (Bool(_), Bool(_)) => Ok(()),
        (String(_), String(_)) => Ok(()),
        (Unit(_), Unit(_)) => Ok(()),

        (synthesis @ Opaque { .. }, constraint @ Opaque { .. }) => {
            super::type_equality::equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synthesis,
                constraint,
                span,
            )
        }

        // Unconstrained type parameters
        // Check if type parameters are equal
        (TypeVar(ref synth_span, synth_id), TypeVar(ref constraint_span, constraint_id)) => {
            if synth_id == constraint_id {
                Ok(())
            } else {
                Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into())
            }
        }

        (TypeVar(ref synth_span, synth_id), constraint) => {
            let synth_var_type = typing_context
                .get_type_var(synth_id.clone())
                .expect("Missing synth var");

            resolve_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_var_type,
                constraint,
                span.clone(),
            )
            .map_err(|_| {
                TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span.clone(),
                }
                .into()
            })
        }

        (synthesis, TypeVar(ref constraint_span, constraint_id)) => {
            let constraint_var_type = typing_context
                .get_type_var(constraint_id.clone())
                .expect("Missing synth var");

            resolve_types_static(
                universe,
                scoped_data,
                typing_context,
                synthesis,
                constraint_var_type,
                span.clone(),
            )
            .map_err(|_| {
                TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into()
            })
        }

        _ => Err(TypeError::UnexpectedType {
            found: synthesis.clone(),
            expected: constraint.clone(),
            span: span,
        }
        .into()),
    }
}

fn resolve_param(
    universe: &AnalysisUniverse,
    scoped_data: &ScopedData,
    typing_context: &mut TypingContext,
    synth: &AbstractType,
    constraint: &AbstractType,
    span: Span,
) -> Result<(), AnalysisError> {
    resolve_param_static(
        universe,
        scoped_data,
        typing_context,
        synth,
        constraint,
        span,
    )
}

fn resolve_param_static(
    universe: &AnalysisUniverse,
    scoped_data: &ScopedData,
    typing_context: &TypingContext,
    synth: &AbstractType,
    constraint: &AbstractType,
    span: Span,
) -> Result<(), AnalysisError> {
    use super::abstract_type::AbstractTypeX::*;

    match (synth, constraint) {
        (Any(_), Any(_)) => Ok(()),

        // If the synth is Any but a specific type is expected, reject
        // EVAL ORDER
        (Any(_), _) => {
            return Err(TypeError::UnexpectedType {
                found: constraint.clone(),
                expected: synth.clone(),
                span: span,
            }
            .into());
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
            }
            .into());
        }

        // NOTE(alex): Synth width must be narrower than the constraint width
        (
            WidthConstraint {
                data: ref synth_span,
                width: ref synth_awc
            },
            WidthConstraint {
                data: ref constraint_span,
                width: ref constraint_awc,
            },
        ) => {
            let constraint_awc = constraint_awc
                .clone()
                .evaluate(universe, scoped_data, typing_context)?;
            let synth_awc = synth_awc
                .clone()
                .evaluate(universe, scoped_data, typing_context)?;

            for (synth_ident, synth_type) in synth_awc.fields().iter() {
                match constraint_awc.fields().get(synth_ident) {
                    Some(constraint_type) => {
                        resolve_types_static(
                            universe,
                            scoped_data,
                            typing_context,
                            synth_type,
                            constraint_type,
                            span.clone(),
                        )?;
                    }

                    None => {
                        // Synth width constraint is not narrower than constraint width

                        return Err(TypeError::UnexpectedType {
                            found: constraint.clone(),
                            expected: synth.clone(),
                            span: span,
                        }
                        .into());
                    }
                }
            }

            Ok(())
        }

        // NOTE(alex): Synth width must be narrower than the nominal record constraint
        // Calling with the nominal record value on the width constraint is allowed
        (
            WidthConstraint {
                data: ref synth_span,
                width: ref synth_awc
            },
            Record {
                abstract_field_map: ref afm,
                ..
            },
        ) => {

            let synth_awc = synth_awc
                .clone()
                .evaluate(universe, scoped_data, typing_context)?;
            for (synth_ident, synth_type) in synth_awc.fields().iter() {
                match afm.get(synth_ident) {
                    Some(constraint_type) => resolve_types_static(
                        universe,
                        scoped_data,
                        typing_context,
                        synth_type,
                        constraint_type,
                        span.clone(),
                    )?,

                    None => {
                        // Synth width constraint is not narrower than nominal constraint
                        return Err(TypeError::UnexpectedType {
                            found: synth.clone(),
                            expected: constraint.clone(),
                            span: span,
                        }
                        .into());
                    }
                }
            }

            Ok(())
        }

        (TypeVar(ref synth_span, synth_id), TypeVar(ref constraint_span, constraint_id)) => {
            if synth_id == constraint_id {
                Ok(())
            } else {
                let synth_type = typing_context
                    .get_type_var(synth_id.clone())
                    .expect("Missing synth var");
                let constraint_type = typing_context
                    .get_type_var(constraint_id.clone())
                    .expect("Missing synth var");

                resolve_param_static(
                    universe,
                    scoped_data,
                    typing_context,
                    synth_type,
                    constraint_type,
                    span.clone(),
                )
                .map_err(|_| {
                    TypeError::UnexpectedType {
                        found: synth.clone(),
                        expected: constraint.clone(),
                        span: span,
                    }
                    .into()
                })
            }
        }

        (TypeVar(ref synth_span, synth_id), _) => {
            let synth_var_type = typing_context
                .get_type_var(synth_id.clone())
                .expect("Missing synth var");

            resolve_param_static(
                universe,
                scoped_data,
                typing_context,
                synth_var_type,
                constraint,
                span.clone(),
            )
            .map_err(|_| {
                TypeError::UnexpectedType {
                    found: synth.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into()
            })
        }

        (_, TypeVar(ref constraint_span, constraint_id)) => {
            let constraint_var_type = typing_context
                .get_type_var(constraint_id.clone())
                .expect("Missing synth var");

            resolve_param_static(
                universe,
                scoped_data,
                typing_context,
                synth,
                constraint_var_type,
                span.clone(),
            )
            .map_err(|_| {
                TypeError::UnexpectedType {
                    found: synth.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into()
            })
        }

        (synth_app @ App { .. }, constraint) => {
            let new_synthesis = synth_app
                .substitute(universe, scoped_data, typing_context)
                .unwrap();

            resolve_param_static(
                universe,
                scoped_data,
                typing_context,
                &new_synthesis,
                constraint,
                span,
            )
        }

        (synthesis, constraint_app @ App { .. }) => {
            let new_constraint = constraint_app
                .substitute(universe, scoped_data, typing_context)
                .unwrap();

            resolve_param_static(
                universe,
                scoped_data,
                typing_context,
                synthesis,
                &new_constraint,
                span,
            )
        }

        _ => resolve_types_static(
            universe,
            scoped_data,
            typing_context,
            synth,
            constraint,
            span,
        ),
    }
}
