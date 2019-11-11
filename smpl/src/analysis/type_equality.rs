use crate::span::Span;

use super::error::*;
use super::resolve_scope::ScopedData;
use super::semantic_data::Universe;
use super::type_checker::TypingContext;
use super::abstract_type::*;

/// Used for invariance (i.e. types must match EXACTLY)
/// Will NOT perform any inferences
pub fn equal_types_static(
    universe: &Universe,
    scoped_data: &ScopedData,
    typing_context: &TypingContext,
    synthesis: &AbstractType,
    constraint: &AbstractType,
    span: Span,
) -> Result<(), TypeError> {
    use super::abstract_type::AbstractType::*;

    match (synthesis, constraint) {
        (
            Record {
                span: ref synth_span,
                type_id: synth_type_id,
                abstract_field_map:
                    AbstractFieldMap {
                        fields: synth_fields,
                        ..
                    },
            },
            Record {
                span: ref constraint_span,
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

                equal_types_static(
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

        (
            WidthConstraint {
                span: ref synth_span,
                width: ref synth_awc,
            },
            WidthConstraint {
                span: ref constraint_span,
                width: ref constraint_awc,
            },
        ) => {
            if constraint_awc.fields.len() != synth_awc.fields.len() {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into());
            }

            for (constraint_ident, constraint_type) in
                constraint_awc.fields.iter()
            {
                match synth_awc.fields.get(constraint_ident) {
                    Some(synth_type) => {
                        equal_types_static(
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

        (
            UncheckedFunction {
                span: ref synth_span,
                return_type: ref synth_return,
            },
            UncheckedFunction {
                span: ref constraint_span,
                return_type: ref constraint_return,
            },
        ) => equal_types_static(
            universe,
            scoped_data,
            typing_context,
            synth_return,
            constraint_return,
            synth_span.clone(),
        ),

        (
            Function {
                span: ref synth_span,
                parameters: ref synth_params,
                return_type: ref synth_return,
            },
            Function {
                span: ref constraint_span,
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
                equal_types_static(
                    universe,
                    scoped_data,
                    typing_context,
                    sp,
                    cp,
                    span.clone(),
                )
                .map_err(|_| {
                    TypeError::FunctionTypeMismatch {
                        fn_found: synthesis.clone(),
                        fn_expected: constraint.clone(),
                        param_found: sp.clone(),
                        param_expected: cp.clone(),
                        index: index,
                        span: span.clone(),
                    }
                })?;
            }

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_return,
                constraint_return,
                synth_span.clone(),
            )
        }

        // TODO: Use this span?
        (
            Array {
                span: ref synth_span,
                element_type: ref synth_element,
                size: synth_size,
            },
            Array {
                span: ref constraint_span,
                element_type: ref constraint_element,
                size: constraint_size,
            },
        ) => {
            // TODO: Allow synth to be larger?
            if synth_size != constraint_size {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: synth_span.clone(),
                }
                .into());
            }

            equal_types_static(
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

            equal_types_static(
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

            equal_types_static(
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
        (Bool, Bool) => Ok(()),
        (String, String) => Ok(()),
        (Unit, Unit) => Ok(()),

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

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_var_type,
                constraint,
                synth_span.clone(),
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

        (synthesis, TypeVar(ref constraint_span, constraint_id)) => {
            let constraint_var_type = typing_context
                .get_type_var(constraint_id.clone())
                .expect("Missing synth var");

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synthesis,
                constraint_var_type,
                constraint_span.clone(),
            )
            .map_err(|_| {
                TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: constraint_span.clone(),
                }
                .into()
            })
        }

        (
            Opaque {
                span: ref synth_span,
                type_id: synth_id,
                args: ref synth_args,
            },
            Opaque {
                span: ref constraint_span,
                type_id: constraint_id,
                args: ref constraint_args,
            },
        ) => {
            if synth_id != constraint_id {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into());
            }

            for (synthesis, constraint) in
                synth_args.iter().zip(constraint_args.iter())
            {
                equal_types_static(
                    universe,
                    scoped_data,
                    typing_context,
                    synthesis,
                    constraint,
                    span.clone(),
                )?;
            }

            Ok(())
        }

        _ => Err(TypeError::UnexpectedType {
            found: synthesis.clone(),
            expected: constraint.clone(),
            span: span,
        }
        .into()),
    }
}
