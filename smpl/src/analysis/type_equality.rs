use crate::span::Span;

use super::error::*;
use super::resolve_scope::ScopedData;
use super::semantic_data::Universe;
use super::type_checker::TypingContext;
use super::type_cons::*;

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
    use super::type_cons::AbstractType::*;

    match (synthesis, constraint) {
        (
            Record {
                type_id: synth_type_id,
                abstract_field_map:
                    AbstractFieldMap {
                        fields: synth_fields,
                        ..
                    },
            },
            Record {
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
                    span,
                )?;
            }

            Ok(())
        }

        (
            WidthConstraint(ref synth_awc),
            WidthConstraint(ref constraint_awc),
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
                            span,
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
                return_type: ref synth_return,
            },
            UncheckedFunction {
                return_type: ref constraint_return,
            },
        ) => equal_types_static(
            universe,
            scoped_data,
            typing_context,
            synth_return,
            constraint_return,
            span,
        ),

        (
            Function {
                parameters: ref synth_params,
                return_type: ref synth_return,
            },
            Function {
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
                    span,
                )
                .map_err(|_| {
                    TypeError::FunctionTypeMismatch {
                        fn_found: synthesis.clone(),
                        fn_expected: constraint.clone(),
                        param_found: sp.clone(),
                        param_expected: cp.clone(),
                        index: index,
                        span: span,
                    }
                })?;
            }

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_return,
                constraint_return,
                span,
            )
        }

        (
            Array {
                element_type: ref synth_element,
                size: synth_size,
            },
            Array {
                element_type: ref constraint_element,
                size: constraint_size,
            },
        ) => {
            // TODO: Allow synth to be larger?
            if synth_size != constraint_size {
                return Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into());
            }

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_element,
                constraint_element,
                span,
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
                Err(TypeError::UnexpectedType {
                    found: synthesis.clone(),
                    expected: constraint.clone(),
                    span: span,
                }
                .into())
            }
        }

        (TypeVar(synth_id), constraint) => {
            let synth_var_type = typing_context
                .get_type_var(synth_id.clone())
                .expect("Missing synth var");

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synth_var_type,
                constraint,
                span,
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

        (synthesis, TypeVar(constraint_id)) => {
            let constraint_var_type = typing_context
                .get_type_var(constraint_id.clone())
                .expect("Missing synth var");

            equal_types_static(
                universe,
                scoped_data,
                typing_context,
                synthesis,
                constraint_var_type,
                span,
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

        (
            Opaque {
                type_id: synth_id,
                args: ref synth_args,
            },
            Opaque {
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
                    span,
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
