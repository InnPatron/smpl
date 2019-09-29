use std::collections::HashMap;

use crate::ast;
use crate::span::Span;

use super::type_cons::*;
use super::error::*;
use super::semantic_data::{TypeParamId, Universe };
use super::resolve_scope::ScopedData;
use super::type_checker::TypingContext;

pub fn resolve_types(universe: &Universe, scoped_data: &ScopedData, 
    typing_context: &mut TypingContext, synthesis: &AbstractType, 
    constraint: &AbstractType, span: Span) 
    -> Result<(), TypeError> {

    use super::type_cons::AbstractType::*;

    let synthesis = synthesis.apply(universe, scoped_data).unwrap();
    let constraint = constraint.apply(universe, scoped_data).unwrap();

    match (synthesis, constraint) {
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
               unimplemented!() 
            }

            for (synth_field_id, synth_type) in synth_fields.iter() {
                let constraint_type = constraint_fields
                    .get(synth_field_id)
                    .expect("Equal type id means identical field IDs");

                resolve_types(universe, scoped_data, typing_context,
                    synth_type, constraint_type, span)?;
            }

            Ok(())
        }

        (UncheckedFunction {
            return_type: ref synth_return,
        }, UncheckedFunction {
            return_type: ref constraint_return,
        }) => {
            resolve_types(universe, scoped_data, typing_context, 
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
                unimplemented!();
            }

            // Function parameters must be contravariant
            for (sp, cp) in synth_params.iter().zip(constraint_params) {
                resolve_param(universe, scoped_data, typing_context,
                    sp, cp, span)?;
            }

            resolve_types(universe, scoped_data, typing_context,
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
                unimplemented!();
            }

            resolve_types(universe, scoped_data, typing_context,
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

        _ => unimplemented!(),
    }
}

fn resolve_param(universe: &Universe, scoped_data: &ScopedData, 
    typing_context: &mut TypingContext, synth: &AbstractType, 
    constraint: &AbstractType, span: Span) 
    -> Result<(), TypeError> {

    use super::type_cons::AbstractType::*;

    match (synth, constraint) {

        // If the constraint is a width constraint, the provided parameter type cannot be a nominal
        // type. Values of nominal types may carry additional metadata that is generally difficult to
        // construct from anonymous types. On the otherhand, its simple to strip this metadata from
        // values of nominal types.
        //
        // The constraint being a width constriant means that in general, an anonymous struct will
        // be provided to the concrete function.
        (Record { .. }, WidthConstraint { .. }) => {
            // False
            unimplemented!()
        }

        // NOTE(alex): Synth width must be narrower than the constraint width
        (WidthConstraint(ref synth_awc), 
         WidthConstraint(ref constraint_awc)) => {

            for (synth_ident, synth_type) in synth_awc.fields.iter() {
                match constraint_awc.fields.get(synth_ident) {
                    Some(constraint_type) => {
                        resolve_types(universe, scoped_data, typing_context,
                            synth_type, constraint_type, span)?;
                    }

                    None => {
                        // Synth width constraint is not narrower than constraint width
                        unimplemented!()
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
                        resolve_types(universe, scoped_data, typing_context,
                            synth_type, constraint_type, span)?
                    }

                    None => {
                        // Synth width constraint is not narrower than nominal constraint
                        unimplemented!()
                    }
                }
            }

            Ok(())
        }

        _ => resolve_types(
                universe, 
                scoped_data, 
                typing_context, 
                synth, 
                constraint, 
                span),
    }
}
