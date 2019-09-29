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
            abstract_field_map: synth_afm,
        }, Record {
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

        (App { .. }, _) | (_, App { .. }) => {
            unreachable!("No AbstractType::App after apply");
        }

        _ => unimplemented!(),
    }
}
