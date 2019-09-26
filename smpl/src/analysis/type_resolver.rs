use std::collections::HashMap;

use super::type_cons::*;
use super::error::TypeError;
use super::semantic_data::{TypeParamId, ScopedData, Universe };

#[derive(Clone)]
pub struct TypingContext {
    map: HashMap<TypeParamId, AbstractType>,
}

pub fn resolve_types(universe: &Universe, scoped_data: &ScopedData, typing_context: TypingContext,
    synthesis: &AbstractType, constraint: &AbstractType) -> Result<(AbstractType, TypingContext), TypeError> {

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

        (AbstractType::App { .. }, _) | (_, AbstractType::App { .. }) => {
            unreachable!("No AbstractType::App after apply");
        }

        _ => unimplemented!(),
    }

    unimplemented!();
}
