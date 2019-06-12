
use super::type_cons::*;

pub fn resolve_types(synthesis: &Type, constraint: &Type) -> bool {
    use super::type_cons::Type::*;
    match (synthesis, constraint) {
        (UncheckedFunction {
            return_type: ref synth_return,
        }, UncheckedFunction {
            return_type: ref constraint_return,
        }) => {

            resolve_types(synth_return, constraint_return)
        },

        (Function {
            parameters: ref synth_params,
            return_type: ref synth_return,
        }, Function {
            parameters: ref constraint_params,
            return_type: ref constraint_return,
        }) => {

            if synth_params.len() != constraint_params.len() {
                return false;
            }

            for (sp, cp) in synth_params.iter().zip(constraint_params) {
                if resolve_types(sp, cp) == false {
                    return false;
                }
            }

            resolve_types(synth_return, constraint_return)
        },

        (Array {
            element_type: ref synth_e,
            size: synth_size,
        }, Array {
            element_type: ref constraint_e,
            size: constraint_size,
        }) => {
            synth_size == constraint_size && resolve_types(synth_e, constraint_e)
        },

        (Record {
            type_id: synth_id,
            ..
        }, Record {
            type_id: constraint_id,
            ..
        }) => {
            synth_id == constraint_id
        },

        (Param(synth_id), Param(constraint_id)) => {
            synth_id == constraint_id
        },

        (Int, Int) => true,
        (Float, Float) => true,
        (String, String) => true,
        (Bool, Bool) => true,
        (Unit, Unit) => true,


        (WidthConstraint {
            fields: ref synth_width,
            field_map: ref synth_map,
        }, WidthConstraint {
            fields: ref constraint_width,
            field_map: ref constraint_map,
        }) | (Record {
            fields: ref synth_width,
            field_map: ref synth_map,
            ..
        }, WidthConstraint {
            fields: ref constraint_width,
            field_map: ref constraint_map,
        }) => {

            for (constraint_ident, constraint_field_id) in constraint_map.iter() {
                let synth_id = synth_map.get(constraint_ident);
                let constraint_type = constraint_width.get(constraint_field_id).unwrap();
                match synth_id {
                    Some(synth_id) => {
                        let synth_type = synth_width.get(synth_id).unwrap();
                        if resolve_types(synth_type, constraint_type) == false {
                            return false;
                        }
                    }
                    None => return false,
                }
            }

            true
        },

        _ => false,
    }
}
