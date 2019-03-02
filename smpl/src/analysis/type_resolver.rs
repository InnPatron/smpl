
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
        }, WidthConstraint {
            fields: ref constraint_width,
        }) => {

            for (constraint_f, constraint_t) in constraint_width.iter() {
                let synth = synth_width.get(constraint_f);
                match synth {
                    Some(synth_t) => {
                        if resolve_types(synth_t, constraint_t) == false {
                            return false;
                        }
                    }
                    None => return false,
                }
            }

            true
        },

        (Record {
            fields: ref synth_field_id_map,
            field_map: ref synth_ident_map,
            ..
        }, WidthConstraint {
            fields: ref constraint_width,
        }) => {

            for (constraint_f, constraint_t) in constraint_width.iter() {
                let synth_field_id = synth_ident_map.get(constraint_f);
                match synth_field_id {
                    Some(synth_field_id) => {
                        let synth_t = synth_field_id_map.get(synth_field_id).unwrap();
                        if resolve_types(synth_t, constraint_t) == false {
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
