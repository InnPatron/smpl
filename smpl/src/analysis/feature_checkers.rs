use crate::analysis::*;
use crate::feature::*;

pub fn var_type_scanner(u: &Universe, f: &mut PresentFeatures, t: TypeId) {
    // TODO: var type scanner
    unimplemented!();
    /*
    match *u.get_type(t) {
        SmplType::Array(_) => {
            f.add_feature(STATIC_ARRAY);
        }

        SmplType::Function(_) => {
            f.add_feature(FUNCTION_VALUE);
        }

        _ => (),
    }
    */
}

pub fn fn_sig_type_scanner(u: &Universe, f: &mut PresentFeatures, t: TypeId) {
    // TODO: fn_sig_type_scanner
    unimplemented!();
    /*
    match *u.get_type(t) {
        SmplType::Array(_) => {
            f.add_feature(STATIC_ARRAY);
        }

        SmplType::Function(_) => {
            f.add_feature(FUNCTION_VALUE);
        }

        _ => (),
    }
    */
}

pub fn field_type_scanner(u: &Universe, f: &mut PresentFeatures, t: TypeId) {
    // TODO: field type scanner
    unimplemented!();
    /*
    match *u.get_type(t) {
        SmplType::Array(_) => {
            f.add_feature(STATIC_ARRAY);
        }

        SmplType::Function(_) => {
            f.add_feature(FUNCTION_VALUE);
        }

        _ => (),
    }
    */
}
