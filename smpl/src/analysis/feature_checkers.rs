use crate::analysis::smpl_type::*;
use crate::analysis::*;
use crate::feature::*;

pub fn var_type_scanner(u: &Universe, f: &mut PresentFeatures, t: TypeId) {
    match *u.get_type(t) {
        SmplType::Array(_) => {
            f.add_feature(STATIC_ARRAY);
        }

        SmplType::Function(_) => {
            f.add_feature(FUNCTION_VALUE);
        }

        _ => (),
    }
}

pub fn fn_sig_type_scanner(u: &Universe, f: &mut PresentFeatures, t: TypeId) {
    match *u.get_type(t) {
        SmplType::Array(_) => {
            f.add_feature(STATIC_ARRAY);
        }

        SmplType::Function(_) => {
            f.add_feature(FUNCTION_VALUE);
        }

        _ => (),
    }
}

pub fn field_type_scanner(u: &Universe, f: &mut PresentFeatures, t: TypeId) {
    match *u.get_type(t) {
        SmplType::Array(_) => {
            f.add_feature(STATIC_ARRAY);
        }

        SmplType::Function(_) => {
            f.add_feature(FUNCTION_VALUE);
        }

        _ => (),
    }
}
