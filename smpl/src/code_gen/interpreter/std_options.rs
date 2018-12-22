use crate::code_gen::interpreter::VmModule;
use crate::code_gen::interpreter::builtins;

macro_rules! include {
    ($v: expr, $self: expr, $field: ident, $builtin: expr) => {
        if $self.$field {
            $v.push($builtin)
        }
    }
}

#[derive(Builder, Clone)]
pub struct Std {
    #[builder(default = "false")]
    convert: bool,
    #[builder(default = "false")]
    err: bool,
    #[builder(default = "false")]
    log: bool,
    #[builder(default = "false")]
    math: bool,
    #[builder(default = "false")]
    str: bool,
}

impl Std {
    pub fn std() -> Std {
        Std {
            convert: true,
            err: true,
            log: true,
            math: true,
            str: true
        }
    }

    pub fn no_std() -> Std {
        Std {
            convert: false,
            err: false,
            log: false,
            math: false,
            str: false
        }
    }

    pub fn include(&self, v: &mut Vec<VmModule>) {
        include!(v, self, convert, builtins::convert::vm_module());
        include!(v, self, err, builtins::err::vm_module());
        include!(v, self, log, builtins::log::vm_module());
        include!(v, self, math, builtins::math::vm_module());
        include!(v, self, str, builtins::str::vm_module());
    }
}
