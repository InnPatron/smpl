use crate::builtins;
use crate::module::VmModule;

macro_rules! include {
    ($v: expr, $self: expr, $field: ident, $builtin: expr) => {
        if $self.$field {
            $v.push($builtin)
        }
    };
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
    #[builder(default = "false")]
    option: bool,
    #[builder(default = "false")]
    vec: bool,
}

impl Std {
    pub fn std() -> Std {
        Std {
            convert: true,
            err: true,
            log: true,
            math: true,
            str: true,
            option: true,
            vec: true,
        }
    }

    pub fn no_std() -> Std {
        Std {
            convert: false,
            err: false,
            log: false,
            math: false,
            str: false,
            option: false,
            vec: false,
        }
    }

    pub fn include(&self, v: &mut Vec<VmModule>) {
        include!(v, self, convert, builtins::convert::vm_module());
        include!(v, self, err, builtins::err::vm_module());
        include!(v, self, log, builtins::log::vm_module());
        include!(v, self, math, builtins::math::vm_module());
        include!(v, self, str, builtins::str::vm_module());
        include!(v, self, option, builtins::option::vm_module());
        include!(v, self, vec, builtins::vec::vm_module());

        // vec requires option
        if !self.option {
            include!(v, self, vec, builtins::option::vm_module());
        }
    }
}
