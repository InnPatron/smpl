use super::builtins::*;
use super::BuiltinMap;

use ast::Module;

pub fn include(mut modules: Vec<Module>) -> Vec<Module> {
    log::include(&mut modules);
    convert::include(&mut modules);
    math::include(&mut modules);
    err::include(&mut modules);
    str::include(&mut modules);

    modules
}

pub fn load<MAP: BuiltinMap>(vm: &mut MAP) {
    log::add(vm);
    convert::add(vm);
    math::add(vm);
    err::add(vm);
    str::add(vm);
}
