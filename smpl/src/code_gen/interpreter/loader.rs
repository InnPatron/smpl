use super::builtins::*;
use super::vm::VM;

use ast::Module;

pub fn include(mut modules: Vec<Module>) -> Vec<Module> {
    log::include(&mut modules);
    convert::include(&mut modules);
    math::include(&mut modules);
    err::include(&mut modules);
    str::include(&mut modules);

    modules
}

pub fn load(vm: &mut VM) {
    log::add(vm);
    convert::add(vm);
    math::add(vm);
    err::add(vm);
    str::add(vm);
}
