use super::builtins::*;
use super::BuiltinMap;

use crate::ast::Module;
use crate::err::Err;

pub type Include = fn(&mut Vec<Module>) -> Result<(), Err>;
pub type Load = fn(&mut dyn BuiltinMap);

macro_rules! option_action {
    ($options: expr, $field: ident, $action: expr) => {{
        if $options.$field {
            $action
        }
    }}
}

#[derive(Clone)]
pub struct Loader {
    std_options: StdOptions,
    modules: Vec<(Include, Option<Load>)>,
}

impl Loader {
    pub fn new(std_options: StdOptions) -> Loader {
        Loader {
            std_options: std_options,
            modules: Vec::new(),
        }
    }

    pub fn add_module(mut self, inc: Include, load: Option<Load>) -> Loader {
        self.modules.push((inc, load));
        self
    }

    pub fn include(&self) -> Result<Vec<Module>, Err> {
        let mut v = Vec::new();
        let options = &self.std_options;

        option_action!(options, convert, convert::include(&mut v));
        option_action!(options, err, err::include(&mut v));
        option_action!(options, log, log::include(&mut v));
        option_action!(options, math, math::include(&mut v));
        option_action!(options, str, str::include(&mut v));

        for &(includer, _) in self.modules.iter() {
            includer(&mut v)?;
        }


        Ok(v)
    }

    pub fn load(&self, m: &mut dyn BuiltinMap) {
        let options = &self.std_options;

        option_action!(options, convert, convert::add(m));
        option_action!(options, err, err::add(m));
        option_action!(options, log, log::add(m));
        option_action!(options, math, math::add(m));
        option_action!(options, str, str::add(m));

        for &(_, loader) in self.modules.iter() {
            if let Some(loader) = loader {
                loader(m);
            }
        }
    }
}

#[derive(Clone)]
pub struct StdOptions {
   pub convert: bool,
   pub err: bool,
   pub log: bool,
   pub math: bool,
   pub str: bool,
}

impl StdOptions {
    pub fn std() -> StdOptions {
        StdOptions {
            convert: true,
            err: true,
            log: true,
            math: true,
            str: true,
        }
    }

    pub fn no_std() -> StdOptions {
        StdOptions {
            convert: false,
            err: false,
            log: false,
            math: false,
            str: false,
        }
    }
}

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
