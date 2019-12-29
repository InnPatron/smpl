extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate irmatch;

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod vm_tests;

#[macro_use]
pub mod err;

mod vm;
mod vm_i;
mod env;
mod value;
mod builtins;
mod std_options;
mod module;
mod executor;

pub use value:: {
    ReferableValue,
    Value,
    Struct,
    Array,
};

pub use vm_i::{
    FnHandle,
    TypeHandle,
    ArgType,
    BuiltinResult,
    NativeReturn,
    BuiltinFn,
};
pub use module::VmModule;

pub use std_options::*;

pub use vm::{ SpawnOptions, AVM };
pub use executor::Executor;

pub use smpl::prelude::{ ParsedModule, UnparsedModule, parse_module };

pub use builtins::erase;
