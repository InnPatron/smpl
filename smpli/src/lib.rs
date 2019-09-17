extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate irmatch;


// mod vm_tests;

#[macro_use]
mod err;

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
    Struct
};

pub use module::VmModule;
