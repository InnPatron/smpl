#[macro_use]
extern crate irmatch;
extern crate failure;
extern crate itertools;
extern crate petgraph;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate derive_builder;

mod err;
mod feature;
#[macro_use]
mod ast_macros;
mod ast;
mod parser;
#[macro_use]
mod analysis;
mod code_gen;
mod module;
mod span;

pub use self::err::Error;
pub use self::module::{ParsedModule, UnparsedModule};

pub use self::parser::parse_module;

pub use crate::analysis::check_program;
pub use crate::analysis::Program;
