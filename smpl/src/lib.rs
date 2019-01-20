#[macro_use]
extern crate irmatch;
extern crate itertools;
extern crate petgraph;
extern crate strfmt;
extern crate failure;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate derive_builder;

mod feature;
mod err;
#[macro_use]
mod ast_macros;
mod parser;
mod ast;
#[macro_use]
mod analysis;
mod span;
mod module;

pub use self::module::{ParsedModule, UnparsedModule};
pub use self::err::Error;

pub use self::parser::parse_module;

pub use crate::analysis::Program;
pub use crate::analysis::check_program;

