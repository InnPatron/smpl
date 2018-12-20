#[macro_use]
extern crate irmatch;
extern crate itertools;
extern crate petgraph;
extern crate strfmt;
extern crate failure;
#[macro_use]
extern crate failure_derive;

mod feature;
mod err;
#[macro_use]
mod ast_macros;
mod parser;
mod ast;
#[macro_use]
mod analysis;
mod code_gen;
mod span;
mod module;

pub use self::module::{ParsedModule, UnparsedModule};
pub use self::err::Error;

pub use self::parser::parse_module;

pub use crate::analysis::Program;
pub use crate::analysis::check_program;

pub use self::code_gen::RustBackend;
pub use self::code_gen::interpreter;
