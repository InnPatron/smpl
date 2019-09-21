#[macro_use]
extern crate irmatch;
extern crate failure;
extern crate itertools;
extern crate petgraph;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate display_derive;

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
pub use crate::analysis::{ TypeId, FnId, ModuleId };

pub use crate::analysis::{ Program, check_program, metadata };

pub use crate::code_gen::byte_gen;
