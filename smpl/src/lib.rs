#[macro_use]
extern crate irmatch;
extern crate failure;
extern crate itertools;
extern crate petgraph;
#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate display_derive;

pub mod module;

mod feature;
#[macro_use]
mod ast_macros;
mod ast;
#[macro_use]
mod analysis;
mod code_gen;
mod program;
mod span;

pub mod parser;
pub mod error;
pub use self::module::{ParsedModule, UnparsedModule};

pub use self::parser::parse_module;
pub use crate::analysis::{FnId, ModuleId, TypeId};

pub use crate::analysis::metadata;

pub use crate::code_gen::byte_gen;

pub use crate::program::{CompilableFn, Program};
