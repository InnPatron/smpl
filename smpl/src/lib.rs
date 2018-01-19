extern crate ascii;
extern crate itertools;
extern crate petgraph;
extern crate lalrpop_util;

mod err;
#[macro_use]
mod ast_macros;
mod parser;
mod ast;
mod smpl_type;
mod analysis;
mod code_gen;


pub use self::err::Err;


pub use self::parser::parse_program;
pub use self::ast::Program as Ast;


pub use analysis::Program;
pub use analysis::check_ast;


pub use self::code_gen::RustCodeGenerator;



use std::ops::Range;

pub type Span = Range<usize>;
