extern crate ascii;
extern crate itertools;
extern crate petgraph;
extern crate lalrpop_util;

mod err;
#[macro_use]
mod ast_macros;
mod parser;
mod ast;
mod typed_ast;
mod smpl_type;
mod semantic_ck;
#[macro_use]
mod control_flow;
mod expr_flow;
mod fn_analyzer;
mod code_gen;
mod linear_cfg_traversal;

use std::ops::Range;

pub type Span = Range<usize>;

pub use self::ast::Program as Ast;
pub use self::semantic_ck::Program;
pub use self::code_gen::RustCodeGenerator;
pub use self::err::Err;

pub use self::semantic_ck::check as check_ast;
pub use self::parser::parse_program;
