extern crate itertools;
extern crate ascii;
extern crate petgraph;

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
#[cfg(test)]
mod parser_tests;

use std::ops::Range;

pub type Span = Range<usize>;
