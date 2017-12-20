extern crate itertools;
extern crate ascii;
extern crate petgraph;

#[macro_use]
mod ast_macros;
mod parser;
mod ast;
mod smpl_type;
mod semantic_ck;
mod control_flow;
#[cfg(test)]
mod parser_tests;

use std::ops::Range;

pub type Span = Range<usize>;
