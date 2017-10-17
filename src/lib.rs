extern crate itertools;
extern crate ascii;

#[macro_use]
mod ast_macros;
mod parser;
mod ast;
mod smpl_type;
mod semantic_ck;
mod ast_visitor;
#[cfg(test)]
mod parser_tests;

use std::ops::Range;

pub type Span = Range<usize>;
