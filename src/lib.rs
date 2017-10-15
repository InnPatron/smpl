extern crate itertools;
extern crate ascii;

mod parser;
mod ast;
mod smpl_type;
mod type_ck;
#[cfg(test)]
mod parser_tests;

use std::ops::Range;

pub type Span = Range<usize>;
