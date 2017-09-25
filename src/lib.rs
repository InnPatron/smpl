extern crate itertools;
extern crate ascii;

mod parser;
mod ast;
mod tokenizer;

use std::ops::Range;

pub type Span = Range<usize>;
