#[macro_use]
extern crate lalrpop_util;

mod analysis;
mod ast;
mod ast_node;
mod expr_ast;
mod parser;
mod span;

use std::fmt;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub enum Source {
    File(PathBuf),
    Anonymous(String),
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Source::File(ref path) => write!(f, "{}", path.display()),
            Source::Anonymous(ref hint) => write!(f, "{}", hint),
        }
    }
}
