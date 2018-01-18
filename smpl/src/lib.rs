extern crate ascii;
extern crate itertools;
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
mod linear_cfg_traversal;

use std::ops::Range;

pub type Span = Range<usize>;

pub use self::ast::Program as Ast;
pub use self::semantic_ck::Program;
pub use self::code_gen::RustCodeGenerator;
pub use self::err::Err;

pub use self::semantic_ck::check as check_ast;
pub use self::parser::parse_Program as parse_program;

pub fn prune_input(input: &str) -> String {
    let mut result = String::with_capacity(input.len());

    let mut ignore = false;
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        if ignore == false {
            if c == '/' {
                match chars.peek() {
                    Some(&'/') => ignore = true,
                    _ => (),
                }
            }
        } else if c == '\n' {
            ignore = false;
        } 

        if ignore == false {
            result.push(c);
        }
    }

    result
}
