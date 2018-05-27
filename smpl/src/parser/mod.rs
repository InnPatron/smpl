use lalrpop_util::ParseError;
use super::ast::Module;
use err::Err;

#[cfg(not(test))]
mod lalr_parser;


#[cfg(test)]
pub mod lalr_parser;
#[cfg(test)]
pub use self::lalr_parser::*;

#[cfg(test)]
mod parser_tests;


pub fn parse_module(input: &str) -> Result<Module, Err>{
    let input = prune_input(input);
    let parser = lalr_parser::ModuleParser::new();
    parser.parse(&input).map_err(|e| Err::ParseErr(format!("{:?}", e)))
}

fn prune_input(input: &str) -> String {
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
