use super::ast::Module;
use crate::err::Error;

#[macro_use]
mod parser_err;
mod tokens;

#[cfg(not(test))]
#[macro_use]
mod parser;
#[cfg(test)]
#[macro_use]
pub mod parser;

#[cfg(not(test))]
mod expr_parser;
#[cfg(test)]
pub mod expr_parser;


#[cfg(test)]
mod parser_tests;


pub fn parse_module(input: &str) -> Result<Module, Error>{
    let tokenizer = tokens::Tokenizer::new(input);
    let mut tokenizer = tokens::BufferedTokenizer::new(tokenizer);
    
    parser::module(&mut tokenizer).map_err(|e| Error::ParseErr(e.to_string()))
}

#[cfg(test)]
pub fn buffer_input(input: &str) -> tokens::BufferedTokenizer {
    let tokenizer = tokens::Tokenizer::new(input);
    tokens::BufferedTokenizer::new(tokenizer)
}
