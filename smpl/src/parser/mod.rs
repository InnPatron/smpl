use super::ast::Module;
use err::Err;

mod tokens;

#[cfg(not(test))]
mod parser;
#[cfg(test)]
pub mod parser;

#[cfg(not(test))]
mod expr_parser;
#[cfg(test)]
pub mod expr_parser;


#[cfg(test)]
mod parser_tests;


pub fn parse_module(input: &str) -> Result<Module, Err>{
    let tokenizer = tokens::Tokenizer::new(input);
    let mut tokenizer = tokens::BufferedTokenizer::new(tokenizer);
    
    parser::module(&mut tokenizer).map_err(|e| Err::ParseErr(e))
}

#[cfg(test)]
pub fn buffer_input(input: &str) -> tokens::BufferedTokenizer {
    let tokenizer = tokens::Tokenizer::new(input);
    tokens::BufferedTokenizer::new(tokenizer)
}
