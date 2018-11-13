use super::ast::Module;
use err::Err;

#[cfg(not(test))]
mod lalr_parser;
mod tokens;
mod parser;
mod expr_parser;

#[cfg(test)]
pub mod lalr_parser;
#[cfg(test)]
pub use self::lalr_parser::*;

#[cfg(test)]
mod parser_tests;


pub fn parse_module(input: &str) -> Result<Module, Err>{
    let tokenizer = tokens::Tokenizer::new(input);
    let mut tokenizer = tokens::BufferedTokenizer::new(tokenizer);
    
    parser::module(&mut tokenizer).map_err(|e| Err::ParseErr(e))
}

#[cfg(test)]
pub fn wrap_input<'a>(input: &'a str) -> Box< Iterator<Item=Result<(usize, tokens::Token, usize), tokens::SpannedError>> +'a> {
    Box::new(tokens::Tokenizer::new(input).map(|result| {
            result.map(|spanned| {
                let (span, tok) = spanned.to_data();
                (span.start().byte_index(), tok, span.end().byte_index())
            })
        })
    )
}

#[cfg(test)]
pub fn buffer_input(input: &str) -> tokens::BufferedTokenizer {
    let tokenizer = tokens::Tokenizer::new(input);
    tokens::BufferedTokenizer::new(tokenizer)
}
