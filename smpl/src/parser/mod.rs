use super::ast::Module;
use err::Err;

#[cfg(not(test))]
mod lalr_parser;
mod tokens;

#[cfg(test)]
pub mod lalr_parser;
#[cfg(test)]
pub use self::lalr_parser::*;

#[cfg(test)]
mod parser_tests;


pub fn parse_module(input: &str) -> Result<Module, Err>{
    let tokenizer = tokens::Tokenizer::new(input).map(|result| {
        result.map(|spanned| {
            let (span, tok) = spanned.to_data();
            (span.start().byte_index(), tok, span.end().byte_index())
        })
    });
    let parser = lalr_parser::ModuleParser::new();
    parser.parse(tokenizer).map_err(|e| Err::ParseErr(format!("{:?}", e)))
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
