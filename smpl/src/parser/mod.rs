use crate::err::Error;
use crate::module::*;

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

pub fn parse_module(input: UnparsedModule) -> Result<ParsedModule, Error> {
    let (module, source) = (input.module, input.source);
    let tokenizer = tokens::Tokenizer::new(&source, &module);
    let mut tokenizer = tokens::BufferedTokenizer::new(tokenizer);

    let module = parser::module(&mut tokenizer)
        .map_err(|e| Error::ParseErr(e.to_string()))?;
    Ok(ParsedModule::new(module, source))
}

#[cfg(test)]
pub fn buffer_input(input: &str) -> tokens::BufferedTokenizer {
    let tokenizer = tokens::Tokenizer::new(input);
    tokens::BufferedTokenizer::new(tokenizer)
}
