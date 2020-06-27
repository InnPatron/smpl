use crate::module::*;

#[macro_use]
mod macros;

#[macro_use]
pub mod error;
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

mod new_parser;
mod type_parser;
mod stmt_parser;

#[cfg(test)]
mod parser_tests;

#[cfg(test)]
mod new_parser_tests;


use self::error::ParserError;

///
/// Tokenizes and parses an unparsed SMPL module.
///
pub fn parse_module(input: UnparsedModule) -> Result<ParsedModule, ParserError> {
    let (module, source) = (input.module, input.source);
    let tokenizer = tokens::Tokenizer::new(&source, &module);
    let mut tokenizer = tokens::BufferedTokenizer::new(tokenizer);

    let module = parser::module(&mut tokenizer)?;

    Ok(ParsedModule::new(module, source))
}

#[cfg(test)]
pub fn buffer_input<'a, 'b>(source: &'a crate::module::ModuleSource, input: &'b str)
    -> tokens::BufferedTokenizer<'a, 'b> {

    let tokenizer = tokens::Tokenizer::new(source, input);
    tokens::BufferedTokenizer::new(tokenizer)
}
