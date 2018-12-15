use crate::ast::Expr;

use super::tokens::{Token, SpannedError};

#[derive(Debug, Clone, Fail)]
pub enum ParserError {
    #[fail(display = "Unexpected end of input.")]
    UnexpectedEOI,

    #[fail(display = "Unexpected token: '{}'.", _0)]
    UnexpectedToken(Token),

    #[fail(display = "Unchecked parameters in non-builtin function.")]
    NonbuiltinUncheckedParameters,

    #[fail(display = "No function body.")]
    NoFnBody,

    #[fail(display = "Can only pipe function calls.")]
    InvalidPiping(Expr),

    #[fail(display = "'{}'", _0)]
    TokenizerError(SpannedError),
}

impl From<SpannedError> for ParserError {
    fn from(e: SpannedError) -> ParserError {
        ParserError::TokenizerError(e)
    }
}
