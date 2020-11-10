use super::tokens::Token;
use crate::span::Location;

#[derive(Debug, PartialEq, Clone)]
// #[fail(display = "{} {}", error, location)]
pub struct SpannedError {
    pub error: TokenizerError,
    pub location: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenizerError {
    // #[fail(display = "Unexpected character: '{}'.", _0)]
    UnexpectedChar(char),

    // #[fail(display = "Unexpected end of input.")]
    UnexpectedEndOfInput,

    // #[fail(display = "Unterminated string literal.")]
    UnterminatedStringLiteral,

    // #[fail(display = "Incomplete token: '{}'.", _0)]
    IncompleteToken(Token),
}
