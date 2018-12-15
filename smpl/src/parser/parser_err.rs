use crate::ast::Expr;

use super::tokens::{Token, SpannedError};

#[macro_export]
macro_rules! parser_state {
    ($state: expr) => {{ ParserState::new_state($state) }};
    ($state: expr, $substate: expr) => { ParserState::new_state($state).substate($substate) };
}

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

pub struct ParserState {
    state: String,
    substate: Option<String>,
}

impl ParserState {
    pub fn new_state(state: &str) -> ParserState {
        ParserState {
            state: state.to_string(),
            substate: None,
        }
    }

    pub fn substate(self, substate: &str) -> ParserState {
        ParserState {
            substate: Some(substate.to_string()),
            .. self
        }
    }
}

impl std::fmt::Display for ParserState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.substate {
            Some(ref sub) => write!(f, "[{}::{}]", self.state, sub),
            None => write!(f, "[{}]", self.state),
        }
    }
}
