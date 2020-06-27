use std::fmt;

use failure::*;

use super::tokens::{SpannedError, Token};
use crate::ast::Expr;
use crate::span::LocationSpan;

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub struct ParserError {
    kind: ParserErrorKind,
    location: Option<LocationSpan>,
    parser_states: Vec<ParserState>,
}

impl Fail for ParserError {
    fn cause(&self) -> Option<&dyn Fail> {
        self.kind.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.kind.backtrace()
    }
}

impl ParserError {
    pub fn new(
        kind: ParserErrorKind,
        location: Option<LocationSpan>,
    ) -> ParserError {
        ParserError {
            kind: kind,
            location: location,
            parser_states: Vec::new(),
        }
    }

    pub fn push_state(mut self, state: ParserState) -> ParserError {
        self.parser_states.push(state);
        self
    }

    pub fn state_trace<'a>(&'a self) -> impl Iterator<Item = &'a ParserState> {
        self.parser_states.iter()
    }

    pub fn error_state(&self) -> &ParserState {
        self.parser_states.get(0).unwrap()
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at state {}", self.kind, self.error_state())
    }
}

#[derive(Debug, Clone, Fail)]
pub enum ParserErrorKind {
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

impl From<SpannedError> for ParserErrorKind {
    fn from(e: SpannedError) -> ParserErrorKind {
        ParserErrorKind::TokenizerError(e)
    }
}

#[derive(Debug, Clone)]
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
            ..self
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
