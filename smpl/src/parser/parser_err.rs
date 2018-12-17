use std::fmt;

use failure::*;

use crate::ast::Expr;
use super::tokens::{Token, SpannedError};

#[macro_export]
macro_rules! parser_error {
    ($kind: expr, $state: expr) => {{
        use crate::parser::parser_err::*;
        ParserError::new($kind)
            .push_state($state)
    }}
}


#[macro_export]
macro_rules! production {
    ($production: expr, $state: expr) => {{ 
        use failure::Fail;
        ($production).map_err(|e| e.context($state))
    }}
}

#[macro_export]
macro_rules! parser_state {
    ($state: expr) => {{ ParserState::new_state($state) }};
    ($state: expr, $substate: expr) => { ParserState::new_state($state).substate($substate) };
}

#[derive(Debug, Clone)]
pub struct ParserError {
    kind: ParserErrorKind,
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
    pub fn new(kind: ParserErrorKind) -> ParserError {
        ParserError {
            kind: kind,
            parser_states: Vec::new(),
        }
    }

    pub fn push_state(mut self, state: ParserState) -> ParserError {
        self.parser_states.push(state);
        self
    }

    pub fn state_trace<'a>(&'a self) -> impl Iterator<Item=&'a ParserState> {
        self.parser_states.iter()
    }

    pub fn error_state(&self) -> &ParserState {
        self.parser_states.0.unwrap()
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
