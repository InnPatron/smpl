macro_rules! consume_token  {

    ($input: expr, $state: expr) => {{
        let next = $input.next()
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(ParserErrorKind::TokenizerError(e), $state))?;
        next.to_data()
    }};

    ($input: expr, $token: pat, $state: expr) => {{
        let next = $input.next()
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(ParserErrorKind::TokenizerError(e), $state))?;
        let data = next.to_data();
        match data.1 {
            $token => data,
            _ => Err(parser_error!(ParserErrorKind::UnexpectedToken(data.1), $state, Some(data.0)))?,
        }
    }};

    ($input: expr, $token: pat => $e: expr, $state: expr) => {{
        let next = $input.next()
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(ParserErrorKind::TokenizerError(e), $state))?;
        let data = next.to_data();
        match data.1 {
            $token => (data.0, $e),
            _ => Err(parser_error!(ParserErrorKind::UnexpectedToken(data.1), $state, Some(data.0)))?,
        }
    }};
}

macro_rules! peek_token {
    ($tokenizer: expr, $lam: expr, $state: expr) => {
        ($tokenizer)
            .peek($lam)
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(e.into(), $state))?
    };

    ($tokenizer: expr, SPAN $lam: expr, $state: expr) => {
        ($tokenizer)
            .peek_span($lam)
            .ok_or(parser_error!(ParserErrorKind::UnexpectedEOI, $state))?
            .map_err(|e| parser_error!(e.into(), $state))?
    };
}

macro_rules! parser_error {
    ($kind: expr, $state: expr) => {{
        parser_error!($kind, $state, None)
    }};

    ($kind: expr, $state: expr, $location: expr) => {{
        use crate::parser::error::*;
        ParserError::new($kind, $location.into()).push_state($state)
    }};
}

macro_rules! production {
    ($production: expr, $state: expr) => {{
        use crate::parser::error::*;
        ($production).map_err(|e| e.push_state($state))?
    }};
}

macro_rules! parser_state {
    ($state: expr) => {{
        ParserState::new_state($state)
    }};
    ($state: expr, $substate: expr) => {
        ParserState::new_state($state).substate($substate)
    };
}
