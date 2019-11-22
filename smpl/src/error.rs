pub use crate::analysis::error as analysis_error;
pub use crate::parser::error as parser_error;

use analysis_error::AnalysisError;
use parser_error::ParserError;

#[derive(Debug, Clone)]
pub enum Error {
    Analysis(AnalysisError),
    Parser(ParserError),
}

impl From<AnalysisError> for Error {
    fn from(e: AnalysisError) -> Error {
        Error::Analysis(e)
    }
}

impl From<ParserError> for Error {
    fn from(e: ParserError) -> Error {
        Error::Parser(e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            // TODO: Implement Fail/Display for AnalysisError
            Error::Analysis(ref e) => write!(f, "{}", format!("{:?}", e)),

            Error::Parser(ref e) => e.fmt(f),
        }
    }
}
