use crate::analysis::TypeId;
use crate::span::Span;
use crate::ast::*;

#[derive(Clone, Debug)]
pub enum Error {
    AnalysisError(String),
    ParseErr(String),
}
