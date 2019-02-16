use crate::analysis::TypeId;
use crate::ast::*;
use crate::span::Span;

#[derive(Clone, Debug)]
pub enum Error {
    AnalysisError(String),
    ParseErr(String),
}
