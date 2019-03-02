#[derive(Clone, Debug)]
pub enum Error {
    AnalysisError(String),
    ParseErr(String),
}
