use crate::span::Span;

#[derive(Debug, Clone)]
pub struct WfError {
    pub span: Span,
    pub error: WfErrorKind,
}

#[derive(Debug, Clone)]
pub enum WfErrorKind {
    WhileExprNotTotal,
    IfExprNotTotal,
    BadUnderscore,
    MissingModuleDecl,
    UnknownLiteralKind,
}
