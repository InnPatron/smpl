use crate::analysis::type_cons::AbstractType;
use crate::ast::*;
use crate::err::Error;
use crate::span::Span;

#[derive(Clone, Debug)]
pub enum AnalysisError {
    ControlFlowError(ControlFlowError),
    TypeError(TypeError),
    ParseError(String),
    MultipleMainFns,
    UnknownType(ModulePath),
    UnknownBinding(Ident, Span),
    UnknownFn(ModulePath),
    UnresolvedUses(Vec<AstNode<UseDecl>>),
    UnresolvedStructs(Vec<AstNode<Struct>>),
    UnresolvedFns(Vec<AstNode<Function>>),
    TopLevelError(TopLevelError),
    MissingModName,
    Errors(Vec<AnalysisError>),
}

impl<T, I> From<T> for AnalysisError
where
    T: std::iter::IntoIterator<Item = I>,
    I: Into<AnalysisError>,
{
    fn from(t: T) -> AnalysisError {
        AnalysisError::Errors(t.into_iter().map(|i| i.into()).collect())
    }
}

impl From<AnalysisError> for Error {
    fn from(e: AnalysisError) -> Error {
        Error::AnalysisError(format!("{:?}", e))
    }
}

impl From<TopLevelError> for AnalysisError {
    fn from(t: TopLevelError) -> AnalysisError {
        AnalysisError::TopLevelError(t)
    }
}

#[derive(Clone, Debug)]
pub enum TopLevelError {
    DuplicateTypes(Ident),
    DuplicateFns(Ident),
}

#[derive(Clone, Debug)]
pub enum ControlFlowError {
    MissingReturn,
    BadBreak(Span),
    BadContinue(Span),
}

impl From<ControlFlowError> for AnalysisError {
    fn from(err: ControlFlowError) -> AnalysisError {
        AnalysisError::ControlFlowError(err)
    }
}

#[derive(Clone, Debug)]
pub enum TypeError {
    LhsRhsInEq(AbstractType, AbstractType, Span),

    IncompatibleLocal {
        name: Ident,
        local_type: AbstractType,
        found_type: AbstractType,
        span: Span,
    },

    InEqFnReturn {
        expr: AbstractType,
        fn_return: AbstractType,
        return_span: Span,
    },

    FunctionTypeMismatch {
        fn_found: AbstractType,
        fn_expected: AbstractType,
        param_found: AbstractType,
        param_expected: AbstractType,
        index: usize,
        span: Span,
    },

    UnexpectedType {
        found: AbstractType,
        expected: AbstractType,
        span: Span,
    },

    Arity {
        fn_type: AbstractType,
        found_args: usize,
        expected_param: usize,
        span: Span,
    },

    BinOp {
        op: BinOp,
        expected: Vec<AbstractType>,
        lhs: AbstractType,
        rhs: AbstractType,
        span: Span,
    },

    UniOp {
        op: UniOp,
        expected: Vec<AbstractType>,
        expr: AbstractType,
        span: Span,
    },

    ArgMismatch {
        fn_type: AbstractType,
        index: usize,
        arg: AbstractType,
        param: AbstractType,
        span: Span,
    },

    FieldAccessOnNonStruct {
        path: Path,
        index: usize,
        invalid_type: AbstractType,
        root_type: AbstractType,
        span: Span,
    },

    NotAStruct {
        type_name: ModulePath,
        found: AbstractType,
        span: Span,
    },

    StructNotFullyInitialized {
        type_name: ModulePath,
        struct_type: AbstractType,
        missing_fields: Vec<Ident>,
        span: Span,
    },

    InvalidInitialization {
        fields: Vec<Ident>,
        span: Span,
    },

    UnknownField {
        name: Ident,
        struct_type: AbstractType,
        span: Span,
    },

    HeterogenousArray {
        expected: AbstractType,
        found: AbstractType,
        index: usize,
        span: Span,
    },

    NotAnArray {
        found: AbstractType,
        span: Span,
    },

    InvalidIndex {
        found: AbstractType,
        span: Span,
    },

    InitOpaqueType {
        struct_type: AbstractType,
        span: Span,
    },

    ParameterNamingConflict {
        ident: Ident,
    },

    FieldNamingConflict {
        ident: Ident,
    },

    TypeParameterNamingConflict {
        ident: Ident,
    },

    ParameterizedParameter {
        ident: Ident,
    },

    InvalidTypeConstraintBase {
        found: AbstractType,
    },

    ConflictingConstraints {
        constraints: Vec<AbstractType>,
    },

    UnknownTypeParameter {
        ident: Ident,
    },

    // TODO: fill this in
    FnAnnLocalTypeParameter,

    ApplicationError(ApplicationError),

    UninstantiatedType,
}

impl From<TypeError> for AnalysisError {
    fn from(err: TypeError) -> AnalysisError {
        AnalysisError::TypeError(err)
    }
}

#[derive(Clone, Debug)]
pub enum ApplicationError {
    Arity { expected: usize, found: usize },
    ExpectedNumber { param_position: usize },
    ExpectedType { param_position: usize },
    InvalidNumber { param_position: usize, found: i64 },
}

impl From<ApplicationError> for TypeError {
    fn from(err: ApplicationError) -> TypeError {
        TypeError::ApplicationError(err)
    }
}
