use crate::span::Span;
use crate::ast::*;
use crate::err::Error;
use crate::analysis::type_cons::TypeApp;

#[derive(Clone, Debug)]
pub enum AnalysisError {
    ControlFlowError(ControlFlowError),
    TypeError(TypeError),
    ParseError(String),
    MultipleMainFns,
    UnknownType(TypeAnnotation),
    UnknownBinding(Ident),
    UnknownFn(ModulePath),
    UnresolvedUses(Vec<AstNode<UseDecl>>),
    UnresolvedStructs(Vec<AstNode<Struct>>),
    UnresolvedFns(Vec<AstNode<Function>>),
    UncheckedFunctionBinding(Ident),
    MissingModName,
}

impl From<AnalysisError> for Error {
    fn from(e: AnalysisError) -> Error {
        Error::AnalysisError(format!("{:?}", e))
    }
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
    CyclicType(TypeApp),
    LhsRhsInEq(TypeApp, TypeApp, Span),
    InEqFnReturn {
        expr: TypeApp,
        fn_return: TypeApp,
        return_span: Span,
    },

    UnexpectedType {
        found: TypeApp,
        expected: TypeApp,
        span: Span,
    },

    Arity {
        fn_type: TypeApp,
        found_args: usize,
        expected_param: usize,
        span: Span,
    },

    BinOp {
        op: BinOp,
        expected: Vec<TypeApp>,
        lhs: TypeApp,
        rhs: TypeApp,
        span: Span,
    },

    UniOp {
        op: UniOp,
        expected: Vec<TypeApp>,
        expr: TypeApp,
        span: Span,
    },

    ArgMismatch {
        fn_type_id: TypeApp,
        index: usize,
        arg: TypeApp,
        param: TypeApp,
        span: Span,
    },

    FieldAccessOnNonStruct {
        path: Path,
        index: usize,
        invalid_type: TypeApp,
        root_type: TypeApp,
        span: Span,
    },

    NotAStruct {
        type_name: ModulePath,
        found: TypeApp,
        span: Span,
    },

    StructNotFullyInitialized {
        type_name: ModulePath,
        struct_type: TypeApp,
        missing_fields: Vec<Ident>,
        span: Span,
    },

    UnknownField {
        name: Ident,
        struct_type: TypeApp,
        span: Span,
    },

    HeterogenousArray {
        expected: TypeApp,
        found: TypeApp,
        index: usize,
        span: Span,
    },

    NotAnArray {
        found: TypeApp,
        span: Span,
    },

    InvalidIndex {
        found: TypeApp,
        span: Span,
    },

    InitOpaqueType {
        struct_type: TypeApp,
        span: Span,
    },

    ParameterNamingConflict {
        ident: Ident
    },

    FieldNamingConflict {
        ident: Ident
    },

    TypeParameterNamingConflict {
        ident: Ident
    },

    ParameterizedParameter {
        ident: Ident,
    },

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
    InvalidNumber { param_position: usize, found: i64 }
}

impl From<ApplicationError> for TypeError {
    fn from(err: ApplicationError) -> TypeError {
        TypeError::ApplicationError(err)
    }
}
