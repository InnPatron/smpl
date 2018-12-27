use crate::analysis::TypeId;
use crate::span::Span;
use crate::ast::*;
use crate::err::Error;

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
    CyclicType(TypeId),
    LhsRhsInEq(TypeId, TypeId, Span),
    InEqFnReturn {
        expr: TypeId,
        fn_return: TypeId,
        return_span: Span,
    },

    UnexpectedType {
        found: TypeId,
        expected: TypeId,
        span: Span,
    },

    Arity {
        fn_type: TypeId,
        found_args: usize,
        expected_param: usize,
        span: Span,
    },

    BinOp {
        op: BinOp,
        expected: Vec<TypeId>,
        lhs: TypeId,
        rhs: TypeId,
        span: Span,
    },

    UniOp {
        op: UniOp,
        expected: Vec<TypeId>,
        expr: TypeId,
        span: Span,
    },

    ArgMismatch {
        fn_type_id: TypeId,
        index: usize,
        arg: TypeId,
        param: TypeId,
        span: Span,
    },

    FieldAccessOnNonStruct {
        path: Path,
        index: usize,
        invalid_type: TypeId,
        root_type: TypeId,
        span: Span,
    },

    NotAStruct {
        type_name: ModulePath,
        found: TypeId,
        span: Span,
    },

    StructNotFullyInitialized {
        type_name: ModulePath,
        struct_type: TypeId,
        missing_fields: Vec<Ident>,
        span: Span,
    },

    UnknownField {
        name: Ident,
        struct_type: TypeId,
        span: Span,
    },

    HeterogenousArray {
        expected: TypeId,
        found: TypeId,
        index: usize,
        span: Span,
    },

    NotAnArray {
        found: TypeId,
        span: Span,
    },

    InvalidIndex {
        found: TypeId,
        span: Span,
    },

    InitOpaqueType {
        struct_type: TypeId,
        span: Span,
    },

    ApplicationError(ApplicationError),
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
