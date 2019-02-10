use crate::span::Span;
use crate::ast::*;
use crate::err::Error;
use crate::analysis::type_cons::{TypeApp, Type};

#[derive(Clone, Debug)]
pub enum AnalysisError {
    ControlFlowError(ControlFlowError),
    TypeError(TypeError),
    ParseError(String),
    MultipleMainFns,
    UnknownType(ModulePath),
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
    LhsRhsInEq(Type, Type, Span),
    InEqFnReturn {
        expr: Type,
        fn_return: Type,
        return_span: Span,
    },

    UnexpectedType {
        found: Type,
        expected: Type,
        span: Span,
    },

    Arity {
        fn_type: Type,
        found_args: usize,
        expected_param: usize,
        span: Span,
    },

    BinOp {
        op: BinOp,
        expected: Vec<Type>,
        lhs: Type,
        rhs: Type,
        span: Span,
    },

    UniOp {
        op: UniOp,
        expected: Vec<Type>,
        expr: Type,
        span: Span,
    },

    ArgMismatch {
        fn_type: Type,
        index: usize,
        arg: Type,
        param: Type,
        span: Span,
    },

    FieldAccessOnNonStruct {
        path: Path,
        index: usize,
        invalid_type: Type,
        root_type: Type,
        span: Span,
    },

    NotAStruct {
        type_name: ModulePath,
        found: Type,
        span: Span,
    },

    StructNotFullyInitialized {
        type_name: ModulePath,
        struct_type: Type,
        missing_fields: Vec<Ident>,
        span: Span,
    },

    UnknownField {
        name: Ident,
        struct_type: Type,
        span: Span,
    },

    HeterogenousArray {
        expected: Type,
        found: Type,
        index: usize,
        span: Span,
    },

    NotAnArray {
        found: Type,
        span: Span,
    },

    InvalidIndex {
        found: Type,
        span: Span,
    },

    InitOpaqueType {
        struct_type: Type,
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
