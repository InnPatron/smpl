use analysis::TypeId;
use span::Span;
use ast::*;

#[derive(Clone, Debug)]
pub enum Err {
    ControlFlowErr(ControlFlowErr),
    TypeErr(TypeErr),
    ParseErr(String),
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

#[derive(Clone, Debug)]
pub enum ControlFlowErr {
    MissingReturn,
    BadBreak(Span),
    BadContinue(Span),
}

impl From<ControlFlowErr> for Err {
    fn from(err: ControlFlowErr) -> Err {
        Err::ControlFlowErr(err)
    }
}

#[derive(Clone, Debug)]
pub enum TypeErr {
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
    }
}

impl From<TypeErr> for Err {
    fn from(err: TypeErr) -> Err {
        Err::TypeErr(err)
    }
}
