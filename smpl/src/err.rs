use analysis::{FnId, TypeId};
use ast::{BinOp, Ident, TypeAnnotation, ModulePath, Path, UniOp};

#[derive(Clone, Debug)]
pub enum Err {
    ControlFlowErr(ControlFlowErr),
    TypeErr(TypeErr),
    ParseErr(String),
    MultipleMainFns,
    UnknownType(TypeAnnotation),
    UnknownBinding(Ident),
    UnknownFn(ModulePath),
    UnresolvedUses(Vec<Ident>),
    UnresolvedStructs(Vec<Ident>),
    UnresolvedFns(Vec<Ident>),
    MissingModName,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ControlFlowErr {
    MissingReturn,
    BadBreak,
    BadContinue,
}

impl From<ControlFlowErr> for Err {
    fn from(err: ControlFlowErr) -> Err {
        Err::ControlFlowErr(err)
    }
}

#[derive(Clone, Debug)]
pub enum TypeErr {
    LhsRhsInEq(TypeId, TypeId),
    InEqFnReturn {
        expr: TypeId,
        fn_return: TypeId,
    },

    UnexpectedType {
        found: TypeId,
        expected: TypeId,
    },

    Arity {
        fn_type: TypeId,
        found_args: usize,
        expected_param: usize,
    },

    BinOp {
        op: BinOp,
        expected: Vec<TypeId>,
        lhs: TypeId,
        rhs: TypeId,
    },

    UniOp {
        op: UniOp,
        expected: Vec<TypeId>,
        expr: TypeId,
    },

    ArgMismatch {
        fn_type_id: TypeId,
        index: usize,
        arg: TypeId,
        param: TypeId,
    },

    FieldAccessOnNonStruct {
        path: Path,
        index: usize,
        invalid_type: TypeId,
        root_type: TypeId,
    },

    NotAStruct {
        type_name: ModulePath,
        found: TypeId,
    },

    StructNotFullyInitialized {
        type_name: ModulePath,
        struct_type: TypeId,
        missing_fields: Vec<Ident>,
    },

    UnknownField {
        name: Ident,
        struct_type: TypeId,
    },

    HeterogenousArray {
        expected: TypeId,
        found: TypeId,
        index: usize,
    },

    NotAnArray {
        found: TypeId,
    },

    InvalidIndex {
        found: TypeId,
    },
}

impl From<TypeErr> for Err {
    fn from(err: TypeErr) -> Err {
        Err::TypeErr(err)
    }
}
