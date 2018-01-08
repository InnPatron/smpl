use ast::{Path, Ident};

#[derive(Clone, Debug)]
pub enum Err {
    ControlFlowErr(ControlFlowErr),
    UnknownType(Path),
    UnknownVar(Ident),
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

}
